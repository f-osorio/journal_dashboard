library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

library(rgeos)
library(rworldmap)

source("load_data.R")




mendeley_map_basic <- function(){
    geo_sum <- mend_geo %>%
        group_by(country, code) %>%
            summarize(count=sum(count))

    df <- as.data.frame(geo_sum)
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)

    g <- list(
        scope = 'world',
        projection = list(type = 'Mercator'),
        showland=T,
        landcolor = toRGB("white"),
        countrycolor = "#c5c5c5",
        coastlinecolor = toRGB("grey90"),
        showcountries = T
    )

    fig <- plot_geo(df)
    fig <- fig %>% add_trace(
        z = ~count,
        color = ~count,
        colorscale = 'Heat',
        text = ~country,
        locations = ~code,
        marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Saves')
    fig <- fig %>% layout(
        title = 'Mendeley Distribution',
        geo = g
    )

    return(fig)
}

mendeley_map_comp <- function(selected){
    merged <- merge(x=mend_geo, y=mend_doi, by.x="id_doi", by.y="id")
    # Find center long/lat for countries
    wmap <- getMap(resolution="high")
    # get centroids
    centroids <- gCentroid(wmap, byid=TRUE)  # rgeos
    # get a data.frame with centroids
    coords <- as.data.frame(centroids)
    data <- merged
    # Add center locations to data
    data[,'x'] <- NA
    data[,'y'] <- NA

    replacements <- c("Bahamas","Macao","Republic of Singapore","Serbia and Montenegro","United States","Hong Kong","Tanzania")
    exceptions <- c("The Bahamas","Macau S.A.R","Singapore","Montenegro","United States of America","Hong Kong S.A.R.","United Republic of Tanzania")

    for (i in 1:length(rownames(coords))){
        name <- rownames(coords[i, ])
        x <- coords[i, "x"]
        y <- coords[i, "y"]
        if (name %in% exceptions){
            i <- match(name, exceptions)
            data_name <- replacements[i]
        } else {
            data_name <- name
        }
        data[data$country == data_name, "x"] <- x
        data[data$country == data_name, "y"] <- y
    }
    keep <- c("publisher", "x", "y", "count.x", "country", "code")
    data <- subset(data, select = keep)

    data <- data[data$publisher %in% selected, ]
    # aggregate data for each journal, country
    data <- data %>%
            group_by(publisher, country, x, y) %>%     # create the groups
            summarise(Value = sum(count.x))

    # order data by Value so smaller circles appear ontop of larger ones
    data <- data[with(data, order(-Value)), ]

    g <- list(
        scope = 'world',
        projection = list(type = 'albers'),
        showland=T,
        landcolor = toRGB("white"),
        countrycolor = "#c5c5c5",
        coastlinecolor = toRGB("grey90"),
        showcountries = T
    )

    fig <- plot_geo(data, sizes = c(1, 2500) )

    # scale the marker sizes, "sizeref=sizeref"
    #sizeref <- 2.0 * max(data$Value) / (80**2)

    fig <- fig %>% add_markers(
        x = ~x, y = ~y, size=~Value,
        hoverinfo="text",
        hovertext=paste("Country: ", data$country,
                        "<br>Journal: ", data$publisher,
                        "<br>Readers: ", data$Value),
        marker=list(
            color=~Value,
            colorscale='Heat',
            colorbar=list(
                title='Colorbar'
            )
        )
    )
    fig <- fig %>% layout(title='Most Readers for Selected Journals', geo=g, autosize=T)

    #return(hide_colorbar(fig))
    return(fig)
}

mendeley_reader_status <- function(){
    status_sum <- mend_status %>%
        group_by(status) %>%
            summarize(count=sum(count))

    fig <- plot_ly(status_sum,
                    x = ~status,
                    y = ~count,
                    type = 'bar'
    )

    return(fig)
}

mendeley_map_comp2 <- function(selected){
    data <- merge(x=mend_geo, y=mend_doi, by.x="id_doi", by.y="id")

    # Combine rows with count for each journal/country combo
    geo_sum <- data %>%
        group_by(country, code, publisher) %>%
            summarize(count=sum(count.x))
    data <- geo_sum

    # Get top 10 for each journal
    datalist = list()
    start <- 1
    stop <- length(selected)
    for (i in start:stop){
        journal_row <- data[which(data$publisher == selected[i]), ]
        top10 <- top_n(ungroup(journal_row), 10, count)
        datalist[[i]] <- top10
    }
    big_data <- do.call(rbind, datalist)

    df <- big_data %>%
        group_by(country, code) %>%
            filter(count == max(count)) # Limit a country to only its max

    df2 <- aggregate(count~., data = big_data, max)

    # Get top 1 for each journal
    datalist = list()
    start <- 1
    stop <- length(selected)
    for (i in start:stop){
        journal_row <- df[which(df$publisher == selected[i]), ]
        top1 <- top_n(ungroup(journal_row), 1, count)
        datalist[[i]] <- top1
    }
    big_data <- do.call(rbind, datalist)

    curr_countries <- big_data$country
    all_countries <- data$country

    start <- 1
    stop <- nrow(big_data)
    for (i in start:stop){
        row <- big_data[i, ]
        target <- row$publisher
        country <- row$country

        # if country is the highest listed country for target journal, do nothing
        # otherwise update row in big_data to have the highest from df2

        # max journal for country in df2
        #temp <- top_n(df2[df2$journal_name == target, ], 1, count)
        temp <- df2[df2$publisher == target, ]
        max <- big_data[i, ]$count
        for (j in 1:nrow(temp)){
            print(temp[j,]$country)
            print(temp[j,]$count)
            if (country != temp[j, ]$country && !(temp[j, ]$country %in% curr_countries) && !is.na(big_data[i, ]$country)){
                temp_count <- temp[j, ]$count
                if ( (temp_count > max) && !(temp[j, ]$country %in% big_data$country) ){
                    big_data[i, ]$code <- temp[j, ]$code
                    big_data[i, ]$country <- temp[j, ]$country
                    big_data[i, ]$publisher <- temp[j, ]$publisher
                    big_data[i, ]$count <- temp[j, ]$count
                    curr_countries[[length(curr_countries)]] <- temp[j, ]$country
                    max <- temp_count
                }
            }
        }
    }

    df <- big_data

    g <- list(
        scope = 'world',
        projection = list(type = 'albers'),
        showland=T,
        landcolor = toRGB("white"),
        countrycolor = "#c5c5c5",
        coastlinecolor = toRGB("grey90"),
        showcountries = T
    )
    fig <- plot_ly(df, z = df$count, type = 'choropleth', locations = df$code, showscale=F, text=df$publisher,
                    hoverinfo="text",
                    hovertext=paste("Country: ", df$country,
                                    "<br>Journal: ", df$publisher,
                                    "<br>Readers: ", df$count)
                    )

    fig <- fig %>%
            layout(geo = g)

    return(fig)
}
