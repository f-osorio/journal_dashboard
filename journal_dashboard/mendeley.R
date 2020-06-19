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
    data <- mend_map_comp

    datalist = list()
    start <- 1
    stop <- length(selected)
    for (i in start:stop){
        journal_row <- data[which(data$journal_name == selected[i]), ]
        top10 <- top_n(ungroup(journal_row), 10, count)
        datalist[[i]] <- top10
    }
    big_data <- do.call(rbind, datalist)

    df <- big_data %>%
        group_by(country, code) %>%
            filter(count == max(count)) # Limit a country to only its max

    df2 <- aggregate(count~., data = big_data, max)

    datalist = list()
    start <- 1
    stop <- length(selected)
    for (i in start:stop){
        journal_row <- df[which(df$journal_name == selected[i]), ]
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
        target <- row$journal_name
        country <- row$country

        # if country is the highest listed country for target journal, do nothing
        # otherwise update row in big_data to have the highest from df2

        # max journal for country in df2
        #temp <- top_n(df2[df2$journal_name == target, ], 1, count)
        temp <- df2[df2$journal_name == target, ]
        max <- big_data[i, ]$count
        for (j in 1:nrow(temp)){
            if (country != temp[j, ]$country && !(temp[j, ]$country %in% curr_countries) && !is.na(big_data[i, ]$country)){
                temp_count <- temp[j, ]$count
                if ( temp_count > max ){
                    big_data[i, ]$code <- temp[j, ]$code
                    big_data[i, ]$country <- temp[j, ]$country
                    big_data[i, ]$journal_name <- temp[j, ]$journal_name
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
        landcolor = toRGB("white")
    )
    fig <- plot_ly(df, z = df$count, type = 'choropleth', locations = df$code, showscale=F, text=df$journal_name,
                    hoverinfo="text",
                    hovertext=paste("Country: ", df$country,
                                    "<br>Journal: ", df$journal_name,
                                    "<br>Readers: ", df$count)
                    )

    fig <- fig %>%
            layout(geo = g)

    return(fig)
}

mendeley_map_comp_circles <- function(selected){
    # Find center long/lat for countries
    wmap <- getMap(resolution="high")
    # get centroids
    centroids <- gCentroid(wmap, byid=TRUE)
    # get a data.frame with centroids
    coords <- as.data.frame(centroids)

    data <- mend_map_comp

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

    data <- data[data$publisher %in% selected, ]
    # aggregate data for each journal, country
    data <- data %>%
        group_by(publisher, country, x, y) %>%     # create the groups
        summarise(Value = as.integer(sum(count)))

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
    fig <- fig %>% add_markers(
        x=~x,
        y=~y,
        size=~Value,
        color=~Value,
        hoverinfo="text",
        hovertext=paste("Country: ", data$country,
                        "<br>Journal: ", data$publisher,
                        "<br>Readers: ", data$Value)
    )

    fig <- fig %>% layout(geo=g, autosize=T)

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

