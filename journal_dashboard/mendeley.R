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
