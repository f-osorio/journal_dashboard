library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

source("load_data.R")


# https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly
vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )}

altmetrics_aggregate_barchart <- function(input){
    max <- aggregate(altmetric_score ~ journal_name, alt, max)
    min <- aggregate(altmetric_score ~ journal_name, alt, min)
    mean <- aggregate(altmetric_score ~ journal_name, alt, mean)
    median <- aggregate(altmetric_score ~ journal_name, alt, median)

    data <- switch(input,
        "Maximum" = max,
        "Minimum" = min,
        "Mean" = mean,
        "Median" = median
    )
    data[data == ''] <- NA # Set empty journal name to NA
    data <- na.omit(data)  # Remove NA

    # Get average for current selection
    avg <- mean(data[['altmetric_score']])

    sorted_data <- data[order(data$altmetric_score), ]

    fig <- plot_ly(data, x=~altmetric_score, y=~journal_name, orientation='h', type='bar', name="test")
    fig <- fig %>% layout(
        xaxis = list(title="Altmetric Score"),
        yaxis = list(title="Journals", tickfont=list(size=10), margin=list(pad=50),
            categoryorder = "array",
            categoryarray = sorted_data$journal_name),
        shapes = list(vline(avg)) # add a line to indicate average across journals
    )

    return(fig)
}

# Some notes on pie chartss
# https://observablehq.com/@didoesdigital/16-may-2020-donut-charts-and-pie-charts?collection=@didoesdigital/journal-getting-started-with-data-viz-collection
# https://www.data-to-viz.com/caveat/pie.html
altmetrics_pie <- function(sources, journal){
    summary <- setDT(alt)[, c(lapply(.SD[, c(10:27), with=FALSE], sum)), by=journal_name]
    sub <- data.frame(subset(summary, journal_name == journal))
    flipped <- as.data.frame(t(sub))
    flipped <- setDT(flipped, keep.rownames = TRUE)[]
    names(flipped)[1] <- 'key'
    names(flipped)[2] <- 'value'
    # remove first row which has a string >> "journal_name"
    flipped <- flipped[-1,]
    # make sure there are no strings
    flipped$values <- as.numeric(as.character(flipped$value))
    # limit to just the options selected for sources
    flipped <- flipped[flipped$key %in% sources, ]

    fig <- plot_ly(flipped, labels=~key, values = ~values, type='pie') %>%
        layout(title = journal,
                xaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                yaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))

    return(fig)
}


altmetrics_social_bar_comp <- function(journals, types){
    alt_simp <- alt_simp[alt_simp$journal_name %in% journals, ] # limit to selected journals

    keep <- c('journal_name', types)
    data <- subset(alt_simp, select = keep)
    data <- setNames(data.frame(t(data)), data[,1])
    setDT(data, keep.rownames = "Sources")[]
    data = as.data.frame(data[-1,])

    fig <- plot_ly(data, type='bar')
    for(i in 2:ncol(data)){
        fig <- add_trace(fig, x = ~Sources, y = data[,i], name = colnames(data)[i])
    }
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

    return(fig)
}
