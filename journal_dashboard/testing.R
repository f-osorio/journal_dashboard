library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)

source("load_data.R")


replace_with_percent <- function(row, order, percentile){
    out = list()
    for (i in 1:length(row)){
        value <- ecdf(percentile[order[i], "min"]:percentile[order[i], "max"])(row[i])
        out[i] <- round(value, 4)
    }
    return(out)
}

testing_spider_chart <- function(journals, type, show_average){
    data <- spider_data
    # Limit columns to those used
    keep <- c('journal_name', 'sjr','if_','cites','altmetric_score','mendeley')
    data <- subset(data, select = keep)

    measures <- c('Impact Factor', 'SJR', 'Altmetric', 'Readers', 'Citations', 'Impact Factor')

    percent <- data.frame(measure = character(0), min=numeric(0), max=numeric(0), p_min=numeric(0), p_max=numeric(0), stringsAsFactors=FALSE)
    for (i in 2:ncol(data)){
        min <- min(data[, i])
        max <- max(data[, i])
        row <- nrow(percent)
        percentile <- ecdf(min:max)
        p_min <- percentile(min)
        p_max <- percentile(max)
        percent[row + 1, 1] = colnames(data[i])
        percent[row + 1, 2] = min
        percent[row + 1, 3] = max
        percent[row + 1, 4] = signif(p_min, 4)
        percent[row + 1, 5] = p_max
    }

    # Get the averages
    avg <- c(mean(data$sjr), mean(data$if_), mean(data$cites), mean(data$altmetric_score), mean(data$mendeley))

    # make first column the index
    percentile <- percent[-1]
    row.names(percentile) <- percent$measure

    # Start Plot
    fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
    )

    order <- c('sjr','if_','cites','altmetric_score','mendeley')
    if (show_average == 'True'){
        if (type == 'Totals'){
            avg <- c(avg, avg[1])
        } else {
            avg <- replace_with_percent(avg, order, percentile)
            avg <- c(avg, avg[1])
        }
        fig <- fig %>%
            add_trace(
                r = avg,
                theta = measures,
                name = "Average",
                fillcolor = 'rgba(177, 177, 177, .5)',
                marker = list(
                    color = '#808080'
                ),
                line = list(
                    color = '#808080'
                )
            )
    }

    for (journal in journals){
        journal_data <- as.character(data[data$journal_name == journal, order])
        if (type == 'Totals'){
            expanded <- c(journal_data, journal_data[1])
        } else {
            d <- replace_with_percent(journal_data, order, percentile)
            expanded <- c(d, d[1])
        }
        fig <- fig %>%
            add_trace(
                r = expanded,
                theta = measures,
                name = journal
            )
    }


     if (type == 'Totals'){
         scale_type = "log"
         title = "Totals"
     } else {
         scale_type = ""
         title = "Percentiles"
     }

    fig <- fig %>%
    layout(
        polar = list(
            radialaxis = list(
                visible = T,
                type=scale_type
            )
        ),
        title=title
    )

    return(fig)
}


# Treemaps
# https://observablehq.com/@didoesdigital/2-june-2020-treemaps-dendrograms-sunbursts-circle-packing
testing_treemap_reader_status <- function(selected){
    data <- treemap_data
    data <- data[data$journal_name %in% selected, ]
    data[,3] <- as.integer(data[,3])  # cast from int64

    # Add rows for the journals to act as "parents"
    for (i in 1:length(selected)){
        s <- sum(data[data$journal_name == selected[i], 3])
        pos = nrow(data)+1
        data[pos, 1] <- ""
        data[pos, 2] <- selected[i]
        data[pos, 3] <- s
    }

    data[['ids']] <- paste(data$journal_name, data$status, sep="")

    labels <- data$status
    parents <- data$journal_name
    values <- data$total
    ids <- data$ids

    fig <- plot_ly(
        type='treemap',
        labels=labels,
        parents=parents,
        values=values,
        branchvalues="total",
        ids=ids,
        hovertemplate = paste("Journal: ", data$journal_name, "<br>Status: ", data$status, "<br>Total: ", data$total),
        pathbar=list(visible= TRUE)
    )

    fig <- fig %>% layout(
        grid=list(columns=3),
        margin=list(l=0, r=0, b=0, t=0)
    )

    return(fig)
}

