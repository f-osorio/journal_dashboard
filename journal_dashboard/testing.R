library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)

library(rgeos)
library(rworldmap)

source("load_data.R")



testing_spider_chart <- function(journals){
    spider_data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    data <- spider_data
    measures <- c('Impact Factor', 'SJR', 'Altmetric', 'Readers', 'Citations', 'Impact Factor')
    fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
    )

    #bwl = c('C', 'A+'),
    #vwl = c('C', 'A+'),
    maxmin = data.frame(
                    if_ = c(0, 5),
                    sjr = c(1, 15),
                    Altmetric = c(500, 10000),
                    Readers = c(3000, 250000),
                    Citations = c(100, 2000)
                )

    for (journal in journals){
        journal_data <- as.character(data[data$journal_name.x == journal, c('sjr','if_','cites','altmetric_score','instances')])
        expanded <- c(journal_data, journal_data[1])
        fig <- fig %>%
            add_trace(
                r = expanded,
                theta = measures,
                name = journal
            )
    }

    fig <- fig %>%
    layout(
        polar = list(
            radialaxis = list(
                visible = T,
                range = maxmin,
                type="log"
            )
        )
    )

    return(fig)
}

replace_percent <- function(row, order, percentile){
    out = list()
    for (i in 1:length(row)){
        value <- ecdf(percentile[order[i], "min"]:percentile[order[i], "max"])(row[i])
        out[i] <- round(value, 4)
    }
    return(out)
}

testing_spider_chart_percentiles <- function(journals){
    spider_data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")

    data <- spider_data

    # Limit columns to those used
    keep <- c('journal_name.x', 'sjr','if_','cites','altmetric_score','instances')
    data <- subset(data, select = keep)

    #print(data)
    min <- min(data$if_)
    max <- max(data$if_)
    percentile <- ecdf(min:max)

    measures <- c('Impact Factor', 'SJR', 'Altmetric', 'Readers', 'Citations', 'Impact Factor')
    fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
    )

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
    # make first column the index
    percentile <- percent[-1]
    row.names(percentile) <- percent$measure

    #bwl = c('C', 'A+'),
    #vwl = c('C', 'A+'),
    maxmin = data.frame(
                    if_ = c(percentile["if_", "p_min"], 1),
                    sjr = c(percentile["sjr", "p_min"], 1),
                    Altmetric = c(percentile["altmetric_score", "p_min"], 1),
                    Readers = c(percentile["instances", "p_min"], 1),
                    Citations = c(percentile["cites", "p_min"], 1)
                )

    order <- c('sjr','if_','cites','altmetric_score','instances')
    for (journal in journals){
        journal_data <- as.character(data[data$journal_name.x == journal, c('sjr','if_','cites','altmetric_score','instances')])
        d <- replace_percent(journal_data, order, percentile)

        expanded <- c(d, d[1])
        fig <- fig %>%
            add_trace(
                r = expanded,
                theta = measures,
                name = journal
            )
    }

    fig <- fig %>%
    layout(
        polar = list(
            radialaxis = list(
                visible = T,
                range = maxmin
            )
        )
    )

    return(fig)
}


# Treemaps
# https://observablehq.com/@didoesdigital/2-june-2020-treemaps-dendrograms-sunbursts-circle-packing
testing_treemap_reader_status <- function(selected){
    data <- merge(x=mend_status, y=mend_doi, by.x="id_doi", by.y="id")

    keep <- c("publisher", "status", "count.x")
    data <- subset(data, select = keep)
    data <- data[data$publisher %in% selected, ]

    data <- data %>%
            group_by(publisher, status) %>%     # create the groups
            summarise(Value = sum(count.x))

    # Add rows for the journals to act as "parents"
    for (i in 1:length(selected)){
        s <- sum(data[data$publisher == selected[i], 3])
        pos = nrow(data)+1
        data[pos, 1] <- ""
        data[pos, 2] <- selected[i]
        data[pos, 3] <- s
    }

    data[['ids']] <- paste(data$status, data$publisher, sep="")

    fig <- plot_ly(
        type='treemap',
        labels=data$status,
        parents=data$publisher,
        values=data$Value,
        branchvalues="total",
        ids=data$ids,
        hovertemplate = paste("Journal: ", data$publisher, "<br>Status: ", data$status, "<br>Total: ", data$Value),
        pathbar=list(visible= TRUE)
    )
    fig <- fig %>% layout(
        grid=list(columns=3),
        margin=list(l=0, r=0, b=0, t=0))

    return(fig)
}


testing_journal_comp_chart <- function(journal_1, journal_2, categories){
    data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    data <- data[data$journal_name.y %in% list(journal_1, journal_2), ] # limit to selected journals

    # Replace NA with 0
    data[is.na(data)] <- 0

    # remove rows that have the same journal_name.y
    n_occur <- data.frame(table(data$journal_name.y))
    dupes <- data[data$journal_name.y %in% n_occur$Var1[n_occur$Freq > 1],]
    data <- data[ !data$journal_name.y %in% dupes$journal_name.y, ]

    keep <- c('journal_name.y', categories)
    data <- subset(data, select = keep)
    data <- setNames(data.frame(t(data)), data[,1])
    setDT(data, keep.rownames = "Sources")[]
    data = as.data.frame(data[-1,])

    fig <- plot_ly(data, type='bar')
    for(i in 2:ncol(data)){
        fig <- add_trace(fig, x = ~Sources, y = data[,i], name = colnames(data)[i])
    }
    fig <- fig %>% layout(
                            yaxis = list(title = 'Count', type = "log"),
                            barmode = 'group'
                    )

    return(fig)
}


testing_journal_comp_lollipop <- function(journal_1, journal_2, categories){
    # https://www.r-graph-gallery.com/303-lollipop-plot-with-2-values.html
    data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    data <- data[data$journal_name.y %in% list(journal_1, journal_2), ] # limit to selected journals
    data[is.na(data)] <- 0

    new_data <- data.frame(
        x = categories,
        value1 = as.numeric(unname(data[data$journal_name.y == journal_1, categories])),
        value2 = as.numeric(unname(data[data$journal_name.y == journal_2, categories])),
        journal1 = journal_1,
        journal2 = journal_2
    )

    fig <- ggplot(new_data) +
        geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
        geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
        geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
        coord_flip()+
        theme(
            legend.position = "top",
        ) +
        xlab("Categories") +
        ylab("Value") +
        scale_y_continuous(trans="log10")

    return(fig)
}
