library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

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
