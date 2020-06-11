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

    new_data <- data.frame(
        x = categories,
        journal1 = as.numeric(unname(data[data$journal_name.y == journal_1, categories])),
        journal2 = as.numeric(unname(data[data$journal_name.y == journal_2, categories]))
    )

    fig <- ggplot(new_data) +
        geom_segment( aes(x=x, xend=x, y=journal1, yend=journal2), color="grey") +
        geom_point( aes(x=x, y=journal1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
        geom_point( aes(x=x, y=journal2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
        coord_flip()+
        theme(
            legend.position = "top",
        ) +
        xlab("Categories") +
        ylab("Value") +
        scale_y_continuous(trans="log10")

    return(fig)
}
