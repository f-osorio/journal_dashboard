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


library(hash)
hash_table <- hash()
hash_table[["A"]] <- 6
hash_table[["B"]] <- 5
hash_table[["C"]] <- 4
hash_table[["D"]] <- 3
hash_table[["E"]] <- 2
hash_table[["F"]] <- 1


replace_letter_grade <- function(value){
    out <- 0
    if (grepl('+', value, fixed=TRUE)){
        out <- out + .5
        value <- substr(value, 1, 1)
    }
    if (has.key(value, hash_table)){
        out <- out + hash_table[[value]]
    }

    return(out)
}


testing_spider_chart <- function(journals, type, show_average){
    spider_data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    data <- spider_data

    # Limit columns to those used
    keep <- c('journal_name.x', 'sjr','if_','cites','altmetric_score','instances', 'bwl', 'vwl')
    data <- subset(data, select = keep)

    for (i in 1:nrow(data)){
        data[i, 7] <- replace_letter_grade(data[i, 7])
        data[i, 8] <- replace_letter_grade(data[i, 8])
    }

    data$vwl <- as.numeric(as.character(data$vwl))
    data$bwl <- as.numeric(as.character(data$bwl))

    measures <- c('Impact Factor', 'SJR', 'Citations', 'Altmetric', 'Readers', 'BWL', 'VWL', 'Impact Factor')

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
    avg <- c(mean(data$if_), mean(data$sjr), mean(data$cites), mean(data$altmetric_score), mean(data$instances), mean(data$bwl), mean(data$vwl))
    print(avg)

    # make first column the index
    percentile <- percent[-1]
    row.names(percentile) <- percent$measure

    # Start Plot
    fig <- plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
    )

    order <- c('if_','sjr','cites','altmetric_score','instances', 'bwl', 'vwl')
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
        journal_data <- as.character(data[data$journal_name.x == journal, order])
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

testing_readers_by_discipline <- function(){
    status_sum <- mend_disc %>%
        group_by(category) %>%
            summarize(count=sum(count))

    fig <- plot_ly(status_sum,
                    x = ~category,
                    y = ~count,
                    type = 'bar'
    )

    fig <- fig %>% layout(
                            yaxis = list(title = 'Count'),
                            xaxis = list(title = 'Category')
                    )

    return(fig)

}
