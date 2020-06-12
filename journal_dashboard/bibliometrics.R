library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

source("load_data.R")


log_loc <- function(value){
    if (value == 0){
        return(0)
    } else {
        return(log10(value))
    }
}

bibliometrics_table <- function(target_journal){
    data <- jd[jd$journal_name == target_journal, ]
    fig <- HTML(paste("<table>
                    <tr>
                        <th>", data$journal_name,"</th>
                        <th></th>
                        <th></th>
                    </tr>
                    <tr>
                        <td>Citations</td>
                        <td>", data$cites,"</td>
                        <td>",
                            if (data$cites > top_10_per_cites_cutoff){
                                "<strong>Top 10%</strong>"
                            }
                        ,"</td>
                    </tr>
                    <tr>
                        <td>Impact Factor</td>
                        <td>", data$if_,"</td>
                        <td>",
                            if (data$if_ > top_10_per_if_cutoff){
                                "<strong>Top 10%</strong>"
                            }
                        ,"</td>
                    </tr>
                    <tr>
                        <td>5 Year Impact Factor</td>
                        <td>", data$if_5,"</td>
                        <td>",
                            if (data$if_5 > top_10_per_if_5_cutoff){
                                "<strong>Top 10%</strong>"
                            }
                        ,"</td>
                    </tr>
                    <tr>
                        <td>H Index</td>
                        <td>", data$h_index,"</td>
                        <td>",
                            if (data$h_index > top_10_per_hindex_cutoff){
                                "<strong>Top 10%</strong>"
                            }
                        ,"</td>
                    </tr>
                    <tr>
                        <td>SJR</td>
                        <td>", data$sjr,"</td>
                        <td>",
                            if (data$sjr > top_10_per_sjr_cutoff){
                                "<strong>Top 10%</strong>"
                            }
                        ,"</td>
                    </tr>
                    <tr>
                        <td>BWL</td>
                        <td>", data$bwl,"</td>
                        <td></td>
                    </tr>
                    <tr>
                        <td>VWL</td>
                        <td>", data$vwl,"</td>
                        <td></td>
                    </tr>
                    <tr>
                        <td>Journal Quality</td>
                        <td>", data$jourqual,"</td>
                        <td></td>
                    </tr>
                </table>
    "))

    return(fig)
}

bibliometrics_published_v_cited <- function(highlight){
    target <- jd[jd$journal_name == highlight, ]

    fig <- plot_ly(
        jd,
        type='scatter',
        mode='markers',
        marker=list(
                    color=~if_,
                    colorscale='Portland',
                    reversescale=F
                ),
        x = ~docs_published,
        y = ~cites,
        text = ~paste(
                journal_name,
                '<br>Publications:', docs_published,
                '<br>Citations:', cites,
                '<br>Impact Factor: ', if_,
                '<br>X: ', docs_published,
                '<br>Y: ', cites
        )
    )
    # need to be able to apply scaling to the annotation
    if (length(highlight) > 0){
        a <- list(
            x = log_loc(target$docs_published),
            y = log_loc(target$cites),
            text = target$journal_name,
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 7,
            ax = 20,
            ay = -40,
            font = list(color = '#264E86',
                        family = 'sans serif',
                        size = 22)
        )

        fig <- layout(fig,
            annotations = a,
            xaxis=list(type="log", title="Documented Published"),
            yaxis=list(type="log", title="Citations")
        )
    } else {
        fig <- layout(fig,
            xaxis=list(type="log", title="Documented Published"),
            yaxis=list(type="log", title="Citations")
        )
    }

    return(fig)
}

bibliometrics_hindx_comp <- function(comp_vector){
    # Group bar graph of H index and IF
    label <- comp_vector
    comp <- switch(comp_vector,
        "Impact Factor" = "if_",
        "5 Year IF" = "if_5",
        "Citations" = "cites",
        "Scimago SJR" = "sjr",
        "BWL Handelsblatt Ranking" = "bwl",
        "VWL Handelsblatt Ranking" = "vwl",
        "Journal Quality" = "jourqual"
    )

    fig <- plot_ly(
        jd,
        x = ~h_index,
        y = jd[[comp]],
        type='scatter',
        mode='markers',
        marker = list(
            size=10,
            color='rgba(225, 182, 193, .9)',
            line = list(color='rgba(152, 0, 0, .8)',
                width = 2)
        ),
        text = ~paste(
                journal_name,
                '<br>', label, ':', jd[[comp]]
        )
    )

    fig <- layout(fig,
        xaxis=list(title="H-Index"),
        yaxis=list(title=label)
    )

    return(fig)
}
