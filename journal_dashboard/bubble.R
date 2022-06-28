library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

source("load_data.R")

bubble_chart <- function(journals, show_journal_names){
  bubble_chart <- tableau_data
  filtered_name_data <- bubble_chart %>% filter(bubble_chart$name %in% journals)
  fig <- plot_ly(filtered_name_data, x = ~alt_score, y = ~IF, text = ~name, type = 'scatter', mode = 'markers', color = ~SJR, colors = 'Reds',
                 marker = list(size = ~SJR*10, opacity = 1))
  if (show_journal_names == 'True'){
    t <- list(
      family = "sans serif",
      size = 11,
      color = 'white')
    #fig <- fig %>% add_text(textfont = t, textposition = "middle right")

    fig <- fig %>%  add_annotations(
                  text = rownames(filtered_name_data$name),
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE,
                  xshift = 75,
                  yshift = 20,
                  opacity = .5,
                  bgcolor = 'black',
                  font = t)

    fig <- fig %>% style(hoverinfo = 'none')
  }
  fig <- fig %>% layout(title = 'Bubbel chart',
                        xaxis = list(showgrid = TRUE, range = c(0,30)),
                        yaxis = list(showgrid = TRUE, range = c(0,6)))
  return(fig)
}

