library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

source("load_data.R")

horizontal_bar <- function(journals){
  horizontal_bar <- tableau_data  
  filtered_name_data <- horizontal_bar %>% filter(horizontal_bar$name %in% journals)
  fig <- plot_ly(filtered_name_data, x = ~SJR, y = ~name, type = 'bar', orientation = 'h', color = ~IF )
  fig <- fig %>% layout(title = 'Bubbel chart',
                        xaxis = list(title = "SJR" ),
                        yaxis = list(title = "Journal name"))

  return(fig)
}

