library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

source("load_data.R")



##########################
#       Spider_chart     #
##########################

spider_chart <- function(journals){
  spider_data <- spider_chart_data 
  journal_list = unique(spider_data$jornal_name)
  measures <- c('SJR', 'IF', 'Cites', 'alt score', 'doi', 'Class.BWL', 'SJR')
  fig <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  )
  
  
  for (journal in journals){
    journal_data <- as.character(spider_data[spider_data$jornal_name == journal, c('SJR','IF','Cites','alt_score','doi', 'Class.BWL')])
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
          visible = T
        
        )
      )
    )
  
  return(fig)
}





