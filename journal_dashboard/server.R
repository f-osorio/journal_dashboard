library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

library(rgeos)
library(rworldmap)

source("load_data.R")
source("altmetrics.R")
source("bibliometrics.R")
source("mendeley.R")
source("testing.R")


function(input, output, session){
    ##############
    # Altmetrics #
    ##############
    # Selection Updates
    journals = unique(alt$journal_name)
    updateSelectInput(session, "social_media_journals", choices=journals, selected=list(journals[1], journals[2]))

    # Section Figures
    output$alt <- renderPlotly({
        altmetrics_aggregate_barchart(input$altVar)
    })

    output$pie <- renderPlotly({
        altmetrics_pie(input$sources, input$journ)
    })

    output$social_bar_comp <- renderPlotly({
        altmetrics_social_bar_comp(input$social_media_journals, input$social_media_types)
    })

    #####################
    #    Journal Data   #
    #####################
    # Selection Updates
    journal_list = sort(unique(jd$journal_name))[-1]
    updateSelectInput(session, "journ_summary", choices=journal_list, selected=journal_list[1])

    # Section Figures
    output$journ_summary <- renderUI({
        bibliometrics_table(input$journ_summary)
    })

    output$pubVcite <- renderPlotly({
        bibliometrics_published_v_cited()
    })

    output$hIndexGroup <- renderPlotly({
        bibliometrics_hindx_comp(input$hindex_comp)
    })

    ############
    # Mendeley #
    ############
    # Selection Updates
    merged <- merge(x=mend_geo, y=mend_doi, by.x="id_doi", by.y="id")
    available <- unique(merged$publisher)
    updatePickerInput(session, "map_comp_select", choices=available, selected=list(available[1], available[2]))

    # Section Figures
    output$map <- renderPlotly({
        mendeley_map_basic()
    })

    output$map_comp <- renderPlotly({
        mendeley_map_comp(input$map_comp_select)
    })

    output$status <- renderPlotly({
        mendeley_reader_status()
    })

    ##########################
    #       Testing          #
    ##########################
    # Selection Updates
    spider_data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    journal_list = unique(spider_data$journal_name.x)
    updatePickerInput(session, "spider_journals", choices=journal_list, selected=list(journal_list[2], journal_list[3]))

    merged <- merge(x=mend_status, y=mend_doi, by.x="id_doi", by.y="id")
    available <- unique(merged$publisher)
    updatePickerInput(session, "treemap_readers_status_journals", choices=available, selected=list(available[1], available[2]))


    # Section Figures
    output$spider <- renderPlotly({
        testing_spider_chart(input$spider_journals)
    })

    output$treemap_readers_status <- renderPlotly({
        testing_treemap_reader_status(input$treemap_readers_status_journals)
    })

}
