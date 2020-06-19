# add shinyWidgets for better selection options https://stackoverflow.com/questions/50218614/shiny-selectinput-to-select-all-from-dropdown
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)

library(rgeos)
library(rworldmap)
library(rworldxtra)

source("helpers.R")
source("load_data.R")
source("altmetrics.R")
source("bibliometrics.R")
source("mendeley.R")
source("testing.R")


function(input, output, session){
    #####################
    #     Altmetrics    #
    #####################
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

    ######################
    #    Bibliometrics   #
    ######################
    # Selection Updates
    journal_list = sort(unique(jd$journal_name))[-1]
    updateSelectInput(session, "journ_summary", choices=journal_list, selected=journal_list[1])
    updatePickerInput(session, "pub_cites_pointer", choices=journal_list)

    # Section Figures
    output$journ_summary <- renderUI({
        bibliometrics_table(input$journ_summary)
    })

    output$pubVcite <- renderPlotly({
        bibliometrics_published_v_cited(input$pub_cites_pointer)
    })

    output$hIndexGroup <- renderPlotly({
        bibliometrics_hindx_comp(input$hindex_comp)
    })

    #####################
    #      Mendeley     #
    #####################
    available <- unique(mend_map_comp$publisher)
    updatePickerInput(session,
                      "map_comp_select",
                      choices=available,
                      selected=list(available[1], available[2]))

    output$map <- renderPlotly({
        mendeley_map_basic()
    })

    output$map_comp <- renderPlotly({
        mendeley_map_comp_circles(input$map_comp_select)
    })

    output$status <- renderPlotly({
        mendeley_reader_status()
    })

    #####################
    #       Testing     #
    #####################
    journal_list = unique(spider_data$journal_name)
    updatePickerInput(session, "spider_journals", choices=journal_list, selected=list(journal_list[2], journal_list[3]))

    data <- treemap_data
    journal_list = unique(data$journal_name)
    updatePickerInput(session, "treemap_readers_status_journals", choices=journal_list, selected=list(journal_list[1], journal_list[2]))


    comp_data <- journal_comp
    available_journals = unique(comp_data$journal_name)
    updatePickerInput(session, "journal_comp_1", choices=available_journals, selected=available_journals[1])
    updatePickerInput(session, "journal_comp_2", choices=available_journals, selected=available_journals[2])


    output$spider <- renderPlotly({
        testing_spider_chart(input$spider_journals, input$spider_output, input$spider_average)
    })

    output$treemap_readers_status <- renderPlotly({
        testing_treemap_reader_status(input$treemap_readers_status_journals)
    })

    output$journal_comp_chart <- renderPlotly({
        testing_journal_comp_chart(input$journal_comp_1, input$journal_comp_2, input$categories)
    })

    output$journal_comp_lollipop <- renderPlotly({
        testing_journal_comp_lollipop(input$journal_comp_1, input$journal_comp_2, input$categories)
    })
}
