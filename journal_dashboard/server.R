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
source("spider.R")
source("bubble.R")
source("horizontal.R")


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

    output$map_comp2 <- renderPlotly({
        mendeley_map_comp2(input$map_comp_select)
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

    comp_data <- merge(x=alt_simp, y=jd, by.x="print_issn", by.y="issn1")
    avialable_journals = unique(comp_data$journal_name.y)
    updatePickerInput(session, "journal_comp_1", choices=avialable_journals, selected=list(avialable_journals[1]))
    updatePickerInput(session, "journal_comp_2", choices=avialable_journals, selected=list(avialable_journals[2]))

    # Section Figures
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

    output$readers_by_discipline <- renderPlotly({
        testing_readers_by_discipline()
    })

    ##########################
    #       Spider_chart     #
    ##########################
    observe({
      spider_data <- spider_chart_data
      choosedReaderCategory <-input$Reader_category
      filtered_category_data <- spider_data %>% filter(spider_data$academic_status %in% choosedReaderCategory)
      updatePickerInput(session, "Discipline_category", choices=unique(filtered_category_data$discipline), selected = unique(filtered_category_data$discipline)[1])
    })

    observe({
      spider_data <- spider_chart_data
      choosedDiscipline <- input$Discipline_category
      choosedReaderCategory <-input$Reader_category
      filtered_category_data <- spider_data %>% filter(spider_data$academic_status %in% choosedReaderCategory)
      filtered_category_discipline_data <- filtered_category_data %>% filter(filtered_category_data$discipline %in% choosedDiscipline)
      updatePickerInput(session, "Jornals_for_spider_chart", choices=filtered_category_discipline_data$jornal_name, selected = filtered_category_discipline_data$jornal_name[1])
    })

    output$spider_chart <- renderPlotly({
      spider_chart(input$Jornals_for_spider_chart)
    })

    ##########################
    #       Bubble_chart     #
    ##########################
    observe({
      bubble_data <- tableau_data
      choosedReaderCategory <-input$Reader_category_bubble
      filtered_category_data <- bubble_data %>% filter(bubble_data$status %in% choosedReaderCategory)
      updatePickerInput(session, "Discipline_category_bubble", choices=unique(filtered_category_data$discipline), selected = unique(filtered_category_data$discipline))
    })

    observe({
      bubble_data <- tableau_data
      choosedDiscipline <- input$Discipline_category_bubble
      choosedReaderCategory <-input$Reader_category_bubble
      filtered_category_data <- bubble_data %>% filter(bubble_data$status %in% choosedReaderCategory)
      filtered_category_discipline_data <- filtered_category_data %>% filter(filtered_category_data$discipline %in% choosedDiscipline)
      updatePickerInput(session, "Jornals_for_bubble_chart", choices=filtered_category_discipline_data$name, selected = filtered_category_discipline_data$name)
    })

    output$bubble_chart <- renderPlotly({
      bubble_chart(input$Jornals_for_bubble_chart, input$show_journal_names)
    })

    ##########################
    #       Horizontal bar   #
    ##########################
    observe({
      bubble_data <- tableau_data
      choosedReaderCategory <-input$Reader_category_horizontal
      filtered_category_data <- bubble_data %>% filter(bubble_data$status %in% choosedReaderCategory)
      updatePickerInput(session, "Discipline_category_horizontal", choices=unique(filtered_category_data$discipline), selected = unique(filtered_category_data$discipline))
    })

    observe({
      bubble_data <- tableau_data
      choosedDiscipline <- input$Discipline_category_horizontal
      choosedReaderCategory <-input$Reader_category_horizontal
      filtered_category_data <- bubble_data %>% filter(bubble_data$status %in% choosedReaderCategory)
      filtered_category_discipline_data <- filtered_category_data %>% filter(filtered_category_data$discipline %in% choosedDiscipline)
      updatePickerInput(session, "Jornals_for_horizontal_bar", choices=filtered_category_discipline_data$name, selected = filtered_category_discipline_data$name)
    })

    output$horizontal_bar <- renderPlotly({
      horizontal_bar(input$Jornals_for_horizontal_bar)
    })




}
