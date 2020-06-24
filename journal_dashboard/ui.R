library(shiny)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

header <- dashboardHeader(
    title = "Journal Metrics"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Altmetrics", tabName = "altmetrics", icon = icon("hashtag")),
        menuItem("Bibliometrics", tabName = "biblio", icon = icon("book")),
        menuItem("Mendeley", tabName = "mendeley", icon = icon("chart-bar")),
        menuItem("Testing", tabName = "testing", icon = icon("vial")),
        menuItem("Spider Chart", tabName = "spider_chart", icon = icon("vial")),
        menuItem("Open R-Studio", href="/rstudio", icon = icon("r-project"))
    )
)

body <- dashboardBody(
    tags$script(HTML(
        "
        var url = window.location.href
        var target = document.getElementsByTagName('a')[6];
        if (url.includes('shiny/journal_dashboard')){
            var new_url = url.replace('shiny/journal_dashboard', 'rstudio')
            target.setAttribute('href', new_url);
        }
        "
    )),
    tabItems(
        tabItem(tabName = "altmetrics",
            fluidRow(
                h1("Altmetrics"),
                selectInput("altVar",
                    label = "Select A Value to Display",
                    choices = c('Maximum', 'Minimum', 'Mean', 'Median'),
                    selected = "Maximum"
                ),
                h2('Altmetric Score'),
                plotlyOutput('alt', height="550px"),
                br(),
                h2('Journal Engagement'),
                selectInput("journ",
                    label = "Select a Journal",
                    choices = c("The Review of Economic Studies", "The Quarterly Journal of Economics", "The Academy of Management Annals", "Strategic Management Journal", "Review of Economic Studies",
                                "Quarterly Journal of Economics", "Management Science", "Journal of the European Economic Association", "Journal of the American Economic Association", "Journal of Political Economy",
                                "Journal of Marketing Research (JMR)", "Journal of Marketing", "Journal of Labor Economics", "Journal of Health Economics", "Journal of Financial Economics",
                                "Journal of Finance", "Journal of Economic Theory", "Journal of Econometrics", "Journal of Consumer Research", "Journal of Business Research",
                                "Journal of Accounting Research", "Journal of Accounting & Economics", "Information Systems Research", "Games & Economic Behavior", "European Economic Review",
                                "Economic Journal", "American Economic Review", "Administrative Science Quarterly", "Academy of Management Review", "Academy of Management Journal"),
                    selected = "American Economic Review"
                ),
                pickerInput('sources', 'Sources', choices = list(
                                        "Mendeley" = "mendeley", "News" = "news",
                                        "Blog" = "blog", "Facebook" = "facebook", "Wikipedia" = "wikipedia", "Google" = "google",
                                        "Syllabi" = "syllabi", "Twitter" = "twitter", "Policy" = "policy", "Peer Review" = "peer_review",
                                        "Patent" = "patent", "Weibo" = "weibo", "linkedIn" = "linkedIn", "Reddit" = "reddit",
                                        "Pinterest" = "pinterest", "F1000" = "f1000", "QA" = "qa", "Videos" = "videos"
                                    ),
                                    selected = list("news", "blog", "google","policy", "peer_review"),
                                    options = list(`actions-box` = TRUE, `max-options` = 5),
                                    multiple = T
                ),
                plotlyOutput('pie'),
                br(),
                h2('Social Media Sources'),
                pickerInput('social_media_journals', 'Journals', choices = c("The Review of Economic Studies", "The Quarterly Journal of Economics", "The Academy of Management Annals", "Strategic Management Journal", "Review of Economic Studies",
                                "Quarterly Journal of Economics", "Management Science", "Journal of the European Economic Association", "Journal of the American Economic Association", "Journal of Political Economy",
                                "Journal of Marketing Research (JMR)", "Journal of Marketing", "Journal of Labor Economics", "Journal of Health Economics", "Journal of Financial Economics",
                                "Journal of Finance", "Journal of Economic Theory", "Journal of Econometrics", "Journal of Consumer Research", "Journal of Business Research",
                                "Journal of Accounting Research", "Journal of Accounting & Economics", "Information Systems Research", "Games & Economic Behavior", "European Economic Review",
                                "Economic Journal", "American Economic Review", "Administrative Science Quarterly", "Academy of Management Review", "Academy of Management Journal"),
                    selected = list("American Economic Review", "Strategic Management Journal", "The Academy of Management Annals"),
                    options = list(`actions-box` = TRUE),
                    multiple = T
                ),

                checkboxGroupButtons("social_media_types",
                                     "Social Media",
                                     choices = list('News'='news', 'Blog'='blog',
                                                    'Policy'='policy', 'Twitter'='twitter',
                                                    'Facebook'='facebook'),
                                     selected = list("news", "blog"),
                                     checkIcon = list(
                                        yes = tags$i(class = "fa fa-check-square",
                                        style = "color: steelblue"),
                                     no = tags$i(class = "fa fa-square-o",
                                        style = "color: steelblue"))
                ),
                plotlyOutput('social_bar_comp')
            )
        ),
        tabItem(tabName = "biblio",
            fluidRow(
                h1("Bibliometrics"),
                h2("Journal Summary"),
                selectInput("journ_summary",
                    label = "Select a Journal",
                    choices = c("None"),
                    selected="None"
                ),
                htmlOutput('journ_summary'),
                h2("Documents Published vs. Total Citations"),
                pickerInput('pub_cites_pointer', 'Select Journal to Highlight',
                            choices = c("None"),
                    options = list(`actions-box` = TRUE, `max-options` = 1),
                    multiple = T
                ),
                plotlyOutput('pubVcite'),

                h2("H-Index &"),
                selectInput("hindex_comp",
                    label = "Select a Metric",
                    choices = c(
                        "Impact Factor",
                        "5 Year IF",
                        "Citations",
                        "Scimago SJR",
                        "BWL Handelsblatt Ranking",
                        "VWL Handelsblatt Ranking",
                        "Journal Quality"
                        ),
                    selected="None"
                ),
                plotlyOutput('hIndexGroup')

            )
        ),
        tabItem(tabName = "mendeley",
            fluidRow(
                h1("Mendeley"),
                h2("Where?"),
                plotlyOutput('map'),
                br(),
                pickerInput('map_comp_select', 'Mendeley Readers For Selected Journals', choices = c("The Review of Economic Studies", "The Quarterly Journal of Economics", "The Academy of Management Annals", "Strategic Management Journal", "Review of Economic Studies",
                                "Quarterly Journal of Economics", "Management Science", "Journal of the European Economic Association", "Journal of the American Economic Association", "Journal of Political Economy",
                                "Journal of Marketing Research (JMR)", "Journal of Marketing", "Journal of Labor Economics", "Journal of Health Economics", "Journal of Financial Economics",
                                "Journal of Finance", "Journal of Economic Theory", "Journal of Econometrics", "Journal of Consumer Research", "Journal of Business Research",
                                "Journal of Accounting Research", "Journal of Accounting & Economics", "Information Systems Research", "Games & Economic Behavior", "European Economic Review",
                                "Economic Journal", "American Economic Review", "Administrative Science Quarterly", "Academy of Management Review", "Academy of Management Journal"),
                    selected = list("American Economic Review", "Strategic Management Journal", "The Academy of Management Annals"),
                    options = list(`actions-box` = TRUE),
                    multiple = T
                ),
                plotlyOutput('map_comp', height="300%"),
                br(),
                plotlyOutput('map_comp2'),
                h2("Who?"),
                plotlyOutput('status')
            )
        ),
        tabItem(tabName = "testing",
            fluidRow(
                h1("Testing"),
                h2("Spider Chart"),
                pickerInput("spider_journals",
                    label = "Select Journals",
                    choices = c("None"),
                    multiple = T,
                    options=list(`max-options` = 3)
                ),
                radioGroupButtons(
                    inputId = "spider_output", label = "Output Type:",
                    choices = c("Totals", "Percentiles"),
                    status = "primary"
                ),
                radioGroupButtons(
                    inputId = "spider_average", label = "Show Average:",
                    choices = c("True", "False"),
                    status = "primary"
                ),
                plotlyOutput('spider'),
                br(),
                h2('Hierarchical Data'),
                h3('Journal Reader Status (tree map)'),
                pickerInput("treemap_readers_status_journals",
                    label = "Select Journals",
                    choices = c("None"),
                    multiple = T
                ),
                plotlyOutput('treemap_readers_status'),
                br(),
                h2("Compare Journals"),
                pickerInput('journal_comp_1',
                            label = "Journal 1",
                            choices = c("None")
                ),
                pickerInput('journal_comp_2',
                            label = "Journal 2",
                            choices = c("None")
                ),
                pickerInput('categories',
                            label = "",
                            choices = list('Altmetric Score'='altmetric_score', 'Blog'='blog',
                                           'Policy'='policy', 'Twitter'='twitter',
                                           'Facebook'='facebook', 'Impact Factor'='if_',
                                           '5 Year IF'='if_5','H-Index'='h_index',
                                           'Publications'='docs_published','Citations'='cites',
                                           'Journal Quality'='jourqual', 'News'='news'),
                            selected = list("cites", "blog", "if_"),
                            options = list(`actions-box` = TRUE),
                            multiple = T
                ),
                plotlyOutput('journal_comp_chart'),
                br(),
                plotlyOutput('journal_comp_lollipop'),
                br(),
                h2('Readers by Discipline'),
                plotlyOutput('readers_by_discipline')
            )
        ),
        tabItem(tabName = "spider_chart",
                fluidRow(
                  h2("Spider Chart"),
                  pickerInput("Jornals_for_spider_chart",
                                     label = "Select Journals",
                                     choices = c("None"),
                                     multiple = T,
                                     options=list(`max-options` = 3)
                  ),
                  plotlyOutput('spider_chart')

                )
        )
    )
)


dashboardPage(
    header,
    sidebar,
    body
)
