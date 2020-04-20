###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# McVernon et al 2020 ("The Doherty Model")

###################################################################################

# Load the mathJax library
withMathJax()

# Define the dashboard header
header <- dashboardHeader(title = "COVID-19 Open-source Infection Dynamics", titleWidth = 420,
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/',
                                         icon('github'), "Source Code", target="_blank"))
)


#######################################################
                    ### Sidebar ###
#######################################################

# Sidebar (icons from https://fontawesome.com/icons)
sidebar <- dashboardSidebar(
    dateRangeInput('dateRange',
                   label = span(tagList(icon('calendar-alt'), 'Date range')),
                   start = '2020-01-01', end = '2020-12-31'),
    sidebarMenu(
        menuItem("Doherty Model", tabName = "doherty", icon = icon("dashboard")),
        menuItem("Fitzgerald Model", tabName = "fitzgerald", icon = icon("dashboard"))
    ) # Closes sidebarMenu
) # closes Sideboard




#######################################################
                    ### Body ###
#######################################################

## Define the Body
body <- dashboardBody(

tags$head(tags$style(HTML('
.box {margin: 5px;}'
    ))),

    ### Doherty Model
    tabItems(
        tabItem(tabName = "doherty",

                tabsetPanel(

                    # The model
                    tabPanel(title = icon("project-diagram"),
                           fluidPage(
                             fluidRow(
                                 column(width=4, h3("Transmission model of COVID-19 infection"),
                                        helpText(HTML("The figure to the right represents the compartmental transition model presented by
                                                          <a href='https://www.doherty.edu.au/uploads/content_doc/McVernon_Modelling_COVID-19_07Apr1_with_appendix.pdf' target='_blank'>
                                                          Moss et al (2020)
                                                        </a>
                                                      used to model the impact of COVID-19 in Australia.
                                                      <br/><br/>
                                                      <strong>Nodes</strong> represent compartments, or groups of individuals in the population defined by their status
                                                      with respect to COVID-19.
                                                      <br/><br/>
                                                      <strong>Edges</strong> represent transition pathways between the compartments. The parameterisation of the model determines
                                                      the likelihood of transitioning from one compartment to another along these pathways.
                                                      <br/><br/>
                                                      <strong>Hover</strong> over a node or edge to view the underlying parameter.
                                                      <br/><br/>
                                                      <strong>Click</strong> on a node or edge to find out more information or modify the underlying parameters.
                                                      "
                                                      ))
                                 ),

                                 column(width=8,
                                        box(width=NULL, status = "success", solidHeader = FALSE,
                                            visNetworkOutput("network_d")
                                        )
                                 )
                             ),

                             fluidRow(
                                column(width=3,
                                        box(title=NULL, width=NULL, status = "info", solidHeader = FALSE,
                                            htmlOutput("summary1")
                                        )
                                 ),
                                column(width=3,
                                       box(width=NULL, status = "info", solidHeader = FALSE,
                                           htmlOutput("summary2")
                                       )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", solidHeader = FALSE,
                                           htmlOutput("summary3")
                                       )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", solidHeader = FALSE,
                                           htmlOutput("summary4"),
                                           )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", solidHeader = FALSE,
                                           htmlOutput("summary5"),
                                       )
                                )
                             )

                        )  # Closes fluidPage
                    ), # Closes tabPanel


                    # Results
                    tabPanel(title = icon("chart-bar"),
                             fluidRow(
                                 column(width=3,
                                        box(title = NULL, width=NULL,
                                            status = "success", solidHeader = FALSE,
                                            actionButton(inputId = "runMod", "Run Model",
                                                         icon = icon("paper-plane"),
                                                         width = '100%',
                                                         class = "btn-success"),

                                            hr(),
                                            sliderInput("ndays",
                                                        "Number of days to plot:",
                                                        min=0, step=1, max=365, value=365),
                                            br(),
                                            checkboxGroupInput("plotvars", "Compartments to include",
                                                               choices = list(
                                                                   "Susceptible" = "S",
                                                                   "Infected (total)" = "Itotal",
                                                                   "Managed (total)" = "Mtotal",
                                                                   "Hospitalised (total)" = "H",
                                                                   "Recovered (total)" = "Rtotal",
                                                                   "Quarantined (total)" = "Q",
                                                                   "Fatalities (total)" = "D"
                                                               ), selected=c("H", "D")
                                            ),
                                            hr(),
                                            radioButtons("scale", "Scale",
                                                         choices = list(
                                                             "Count",
                                                             "Percent",
                                                             "Per 100,000"),
                                                         inline = FALSE,
                                                         selected=c("Count")
                                            ),
                                            materialSwitch("logScale", "Log scale", FALSE, inline=TRUE, status = "info"),
                                            materialSwitch("cuml", "Cumulative", FALSE, inline=TRUE, status = "info")
                                        )
                                 ),

                                 column(width=9,
                                        box(title = tagList(shiny::icon("chart-area"), "Simulation results: Incidence over time"),
                                            width = "100%", status = "primary", solidHeader = FALSE,
                                            withLoader(ggiraphOutput("plot"), type="html", loader="loader5")
                                        ),
                                        box(width = "100%",
                                            helpText(HTML(paste(strong("Notes"), "Cumulative counts are sensible for hospitalised and fatalaities"))))
                                 )
                             )
                    ),

                    # Summary
                    tabPanel(title = icon("file-download"),
                             box(width=NULL, status = "info", solidHeader = FALSE,
                                 column(width=3,
                                        textInput("reportname", label=NULL, value="", placeholder = "my-report")
                                 ),
                                 column(width=3,
                                        prettyCheckbox(inputId = "datelab", "yyyy-mm-dd tag?", icon = icon("check"),
                                                       status = "default", shape = "curve", animation = "pulse", value = TRUE)
                                 ),
                                 column(width=3,
                                        radioButtons(
                                            inputId = "format", label = NULL,
                                            choices = c("HTML"),
                                            selected = "HTML")
                                 ),
                                 column(width=3,
                                        withLoader(downloadButton("downloadReport", "Download report",
                                                       icon = icon("file-download"),
                                                       width = '100%',
                                                       class = "btn-info"), type="html", loader="loader5")
                                 )
                             )
                    ),

                    # Data
                    tabPanel(title = icon("file-excel"),
                             DT::dataTableOutput("mod_df_wide"),
                             hr(),
                             downloadLink("get_wide_data", label = HTML(paste("Download the data", icon("download"))))
                    ),
                    # About
                    tabPanel(title = icon("info-circle")
                    )

                ) # Closes tabsetPanel



                ),


        ### Extended Model
        tabItem(tabName = "fitzgerald",
            h2("Extensions to the doherty model"),
            tabsetPanel(

                # Model
                tabPanel(title = icon("project-diagram"),
                         fluidPage(

                             fluidRow(
                                 column(width=4,
                                        HTML(paste("Under construction", icon("wrench")))
                                        ),

                                 column(width=8,
                                 )
                             ),

                             fluidRow(
                             )

                         )  # Closes fluidPage
                    ), # Closes tabPanel

                # Results
                tabPanel(title = icon("chart-bar")),

                # Summary
                tabPanel(title = icon("file-download")),

                # Data
                tabPanel(title = icon("file-excel")),

                # About
                tabPanel(title = icon("info-circle"))

            ) # Closes tabsetPanel
        ) # Closes tabItem

    ) # Closes tabItems
) # Closes dashboardBody


# Put them together into a dashboardPage
dashboardPage(
    header,
    sidebar,
    body
)

