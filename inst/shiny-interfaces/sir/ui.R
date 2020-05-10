###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# Extensions to the Doherty Model: Phased Interventions (pint)

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
    h3("COVOID | SIR Model"),
    sidebarMenu(
        menuItem("Parameterise model", tabName = "model", icon = icon("project-diagram")),
        menuItem("Define interventions", tabName = "intervention", icon = icon("user-md")),
        menuItem("View results", tabName = "results", icon = icon("chart-bar")),
        menuItem("Animate results", tabName = "video", icon = icon("video")),
        menuItem("Prepare report", tabName = "canvas", icon = icon("palette")),
        menuItem("Download data", tabName = "data", icon = icon("file-excel")),
        menuItem("Information", tabName = "info", icon = icon("info-circle"))
    ), # Closes sidebarMenu
    div(style = 'overflow-y:scroll;height:600px;',
        dateRangeInput('dateRange',
                       label = span(tagList(icon('calendar-alt'), 'Date range')),
                       start = '2020-03-01', end = '2020-09-30')
    ) # Closes div
) # closes Sideboard


#######################################################
                    ### Body ###
#######################################################

## Define the Body
body <- dashboardBody(



    ### Extended Doherty Model
    tabItems(
                    # The model
                    tabItem(tabName = "model",
                           fluidPage(
                               div(style="text-align:center",
                                   box(width = "100%", background = "purple",
                                   h4(HTML(paste(icon("project-diagram"), "Parameterise model"))))
                                ),

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
                                            visNetworkOutput("network_d", height = "180px", width = "100%")
                                        )
                                 )
                             ),

                             fluidRow(
                                column(width=3,
                                        box(title=NULL, width=NULL, height = "250px", status = "info", solidHeader = FALSE,
                                            htmlOutput("summary1")
                                        )
                                 ),
                                column(width=3,
                                       box(width=NULL, status = "info", height = '250px', solidHeader = FALSE,
                                           htmlOutput("summary2")
                                       )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", height = '250px', solidHeader = FALSE,
                                           htmlOutput("summary3")
                                       )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", height = '250px', solidHeader = FALSE,
                                           htmlOutput("summary4")
                                           )
                                ),
                                column(width=2,
                                       box(width=NULL, status = "info", height = '250px', solidHeader = FALSE,

                                       )
                                )
                             )

                        )  # Closes fluidPage
                    ), # Closes tabItem

                    # Interventions
                    tabItem(tabName = "intervention",
                             fluidPage(
                                 div(style="text-align:center",
                                     box(width = "100%", background = "purple",
                                         h4(HTML(paste(icon("user-md"), "Define interventions"))))
                                 ),
                                 box(width="100%", title = "Introduce an intervention. Choose the intervention setting(s):", status = "info", solidHeader = FALSE,
                                 checkboxGroupInput("intSetting", NULL, selected = "general", inline = TRUE,
                                                    choices=list("School" = "school", "Work" = "work", "Home" = "home")),
                                 helpText("Separate interventions can be simultaneously applied to contacts within school, work and home settings.
                                          Interventions for all remaining contacts can be controlled with the 'All contacts' box.")
                                 ),
                                 #textOutput("test"),

                                 div(style = 'overflow-y:scroll;height:600px;',

                                     # All
                                         box(width="100%", collapsible = TRUE, solidHeader = TRUE, title = HTML(paste(icon("users"), "All contacts")),
                                             helpText("If school, work, or home-based interventions are selected, those contacts will be excluded from the intervention specified here."),
                                             column(width = 2,
                                                    sparklineOutput("sparklineGeneral"),
                                                    hr(),
                                                    strong("Pointer value:"),
                                                    textOutput("tooltipGeneral"),
                                                    helpText("Click to add a point. Double-click to remove."),
                                                    hr(),
                                                    actionButton("resetGeneral", "Reset"),
                                                    actionButton("undoGeneral", "Undo")),
                                             column(width = 6,
                                                    plotOutput("intGeneral", height = 270,
                                                               click = "intGeneral_click",
                                                               dblclick = "intGeneral_dblclick",
                                                               hover = hoverOpts("intGeneral_hover", delay=100, delayType = "debounce")),
                                             ),

                                             column(width = 4,
                                                    DTOutput("reviewGeneral")
                                             )
                                         ), # Closes box

                                 # Schools
                                 conditionalPanel(

                                     condition = "input.intSetting.includes('school')",
                                         box(width="100%", collapsible = TRUE, solidHeader = TRUE, title = HTML(paste(icon("school"), "Contacts at school")),
                                             helpText("Apply a distinct intervention for school-based contacts"),
                                             column(width = 2,
                                                    sparklineOutput("sparklineSchool"),
                                                    hr(),
                                                    strong("Pointer value:"),
                                                    textOutput("tooltipSchool"),
                                                    helpText("Click to add a point. Double-click to remove."),
                                                    hr(),
                                                    actionButton("resetSchool", "Reset"),
                                                    actionButton("undoSchool", "Undo")),
                                             column(width = 6,
                                             plotOutput("intSchool", height = 270,
                                                        click = "intSchool_click",
                                                        dblclick = "intSchool_dblclick",
                                                        hover = hoverOpts("intSchool_hover", delay=100, delayType = "debounce")),
                                             ),

                                             column(width = 4,
                                                    DTOutput("reviewSchool")
                                                    )
                                     ) # Closes box
                                 ), # Closes Conditional panel

                                 # Work
                                 conditionalPanel(

                                     condition = "input.intSetting.includes('work')",
                                     box(width="100%", collapsible = TRUE, solidHeader = TRUE, title = HTML(paste(icon("building"), "Contacts at work")),
                                         helpText("Apply a distinct intervention for work-based contacts"),
                                         column(width = 2,
                                                sparklineOutput("sparklineWork"),
                                                hr(),
                                                strong("Pointer value:"),
                                                textOutput("tooltipWork"),
                                                helpText("Click to add a point. Double-click to remove."),
                                                hr(),
                                                actionButton("resetWork", "Reset"),
                                                actionButton("undoWork", "Undo")),
                                         column(width = 6,
                                                plotOutput("intWork", height = 270,
                                                           click = "intWork_click",
                                                           dblclick = "intWork_dblclick",
                                                           hover = hoverOpts("intWork_hover", delay=100, delayType = "debounce")),
                                         ),

                                         column(width = 4,
                                                DTOutput("reviewWork")
                                         )
                                     ) # Closes box
                                 ), # Closes Conditional panel

                                 # Home
                                 conditionalPanel(

                                     condition = "input.intSetting.includes('home')",
                                     box(width="100%", collapsible = TRUE, solidHeader = TRUE, title = HTML(paste(icon("home"), "Contacts at home")),
                                         helpText("Apply a distinct intervention for home-based contacts"),
                                         column(width = 2,
                                                sparklineOutput("sparklineHome"),
                                                hr(),
                                                strong("Pointer value:"),
                                                textOutput("tooltipHome"),
                                                helpText("Click to add a point. Double-click to remove."),
                                                hr(),
                                                actionButton("resetHome", "Reset"),
                                                actionButton("undoHome", "Undo")),
                                         column(width = 6,
                                                plotOutput("intHome", height = 270,
                                                           click = "intHome_click",
                                                           dblclick = "intHome_dblclick",
                                                           hover = hoverOpts("intHome_hover", delay=100, delayType = "debounce")),
                                         ),

                                         column(width = 4,
                                                DTOutput("reviewHome")
                                         )
                                     ) # Closes box
                                 ) # Closes Conditional panel

                                ) # Closes div
                             )  # Closes fluidPage


                    ), # Closes tabPanel

                    # Results
                    tabItem(tabName = "results",
                            div(style="text-align:center",
                                box(width = "100%", background = "purple",
                                    h4(HTML(paste(icon("chart-bar"), "View results"))))
                            ),
                             fluidRow(
                                 column(width=3,
                                        box(title = NULL, width=NULL, height="600px",
                                            status = "success", solidHeader = FALSE,
                                            actionButton(inputId = "runMod", "Run Model",
                                                         icon = icon("paper-plane"),
                                                         width = '100%',
                                                         class = "btn-success"),
                                            hr(),
                                            downloadButton('downloadPlot','Download Plot', icon=icon("download"), width="100%", class = "btn-info"),

                                            hr(),
                                            checkboxGroupInput("plotvars", "Compartments to include",
                                                               choices = list(
                                                                   "Susceptible" = "S",
                                                                   "Infectious" = "I",
                                                                   "Recovered" = "R"),
                                                               selected=c("S", "I", "R")
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
                                            hr(),
                                            sliderInput("ndays",
                                                        "Number of days to plot:",
                                                        min=0, step=1, max=365, value=100)
                                        )
                                 ),

                                 column(width=6,
                                        box(title = tagList(shiny::icon("chart-area"), "Simulation results: Incidence over time"),
                                            width = "100%", height="600px", status = "primary", solidHeader = FALSE,
                                            withLoader(ggiraphOutput("plot"), type="image", loader="SARS-CoV-2.gif")
                                        )
                                 ),

                                 column(width=3,
                                        box(title = NULL,
                                            width = "100%", height="600px", status = "primary", solidHeader = FALSE,
                                            actionButton("toreport", label="Add to my report canvas", icon("palette"), class = "btn-success"),
                                            helpText("Plots that are added to your report canvas can be reviewed and captioned later.
                                                     You can also download these files as part of a customisable report for easy sharing."),
                                            hr(),
                                            radioButtons("reportFigs", label = "What figure should this appear as?",
                                                         choices = list("Figure 1" = 1, "Figure 2" = 2, "Figure 3" = 3, "Figure 4" = 4, "Figure 5" = 5, "Figure 6" = 6),
                                                         selected = NULL, inline =  FALSE)
                                        )
                                 ),


                             )
                    ),


                    # Animate results
                    tabItem(tabName = "video",
                            div(style="text-align:center",
                                box(width = "100%", background = "purple",
                                    h4(HTML(paste(icon("video"), "Animate results"))))
                            ),

                             fluidRow(
                                 column(width=3,
                                        box(title = NULL, width="100%", height = "600px",
                                            status = "success", solidHeader = FALSE,
                                            checkboxGroupInput("plotvars_a", "Compartments to include",
                                                               choices = list(
                                                                   "Susceptible" = "S",
                                                                   "Infectious" = "I",
                                                                   "Recovered" = "R"),
                                                               selected=c("S", "I", "R")
                                            ),
                                            hr(),
                                            radioButtons("scale_a", "Scale",
                                                         choices = list(
                                                             "Count",
                                                             "Percent",
                                                             "Per 100,000"),
                                                         inline = FALSE,
                                                         selected=c("Count")
                                            ),
                                            materialSwitch("logScale_a", "Log scale", FALSE, inline=TRUE, status = "info"),
                                            hr(),
                                            sliderInput("ndays_a",
                                                        "Number of days to animate:",
                                                        min=0, step=1, max=365, value=365),
                                            hr(),
                                            helpText("Right-click to download the gif")
                                        )
                                 ),

                                 column(width=6,
                                        box(title = tagList(shiny::icon("chart-area"), "Simulation results: Incidence over time"),
                                            width = "100%", height = "600px", status = "primary", solidHeader = FALSE,
                                            withLoader(imageOutput("animation"), type="image", loader="SARS-CoV-2.gif")
                                        )
                                 ),

                                 column(width=3,
                                        box(title = NULL,
                                            width = "100%", height = "600px", status = "primary", solidHeader = FALSE,
                                            helpText("Here you can visualise disease transition over time. Design the plot you would
                                                     like to animate then click on Create Animation to view. If no preview appears below, make sure that you have run a model."),
                                            h4("Animation preview"),
                                            ggiraphOutput("animation_preview", width='100%', height='200px'),
                                            actionButton(inputId = "runAni", "Create Animation",
                                                         icon = icon("play"),
                                                         width = '100%',
                                                         class = "btn-success")
                                        )
                                        )
                             )

                    ),


                    # Dynamic report
                    tabItem(tabName = "canvas",
                            div(style="text-align:center",
                                box(width = "100%", background = "purple",
                                    h4(HTML(paste(icon("palette"), "Prepare report"))))
                            ),
                                 fluidRow(
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
                                                           class = "btn-info"), type="image", loader="SARS-CoV-2.gif")
                                     )
                                 ),

                                 ),

                             conditionalPanel("output.nPlots>=1",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot1", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p1title", "Add a title for this plot", width="100%", placeholder = "Figure 1"),
                                                         textAreaInput("p1comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot1", width='100%', height='300px')
                                                  )
                                              ),
                                              hr()
                                        ),
                             conditionalPanel("output.nPlots>=2",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot2", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p2title", "Add a title for this plot", width="100%", placeholder = "Figure 2"),
                                                         textAreaInput("p2comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot2", width='100%', height='300px')
                                                  )
                                              ),
                                              hr()
                                        ),
                             conditionalPanel("output.nPlots>=3",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot3", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p3title", "Add a title for this plot", width="100%", placeholder = "Figure 3"),
                                                         textAreaInput("p3comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot3", width='100%', height='300px')
                                                  )
                                              ),
                                              hr()
                                        ),
                             conditionalPanel("output.nPlots>=4",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot4", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p4title", "Add a title for this plot", width="100%", placeholder = "Figure 4"),
                                                         textAreaInput("p4comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot4", width='100%', height='300px')
                                                  )
                                              ),
                                              hr()
                                        ),
                             conditionalPanel("output.nPlots>=5",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot5", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p5title", "Add a title for this plot", width="100%", placeholder = "Figure 5"),
                                                         textAreaInput("p5comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot5", width='100%', height='300px')
                                                  )
                                              ),
                                              hr()
                                        ),

                             conditionalPanel("output.nPlots>=6",
                                              fluidRow(
                                                  column(width = 2,
                                                         materialSwitch("inclPlot6", "Include this plot in your downloadable report?", value = FALSE, status = "success"),
                                                  ),

                                                  column(width = 5,
                                                         textInput("p6title", "Add a title for this plot", width="100%", placeholder = "Figure 6"),
                                                         textAreaInput("p6comment", "Add a comment on this plot", width="400px", height="200px")
                                                  ),

                                                  column(width = 5,
                                                         ggiraphOutput("extplot6", width='100%', height='300px')
                                                  )
                                              ))
                    ),

                    # Data
                    tabItem(tabName = "data",
                            div(style="text-align:center",
                                box(width = "100%", background = "purple",
                                    h4(HTML(paste(icon("file-excel"), "Download data"))))
                            ),
                             helpText("Once you run a model you will be able to view and download the data here."),
                             DT::dataTableOutput("mod_df_wide"),
                             hr(),
                             conditionalPanel("output.mod_df_wide",
                                downloadLink("get_wide_data", label = HTML(paste("Download the data", icon("download"))))
                             )
                    ),
                    # About
                    tabItem(tabName = "info",
                            div(style="text-align:center",
                                box(width = "100%", background = "purple",
                                    h4(HTML(paste(icon("info-circle"), "Information"))))
                            ),
                             h3("Well head")
                    )

                ) # Closes tabsetPanel

) # Closes dashboardBody


# Put them together into a dashboardPage
dashboardPage(
    header,
    sidebar,
    body
)
