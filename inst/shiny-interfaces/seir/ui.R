###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# Extensions to the Doherty Model: Phased Interventions (pint)

###################################################################################

# Load the mathJax library
withMathJax()

# Define the dashboard header
# title = "COVID-19 Open-source Infection Dynamics"
# title = tags$div(tags$a(href='https://cbdrh.med.unsw.edu.au/postgraduate-coursework', tags$img(src='unsw_logo_reverse.png',height=40)), "COVID-19 Open-source Infection Dynamics"
header <- dashboardHeader(title = paste("COVID-19 Open-source Infection Dynamics"),
                          titleWidth = 400,
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/covoid', icon('github'), "Source Code", target="_blank")),
                          tags$li(class="dropdown",
                                  tags$a(href='https://twitter.com/cbdrh1',
                                         icon('twitter'), "Twitter", target="_blank"))
                          )


#######################################################
                    ### Sidebar ###
#######################################################

# Sidebar (icons from https://fontawesome.com/icons)
sidebar <- dashboardSidebar(

    div(style="text-align:center",
    br(),
    img(src = 'covoid-hexsticker.png', width="120px"),
    hr(),
    h3("COVOID | SEIR Model"),
    hr(),
    ),

    sidebarMenu(
        menuItem("Introduction", tabName = "introduction", icon = shiny::icon("home")),
        menuItem("Parameterise model", tabName = "model", icon = shiny::icon("tachometer")),
        menuItem("Define interventions", tabName = "intervention", icon = shiny::icon("user-md")),
        menuItem("Run simulation", tabName = "results", icon = shiny::icon("paper-plane")),
        menuItem("Report canvas", tabName = "canvas", icon = shiny::icon("palette")),
        menuItem("Animation", tabName = "video", icon = shiny::icon("video")),
        menuItem("Data", tabName = "data", icon = shiny::icon("file-excel"))

    ) # Closes sidebarMenu

) # closes Sideboard


#######################################################
                    ### Body ###
#######################################################

## Define the Body
body <- dashboardBody(
    # customise CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    # Tailor box colour to UNSW branding
    tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#ffdc00;
                    background:#ffdc00
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#000000;
                    border-left-color:#000000;
                    border-right-color:#000000;
                    border-top-color:#000000;
                    background:#ffdc00
                    }

                    ")),

    ### SEIR Model
    tabItems(
                    # The model
                    tabItem(tabName = "model",
                           fluidPage(
                               div(style="text-align:center",
                                   box(width = "100%", status = "primary", solidHeader = TRUE,
                                       h4(HTML(paste(icon("tachometer"), "Parameterise model"))))
                               ),

                               column(width = 3,
                                    box(width="100%", height = "850px", solidHeader = FALSE,
                                        div(style="text-align:center",
                                            h4(tagList(icon('tachometer'), 'Simulation settings'))
                                        ),
                                        hr(),

                                        selectInput("countryChoice",
                                                       label = span(tagList(icon('globe-asia'), 'Location')),
                                                       choices = ctryList,
                                                       selected = "Italy",
                                                       multiple = FALSE),

                                        numericInput("s_num",
                                                     label = span(tagList(icon('user'), 'Initial number susceptible')),
                                                     min=0, value = ""),

                                        span(tagList(icon('chart-bar'), 'Age distribution')),
                                        div(style="text-align:center",
                                            ggiraphOutput("ageHist", width = "250px", height = "250px")
                                        ),

                                        span(tagList(icon('handshake'), 'Social contacts by age band')),
                                        div(style="text-align:center",
                                            plotlyOutput("heatMap", width = "230px", height = "230px", inline = TRUE)
                                        ),
                                        hr(),
                                        dateRangeInput('dateRange',
                                                       label = span(tagList(icon('calendar-alt'), 'Date range for simulation')),
                                                       start = '2020-01-01', end = '2020-12-31'
                                        )
                                    )
                                ),

                               column(width = 3,
                                      box(width="100%", height = "850px", solidHeader = FALSE,
                                          div(style="text-align:center",
                                              h4(tagList(icon('user-friends'), 'Initial conditions'))
                                          ),
                                          hr(),
                                          numericInput("e_num",
                                                       label = span(tagList(icon('user'), 'Initial number exposed')),
                                                       min=0, value = 1000),
                                          br(),
                                          radioButtons("e_num_dist",
                                                       label = span(tagList(icon('chart-bar'), "Age distribution of exposed cases")),
                                                       choices = c("Match national age distribution"="Uniform", "Custom"), selected = "Uniform", inline = FALSE),
                                          sparklineOutput("sparklineAge_e"),
                                          hr(),

                                          numericInput("i_num",
                                                       label = span(tagList(icon('user'), 'Initial number infectious')),
                                                       min=0, value = 200),
                                          br(),
                                          radioButtons("i_num_dist",
                                                       label = span(tagList(icon('chart-bar'), "Age distribution of infectious cases")),
                                                       choices = c("Match national age distribution"="Uniform", "Custom"), selected = "Uniform", inline = FALSE),
                                          sparklineOutput("sparklineAge_i"),
                                          hr(),

                                          numericInput("r_num",
                                                       label = span(tagList(icon('user'), 'Initial number recovered')),
                                                       min=0, value = 0),
                                          br(),
                                          radioButtons("r_num_dist",
                                                       label = span(tagList(icon('chart-bar'), "Age distribution of recovered cases")),
                                                       choices = c("Match national age distribution"="Uniform", "Custom"), selected = "Uniform", inline = FALSE),
                                          sparklineOutput("sparklineAge_r")
                                      )
                               ),

                               column(width = 6,
                                          box(width="100%", height = "400px", solidHeader = FALSE,
                                              div(style="text-align:center",
                                                  h4(tagList(icon('project-diagram'), 'SEIR model parameters'))
                                              ),
                                              hr(),
                                              fluidRow(
                                                  column(width=6,
                                                      sliderInput("r0",
                                                                  label = HTML(paste(shiny::icon('sliders-h'), "The initial unmitigated reproduction number (R0)")),
                                                                  min=0, max=4, step = .1, value = 2.5),
                                                      sliderInput("sigmainv",
                                                                  label = HTML(paste0(icon('sliders-h'), " Duration of the latent period in days (1/", "&sigma;", ")")),
                                                                  min=1, max=14, step=.5, value=5.5),

                                                      sliderInput("gammainv",
                                                                  label = HTML(paste0(icon('sliders-h'), " Duration of infectious period in days (1/", "&gamma;", ")")),
                                                                  min=1, max=21, step=.5, value=10)
                                              ),

                                              column(width = 6,
                                                     visNetworkOutput("network_d", height = "180px", width = "100%")
                                                     )
                                              )

                                          )
                                      ),

                               column(width = 6,
                                      box(width="100%", height = "430px", solidHeader = FALSE,
                                          div(style="text-align:center",
                                              h4(tagList(icon('user-md'), 'Interventions overview')
                                                 )
                                          ),
                                          hr(),
                                          fluidRow(
                                              column(width = 5,

                                                     HTML(paste("The", tags$code("covoid"), "package allows you to implement two different types of intervention:
                                                     (i) interventions that target the number of social contacts between individuals; and (ii) interventions that
                                                     target the probability of transmission at each contact. Both types of interventions can be time-fixed or
                                                     time-varying. Interventions are specified in relative terms (i.e. relative to the pre-pandemic levels).")),
                                                     br(), br(),
                                                     HTML(paste("Use the", shiny::icon("user-md"), tags$strong("Interventions"), "panel on the right hand side
                                                                to specify interventions."))

                                                     ),
                                              column(width = 7,
                                                     ggiraphOutput("intSummary")
                                                     )

                                          )
                                      )
                               )


                           )  # Closes fluidPage
                    ), # Closes tabItem

                    # Interventions
                    tabItem(tabName = "intervention",
                            fluidPage(
                                div(style="text-align:center",
                                    box(width = "100%", status = "primary", solidHeader = TRUE,
                                        h4(HTML(paste(icon("user-md"), "Define interventions"))))
                                ),

                                column(width = 4,
                                       box(width="100%", height = "820px", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('user-md'), 'About interventions')))
                                           ),
                                           hr(),

                                           HTML(paste("The", tags$code("covoid"), "package allows you to implement two different types of intervention:", br(),
                                                      "(i) interventions that target the probability of transmission;", br(), "(ii) interventions that target the
                                                    number of contacts between individuals.", br(), br(),
                                                    "Both types of interventions can be time-fixed or time-varying.
                                                    To specify an intervention, use the panels to the right. All interventions are specified in relative terms
                                                    (i.e. relative to the pre-pandemic levels).")),
                                           br(), br(),

                                           HTML(paste(icon("handshake"), strong("Transmission probability"))),
                                           p("These interventions mimic the effect of measures that target the probability of transmitting the virus between
                                           infectious and susceptible individuals, such as wearing faces masks and hand-washing."),

                                           HTML(paste(icon("users"), strong("Social contacts"))),
                                           p("These interventions mimic the effect of measures that target the number of face-to-face contacts
                                           between individuals in the population, such as quarantine, school closures, working from home and
                                           other 'lockdown' measures."),


                                           hr(),

                                           helpText("By default, interventions targetting social contacts are applied to all types of contacts. However,
                                                    specific interventions can be tailored to contacts in different settings, including school, work
                                                  and home. If you would like to tailor interventions to contacts in specific settings, select these below."),

                                           checkboxGroupInput("intSetting_c", "Add setting-specific interention on daily contacts:", inline = TRUE,
                                                              choices=list("School" = "school", "Work" = "work", "Home" = "home"))

                                       )
                                ),

                                div(style = 'overflow-y:scroll;height:820px;',
                                    column(width = 12,

                                           box(width="100%", collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
                                               title = HTML(paste(icon("handshake"), "Transmission probability", sparklineOutput("sparklineGeneral_t"))),
                                               column(width = 7,
                                                      plotOutput("intGeneral_t", height = 270,
                                                                 click = "intGeneral_click_t",
                                                                 dblclick = "intGeneral_dblclick_t",
                                                                 hover = hoverOpts("intGeneral_hover_t", delay=100, delayType = "debounce")
                                                      ),
                                                      textOutput("tooltipGeneral_t"),
                                                      helpText("Click to add a point. Double-click to remove."),
                                                      hr(),
                                                      actionButton("resetGeneral_t", "Reset"),
                                                      actionButton("undoGeneral_t", "Undo")
                                               ),

                                               column(width = 5,
                                                      DTOutput("reviewGeneral_t")
                                               )
                                           ), # Closes box

                                           # All
                                           box(width="100%", collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
                                               title = HTML(paste(icon("users"), "Social contacts", sparklineOutput("sparklineGeneral_c"))),
                                               helpText("If school, work, or home-based interventions are selected, those contacts will be excluded from the intervention specified here."),
                                               column(width = 7,
                                                      plotOutput("intGeneral_c", height = 270,
                                                                 click = "intGeneral_click_c",
                                                                 dblclick = "intGeneral_dblclick_c",
                                                                 hover = hoverOpts("intGeneral_hover_c", delay=100, delayType = "debounce")
                                                      ),
                                                      textOutput("tooltipGeneral_c"),
                                                      helpText("Click to add a point. Double-click to remove."),
                                                      hr(),
                                                      actionButton("resetGeneral_c", "Reset"),
                                                      actionButton("undoGeneral_c", "Undo")
                                               ),

                                               column(width = 5,
                                                      DTOutput("reviewGeneral_c")
                                               )
                                           ), # Closes box

                                           # Schools
                                           conditionalPanel(

                                               condition = "input.intSetting_c.includes('school')",
                                               box(width="100%", collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
                                                   title = HTML(paste(icon("school"), "Contacts at school", sparklineOutput("sparklineSchool_c"))),
                                                   helpText("Apply a distinct intervention for school-based contacts"),
                                                   column(width = 7,
                                                          plotOutput("intSchool_c", height = 270,
                                                                     click = "intSchool_click_c",
                                                                     dblclick = "intSchool_dblclick_c",
                                                                     hover = hoverOpts("intSchool_hover_c", delay=100, delayType = "debounce")
                                                          ),
                                                          textOutput("tooltipSchool_c"),
                                                          helpText("Click to add a point. Double-click to remove."),
                                                          hr(),
                                                          actionButton("resetSchool_c", "Reset"),
                                                          actionButton("undoSchool_c", "Undo")
                                                   ),

                                                   column(width = 5,
                                                          DTOutput("reviewSchool_c")
                                                   )
                                               ) # Closes box
                                           ), # Closes Conditional panel

                                           # Work
                                           conditionalPanel(

                                               condition = "input.intSetting_c.includes('work')",
                                               box(width="100%", collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
                                                   title = HTML(paste(icon("building"), "Contacts at work", sparklineOutput("sparklineWork_c"))),
                                                   helpText("Apply a distinct intervention for work-based contacts"),
                                                   column(width = 7,
                                                          plotOutput("intWork_c", height = 270,
                                                                     click = "intWork_click_c",
                                                                     dblclick = "intWork_dblclick_c",
                                                                     hover = hoverOpts("intWork_hover_c", delay=100, delayType = "debounce")
                                                          ),
                                                          textOutput("tooltipWork_c"),
                                                          helpText("Click to add a point. Double-click to remove."),
                                                          hr(),
                                                          actionButton("resetWork_c", "Reset"),
                                                          actionButton("undoWork_c", "Undo")
                                                   ),

                                                   column(width = 5,
                                                          DTOutput("reviewWork_c")
                                                   )
                                               ) # Closes box
                                           ), # Closes Conditional panel

                                           # Home
                                           conditionalPanel(

                                               condition = "input.intSetting_c.includes('home')",
                                               box(width="100%", collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
                                                   title = HTML(paste(icon("home"), "Contacts at home", sparklineOutput("sparklineHome_c"))),
                                                   helpText("Apply a distinct intervention for work-based contacts"),
                                                   column(width = 7,
                                                          plotOutput("intHome_c", height = 270,
                                                                     click = "intHome_click_c",
                                                                     dblclick = "intHome_dblclick_c",
                                                                     hover = hoverOpts("intHome_hover_c", delay=100, delayType = "debounce")
                                                          ),
                                                          textOutput("tooltipHome_c"),
                                                          helpText("Click to add a point. Double-click to remove."),
                                                          hr(),
                                                          actionButton("resetHome_c", "Reset"),
                                                          actionButton("undoHome_c", "Undo")
                                                   ),

                                                   column(width = 5,
                                                          DTOutput("reviewHome_c")
                                                   )
                                               ) # Closes box
                                           ) # Closes Conditional panel

                                    ) # Closes Right-Hand column
                                ) # Closes div
                            )  # Closes fluidPage


                    ), # Closes tabPanel

                    # Results
                    tabItem(tabName = "results",
                            div(style="text-align:center",
                                box(width = "100%", status = 'primary', solidHeader = TRUE,
                                    h4(HTML(paste(icon("paper-plane"), "Run simulation"))))
                            ),
                             fluidRow(
                                 column(width=3,
                                        box(title = NULL, width=NULL, height="700px",
                                            solidHeader = FALSE,
                                            div(style="text-align:center",
                                                h4(HTML(paste(icon('sliders-h'), 'Presentation settings')))
                                            ),
                                            hr(),

                                            checkboxGroupInput("plotvars", "Compartments to plot:",
                                                               choices = list(
                                                                   "Susceptible" = "S",
                                                                   "Exposed" = "E",
                                                                   "Infectious" = "I",
                                                                   "Recovered" = "R"),
                                                               selected=c("I")
                                            ),
                                            hr(),
                                            radioButtons("scale", "Scale",
                                                         choices = list(
                                                             "Count",
                                                             "Percent",
                                                             "Per 100,000"),
                                                         inline = TRUE,
                                                         selected=c("Count")
                                            ),
                                            materialSwitch("logScale", "Log scale", FALSE, inline=TRUE, status = "info"),
                                            hr(),
                                            sliderInput("ndays",
                                                        "Number of days to plot:",
                                                        min=0, step=1, max=365, value=365),
                                            selectizeInput("compCountries", "Compare to existing data:", choices = c("", ctry3), multiple = FALSE),
                                            selectizeInput("compProvince", "Specify province or state (where available):", choices = c("", unique(covid19_data$province)), multiple = FALSE)
                                            )
                                 ),

                                 column(width=6,
                                        box(title = NULL, width = "100%", height="700px", solidHeader = FALSE,
                                            div(style="text-align:center",
                                                h4(HTML(paste(icon('chart-area'), 'Infection dynamics over time')))
                                            ),
                                            hr(),

                                            actionButton(inputId = "runMod", "Click to run simulation",
                                                         icon = icon("paper-plane"),
                                                         width = '100%',
                                                         class = "btn-success"),

                                            withLoader(ggiraphOutput("plot"), type="image", loader="SARS-CoV-2.gif"),

                                            downloadButton('downloadPlot','Download Plot', icon=icon("download"), width="100%", class = "btn-info")
                                        )
                                 ),

                                 column(width=3,
                                        box(title = NULL,
                                            width = "100%", height="700px", solidHeader = FALSE,
                                            div(style="text-align:center",
                                                h4(HTML(paste(icon('palette'), 'Saving to the report canvas')))
                                            ),
                                            hr(),
                                            actionButton("toreport", label="Add to my report canvas", icon("palette"), width = "100%", class = "btn-success"),
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

                    # Dynamic report
                    tabItem(tabName = "canvas",
                            div(style="text-align:center",
                                box(width = "100%", status = 'primary', solidHeader = TRUE,
                                    h4(HTML(paste(icon("palette"), "Report canvas"))))
                            ),
                                 fluidRow(
                                 box(width=NULL, solidHeader = FALSE,
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



                    # Animate results
                    tabItem(tabName = "video",
                            div(style="text-align:center",
                                box(width = "100%", status = "primary", solidHeader = TRUE,
                                    h4(HTML(paste(icon("video"), "Animate results"))))
                            ),

                            fluidRow(
                                column(width=3,
                                       box(title = NULL, width="100%", height = "600px", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('sliders-h'), 'Presentation settings')))
                                           ),
                                           hr(),
                                           checkboxGroupInput("plotvars_a", "Compartments to plot:",
                                                              choices = list(
                                                                  "Susceptible" = "S",
                                                                  "Exposed" = "E",
                                                                  "Infectious" = "I",
                                                                  "Recovered" = "R"),
                                                              selected=c("I")
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
                                       box(title = NULL, width = "100%", height = "600px", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('chart-area'), 'Infection dynamics over time')))
                                           ),
                                           hr(),
                                           withLoader(imageOutput("animation"), type="image", loader="SARS-CoV-2.gif")
                                       )
                                ),

                                column(width=3,
                                       box(title = NULL, width = "100%", height = "600px", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('chart-area'), 'Animation preview')))
                                           ),
                                           hr(),
                                           helpText("Here you can visualise disease transition over time. Design the plot you would
                                                     like to animate then click on Create Animation to view. If no preview appears below, make sure that you have run a model."),
                                           ggiraphOutput("animation_preview", width='100%', height='200px'),
                                           actionButton(inputId = "runAni", "Create Animation",
                                                        icon = icon("play"),
                                                        width = '100%',
                                                        class = "btn-success")
                                       )
                                )
                            )

                    ),


                    # Data
                    tabItem(tabName = "data",
                            div(style="text-align:center",
                                box(width = "100%", status = "primary", solidHeader = TRUE,
                                    h4(HTML(paste(icon("file-excel"), "Download data"))))
                            ),
                             helpText("Once you run a model you will be able to view and download the data here."),
                             DT::dataTableOutput("mod_df_wide"),
                             hr(),
                             conditionalPanel("output.mod_df_wide",
                                downloadLink("get_wide_data", label = HTML(paste("Download the data", icon("download"))))
                             )
                    ),
                    # Introduction
                    tabItem(tabName = "introduction",
                            div(style="text-align:center",
                                box(width = "100%", status = "primary", solidHeader = TRUE,
                                    h4(HTML(paste(icon("home"), "Introduction"))))
                            ),

                            fluidRow(
                            column(width = 5,
                                   box(title = NULL, width = "100%", height = "60%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(HTML(paste(icon('info'), 'About this app')))
                                       ),
                                       hr(),

                                       helpText(HTML(paste0(
                                           "This Shiny app provides a dynamic user interface for the age-structured SEIR model from the ", code("COVOID"), " R package.
                                           If you have download the ", code("covoid"), " package, you can launch the app by entering ", code("covid::covis('seir')"),
                                           " at the console. Alternatively, you can access the package online at ",
                                           tags$a(href=" https://cbdrh.shinyapps.io/covoidance/", target="_blank", "cbdrh.shinyapps.io/covoidance"), "."
                                       ))),
                                       hr(),

                                    h4(HTML(paste(icon("tachometer"), "Parameterise model:"))),
                                    tags$ul(
                                        tags$li("Choose simulation setting and dates"),
                                        tags$li(HTML(paste0("Specify model parameters (R", tags$sub("0"), ", ", "&sigma;", ", ", "&gamma;", ").")))
                                    ),
                                    h4(HTML(paste(icon("user-md"), "Define interventions"))),
                                    tags$ul(
                                        tags$li("Specify time-varying interventions on social contacts and transmission probability"),
                                        tags$li("Optional target interventions to specific settings (School/Work/Home)")
                                    ),
                                    h4(HTML(paste(icon("paper-plane"), "Run simulation"))),
                                    tags$ul(
                                        tags$li("Simulate and visualise infection dynamics over time"),
                                        tags$li("Optionally save results to the report canvas")
                                    ),
                                    h4(HTML(paste(icon("palette"), "Report canvas"))),
                                    tags$ul(
                                        tags$li("Review and comment on saved results"),
                                        tags$li("Download a dynamic summary report of your simulations")
                                    ),
                                    h4(HTML(paste(icon("video"), "Animation"))),
                                    tags$ul(
                                        tags$li("Create and download a gif animation of your results")
                                    ),
                                    h4(HTML(paste(icon("file-excel"), "Data"))),
                                    tags$ul(
                                        tags$li("Download the raw simulation data")
                                    )

                                   )
                            ),

                            column(width = 7,
                                   box(title = "Select + to display the animated demonstration", width = "100%", height = "60%", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                                       hr(),
                                       img(src="demo-cropped.gif", align = "left", width='100%')
                                   )
                            )),

                            fluidRow(
                            column(width = 5,
                                   box(title = NULL, width = "100%", height = "40%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(HTML(paste(icon('envelope'), 'Contact us')))
                                       ),
                                       hr(),

                                       p(tags$a(href="https://cbdrh.med.unsw.edu.au/", icon("chrome"), target="_blank", "Centre for Big Data Research in Health website")),
                                       p(tags$a(href="https://github.com/CBDRH/", icon("github"), target="_blank", "Centre for Big Data Research in Health on GitHub")),
                                       p(tags$a(href="https://twitter.com/cbdrh1", icon("twitter"), target="_blank", "Centre for Big Data Research in Health on Twitter")),

                                   )),

                            column(width = 7,
                                   box(title = NULL, width = "100%", height = "40%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(HTML(paste(icon('hands-helping'), 'Acknowledgements')))
                                       ),
                                       hr(),
                                       p(HTML(paste0(
                                           "Work on COVOID is facilitated by the generous assistance of ",
                                           strong("Ian Sharp"), ", philanthropic supporter of UNSW research.",
                                           " If you would like to support our work, please visit the ",
                                           tags$a(href="https://www.alumni.giving.unsw.edu.au/", "UNSW Alumni & Giving web site"), ".", br(), br(),
                                           "The Bedegal and Gadigal people of the Eora Nation are the traditional owners of the land on
                                           which this work was undertaken. We acknowledge and pay our respects to their Elders, both past, present and emerging."
                                       )))

                                   )
                            ))

                    )

                ) # Closes tabsetPanel

) # Closes dashboardBody


# Put them together into a dashboardPage
dashboardPage(
    header,
    sidebar,
    body
)

