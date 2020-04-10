###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################


###################################################################################


# Define the dashboar header
header <- dashboardHeader(title = "COVOID: COvid-19 Open-source Infection Dynamics", titleWidth = 500,
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/',
                                         icon('github'), "Source Code", target="_blank"))
)

# Sidebar (icons from https://fontawesome.com/icons)
sidebar <- dashboardSidebar(
    selectInput("modType", "Choose a model type:",
                choices = list("SIR", "SEIR", "SEIQHR")),
    sidebarMenu(
        menuItem("Controls", tabName = "controls", icon = icon("tachometer-alt")),
        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
)

## Define the Body
body <- dashboardBody(


    tabItems(

        # Controls
        tabItem(tabName = "controls", h3("Model Parameterisation"),

                fluidRow(
                  box(title = "Results", solidHeader = TRUE, height = 5)
                ),

                fluidRow(

                    # SIR Model
                    conditionalPanel("input.modType=='SIR'",
                        h4("SIR"),
                        splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
                                    numericInput(inputId = "s.num",
                                                 label = "Number Susceptible:",
                                                 min = 0,
                                                 value = ""),

                                    numericInput(inputId = "i.num",
                                                 label = "Number Infected and Infectious:",
                                                 min = 0,
                                                 value = ""),

                                    numericInput(inputId = "r.num",
                                                 label = "Number Recovered:",
                                                 min = 0,
                                                 value = "")
                        )
                    ),

                    # SEIR Model
                    conditionalPanel("input.modType=='SEIR'",
                                     h4("SEIR")
                    ),

                    # SEIQHR Model
                    conditionalPanel("input.modType=='SEIQHR'",
                                     h4("SEIQHR")
                    )

                )
        ),


        # Results
        tabItem(tabName = "results",
                fluidRow(
                    h3("Model Results")
                )
        ),

        # About
        tabItem(tabName = "about",
                fluidRow(
                    h3("Further information")
                )
        )
    )
)

# Put them together into a dashboardPage
dashboardPage(
    header,
    sidebar,
    body
)
