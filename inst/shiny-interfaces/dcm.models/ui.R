###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################


###################################################################################



# Define the dashboar header
header <- dashboardHeader(title = "COVOID: COvid-19 Open-source Infection Dynamics", titleWidth = 450,
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/',
                                         icon('github'), "Source Code", target="_blank"))
)

# Sidebar (icons from https://fontawesome.com/icons)
sidebar <- dashboardSidebar(width = 450,
                            div(style="text-align:center",
                                h3(icon("tachometer-alt"), "Model Settings")
                            ),
            hr(),
            div(style = 'overflow-y:scroll;height:600px;',
                sliderInput(inputId = "nsteps",
                            label = "Time steps:",
                            min = 0,
                            max=1000,
                            step=5,
                            value = 365),
                hr(),
            div(style="text-align:center",
                h4(icon("globe-asia"), "Initial conditions"),
                em(htmlOutput("popcount"))
            ),

            hr(),

            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        numericInput("s_num", "Susceptible", min=0, value=9997),
                        numericInput("e_num", "Exposed", min=0, value=3),
                        numericInput("i1_num", "Infected Stage 1", min=0, value=0)
            ),

            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        numericInput("i2_num", "Infected Stage 2", min=0, value=0),
                        numericInput("iq1_num", "Inf Quar Stage 1", min=0, value=0),
                        numericInput("iq2_num", "Inf Quar Stage 2", min=0, value=0)
            ),

            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        numericInput("h_num", "Hospitalised", min=0, value=0),
                        numericInput("hq_num", "Hosp Quar", min=0, value=0),
                        numericInput("r_num", "Recovered", min=0, value=0)
            ),

            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        numericInput("rq_num", "Recovered Quar", min=0, value=0),
                        numericInput("rh_num", "Recovered Hosp", min=0, value=0),
                        numericInput("rqh_num", "Hosp Quar Rec", min=0, value=0)
            ),

            hr(),
            div(style="text-align:center",
                h4(icon("sliders-h"), "Model parameters")
            ),
            hr(),
            splitLayout(cellWidths = c('33%', '33%', '33%'),
                sliderInput("r0", HTML(paste0("R",tags$sub("0"))), min=0, max=10, value=2.5, step=.1, animate= TRUE),
                sliderInput("beta", HTML("&beta;"), min=0, max=1, value=0, step=.1, animate= TRUE),
                sliderInput("sigma", HTML("&sigma;"), min=0, max=1, value=0.3, step=.01, animate= TRUE)
            ),
            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        sliderInput("gamma1", HTML(paste0("&gamma;",tags$sub("1"))), min=0, max=1, value=0.3, step=.01, animate= TRUE),
                        sliderInput("gamma2", HTML(paste0("&gamma;",tags$sub("2"))), min=0, max=1, value=0.3, step=.01, animate= TRUE),
                        sliderInput("gamma3", HTML(paste0("&gamma;",tags$sub("3"))), min=0, max=1, value=0.3, step=.01, animate= TRUE)
            ),
            splitLayout(cellWidths = c('33%', '33%', '33%'),
                sliderInput("q_eff", HTML(paste0("Q",tags$sub("Eff"))), min=0, max=1, value=0.5, step=.01, animate= TRUE),
                sliderInput("h_eff", HTML(paste0("H",tags$sub("Eff"))), min=0, max=1, value=0.99, step=.01, animate= TRUE),
                sliderInput("rho", HTML("&rho;"), min=0, max=1, value=0.1, step=.01, animate= TRUE)
            ),
            splitLayout(cellWidths = c('33%', '33%', '33%'),
                        sliderInput("alpha", HTML("&alpha;"), min=0, max=1, value=0.2, step=.01, animate= TRUE),
                        sliderInput("eta", HTML("&eta;"), min=0, max=1, value=0.01, step=.01, animate= TRUE)
            )

    ) # Closes div
) # closes Sideboard


## Define the Body
body <- dashboardBody(
    tabsetPanel(

        # Results
        tabPanel("Results"),

        # About
        tabPanel("About")
    ),

# Tool tips
bsTooltip("nsteps", "Number of time steps over which to sample from the model", placement = 'bottom'),
# Initial conditions
bsTooltip("s_num", "Initial number susceptible", placement = 'right'),
bsTooltip("e_num", "Initial number exposed", placement = 'right'),
bsTooltip("i1_num", "Initial number infected (stage 1)", placement = 'left'),
bsTooltip("i2_num", "Initial number infected (stage 2)", placement = 'right'),
bsTooltip("iq1_num", "Initial number of infected who had self quarantined (Stage 1)", placement = 'right'),
bsTooltip("iq2_num", "Initial number of infected who had self quarantined (Stage 2)", placement = 'left'),
bsTooltip("h_num", "Initial number hospitalised", placement = 'right'),
bsTooltip("hq_num", "Initial number of hospitalised (who had self quarantined)", placement = 'right'),
bsTooltip("r_num", "Initial number recovered", placement = 'left'),
bsTooltip("rq_num", "Initial number of recovered (who had self quarantined)", placement = 'right'),
bsTooltip("rh_num", "Initial number of recovered hospitalisations", placement = 'right'),
bsTooltip("rqh_num", "Initial number of recovered hospitalisations (who had self quarantined)", placement = 'left'),

# Model parameters
bsTooltip("r0", "Basic reproduction number (S -> E)", placement = 'right'),
bsTooltip("beta", HTML(paste0("&beta;",tags$em(" (beta)"), " = Rate of potential new infections per infected (S -> E)")), placement = 'right'),
bsTooltip("sigma", HTML(paste0("&sigma;",tags$em(" (sigma)"), " = Inverse of the average length of latent period (E -> I1)")), placement = 'left'),
bsTooltip("gamma1", HTML(paste0("&gamma;", tags$sub("1"), tags$em(" (gamma one)"), " = Inverse of the average length of first infectious period (I1 -> I2 or H)")), placement = 'right'),
bsTooltip("gamma2", HTML(paste0("&gamma;", tags$sub("2"), tags$em(" (gamma two)"), " = Inverse of the average length of second infectious period (I2 -> R)")), placement = 'right'),
bsTooltip("gamma3", HTML(paste0("&gamma;", tags$sub("3"), tags$em(" (gamma three)"), " = Inverse of the average length of second infectious period (for hospitalised) (H -> R or F)")), placement = 'left'),
bsTooltip("q_eff", "Q effect = Effect of quarantine on infectiousness", placement = 'right'),
bsTooltip("h_eff", "H effect = Effect of hospitalisation on infectiousness", placement = 'right'),
bsTooltip("rho", HTML(paste0("&rho;",tags$em(" (rho)"), " = Proportion of people who enter quarantine after exposure (E -> Iq1 or I1)")), placement = 'left'),
bsTooltip("alpha", HTML(paste0("&alpha;",tags$em(" (alpha)"), " = Proportion of infected requiring hospitalisation (I1 -> I2 or H)")), placement = 'right'),
bsTooltip("eta", HTML(paste0("&eta;",tags$em(" (eta)"), " = Case fatality rate (H -> F or Rh)")), placement = 'right')
)

# Put them together into a dashboardPage
dashboardPage(
    header,
    sidebar,
    body
)

