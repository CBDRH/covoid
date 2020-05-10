###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# Extensions to the Doherty model
# incl. Time varying interventions

###################################################################################


shinyServer(function(session, input, output) {


# Add up the different populations and display the population count
popcounter <- reactive({
    sum(default$s_num(), default$i_num(), default$r_num())
})



# Output the population count
output$popcount = renderText({
    HTML(paste("Population Size: ", formatC(popcounter(), format="d", big.mark=',')))
})

# Calculate the number of simulation days
nsteps <- reactive({as.numeric( input$dateRange[2] - input$dateRange[1]) })

# Update graph range sliders based on nsteps
observe(updateSliderInput(session, "ndays", max = nsteps()))
observe(updateSliderInput(session, "ndays_a", max = nsteps()))

# Visualise the contact matrix as a heatmap
ageMat <- reactive({
  req(default$countryAgeMat())
  import_contact_matrix(default$countryAgeMat(), setting=default$settingAgeMat())
  })


output$heatMap <- renderPlotly({
  heatmaply(ageMat(), dendrogram = FALSE, hide_colorbar = TRUE, margins = c(2,2,2,2), fontsize_row=6, fontsize_col=6,
            titleX = FALSE, titleY = FALSE, label_names = c('Age group Y', 'Age group X', 'Value'))
})

# Age distribution based on selected country
ageDist <- reactive({
  req(default$countryAgeDist())
  if(default$countryAgeDist() == "Not found, please choose manually")
    return("Not found, please choose manually")

  import_age_distribution(default$countryAgeDist())

})

# Custom distribution of infected population
infDist <- reactive({
  dgamma(seq(1:16), shape = input$shape_inf, scale = input$scale_inf)/sum(dgamma(seq(1:16), shape = input$shape_inf, scale = input$scale_inf))
})

# Custom distribution of recovered population
recDist <- reactive({
  dgamma(seq(1:16), shape = input$shape_rec, scale = input$scale_rec)/sum(dgamma(seq(1:16), shape = input$shape_rec, scale = input$scale_rec))
})

# Update second country choice based on first country choice
choiceCountry <- eventReactive(input$countryAgeMat ,{
  ifelse(input$countryAgeMat %in% age_distributions_un(), input$countryAgeMat, "Not found, please choose manually")
})

observe(updateSelectizeInput(session, "countryAgeDist", selected = choiceCountry()))

dfAgeDist <- reactive({
                req(default$countryAgeDist())

                data.frame(
                  x = seq(from=5, to=80, by=5),
                  y = ageDist()
                  ) %>%
                  mutate(
                    pct = round(100*(y), digits=1),
                    tt = paste0(x, "-", x+4, " years: ", pct, "%")
                    )
            })

output$ageHist <- renderggiraph({

if(input$countryAgeDist == "Not found, please choose manually"){
  return()
}

p1 <- ggplot(dfAgeDist(), aes(x,y, fill=y)) +
  geom_col_interactive(aes(tooltip = tt)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Age group", y = "Percentage") +
  scale_fill_viridis() +
  theme(legend.position = "none")

z <- girafe(code = print(p1))
girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150


})


################################################################################################
                        ### Defining the Model Compartments Diagram ###
################################################################################################

output$network_d <- renderVisNetwork({

    nodes <- data.frame(id = 1:3,

        # add labels on nodes
        label = c('S', 'I', 'R'),

        # size adding value
        size = c(rep(10, 3)),

        # Hierarchical level
        level = c(0,0,0),

        # Group
        group = c("Susceptible", "Infectious", "Recovered"),

        # control shape of nodes
        shape = c(rep("square", 3)),

        font.color = c(rep("grey", 3)),

        # Don't need physics
        physics = rep(FALSE, 3),

        shadow = c(rep(FALSE,3)),

        # tooltip (html or character), when the mouse is above
        title = c(HTML(paste0("S", br(), 'Susceptible')),
                  HTML(paste0("I", br(), "Infectious")),
                  HTML(paste0("R", br(), "Recovered"))
        )

    )

    edges <- data.frame(from = c(1, 2),
        to = c(2, 3),
        id = LETTERS[seq( from = 1, to = 2 )],
        dashes = c(rep(FALSE,2)),

        # Label text
        label = c(
            round(999, digits=3),
            round(default$gamma(), digits=3)
            ),

        # Label colour
        font.color = c(rep("darkgrey", 2)),

        # Hover titles
        title = c(HTML('&beta;I/N'),
                  HTML('&gamma;')
                  )
    )


    visNetwork(nodes, edges, submain = HTML("SIR model representation")) %>%
        visHierarchicalLayout(direction = "UD", levelSeparation = 80) %>%
        visEdges(arrows = "to") %>%
        visGroups(groupname = "Susceptible", color = "lightblue") %>%
        visGroups(groupname = "Infectious", color = "red") %>%
        visGroups(groupname = "Recovered", color = "lightgreen") %>%
        visLegend(width = 0.1, position = "left", main = "Compartment") %>%
        visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
        visInteraction(hover = TRUE) %>%
        visPhysics(stabilization = FALSE) %>%
        visEvents(startStabilizing = "function() {
            this.moveTo({scale:1.0})}") %>%
        visEvents(selectNode = "function(data) {
                Shiny.onInputChange('node_id', data.nodes);
                ;}",
                  selectEdge = "function(data) {
                Shiny.onInputChange('edge_id', data.edges);
                ;}"
                  )
})


################################################################################################
                    ### Set parameters by clicking on a network node ###
################################################################################################

## Set current node on click

# values$setNode and values$setEdge are placeholders for the clicked node and edge
values <- reactiveValues(setNode = 0, setEdge = 0)

# They start out as NULL so need to replace that with zero
observe(values$setNode <- ifelse(is.null(input$node_id), 0, input$node_id))
observe(values$setEdge <- ifelse(is.null(input$edge_id), 0, input$edge_id))

setNode <- reactive({values$setNode})
setEdge <- reactive({values$setEdge})

# They are reverted to 0 and z respectively every time a dialogue box is closed
observeEvent(input$modal_done,{
    values$setNode <- 0
    values$setEdge <- 'Z'
    removeModal()
})

observe(
## Nodes

    # S.
    if (setNode()==1) { isolate(
            showModal(modalDialog(
                title = "Susceptible individuals",
                size = 'm',
                code("S"),
                helpText("The initial number of susceptible individuals"),
                numericInput("s_num", label=NULL, min=0, value=default$s_num()),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

    # I.
    else if (setNode()==2) { isolate(
        showModal(modalDialog(
            title = "Infectious individuals",
            size = 'm',
            code(HTML(paste0("I"))),
            helpText("Individuals who are infectious"),
            numericInput("i_num", NULL, min=0, value=default$i_num()),
            radioButtons("i_num_dist", "Distribution of infectious individuals across age groups",
                         choices = c("Uniform", "Custom"), selected = default$i_num_dist(), inline = TRUE),
            conditionalPanel(condition = "input.i_num_dist == 'Uniform'",

                             renderggiraph({

                                   p1 <- dfAgeDist() %>%
                                     mutate(inf = y*default$i_num()) %>%
                                     ggplot(aes(x=x, y=inf, fill=inf)) +
                                       geom_col_interactive(aes(tooltip = paste0(x, "-", x+4, " years, N = ", round(inf, digits=0)))) +
                                       labs(x="Age group", y = "Count") +
                                       scale_fill_viridis() +
                                       theme(legend.position = "none")

                                   z <- girafe(code = print(p1))
                                   girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150

                                 })
                             ),

            conditionalPanel(condition = "input.i_num_dist == 'Custom'",
                        fluidRow(
                          column(width=5,
                               renderggiraph({

                                 p1 <- dfAgeDist() %>%
                                   mutate(inf = default$i_num()*infDist()) %>%
                                   ggplot(aes(x=x, y=inf, fill=inf)) +
                                   geom_col_interactive(aes(tooltip = paste0(x, "-", x+4, " years, N = ", round(inf, digits=0)))) +

                                   labs(x="Age group", y = "Count") +
                                   scale_fill_viridis() +
                                   theme(legend.position = "none")

                                 z <- girafe(code = print(p1))
                                 girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150

                               })
                          ),

                              column(width = 3,
                                     radioButtons("opts_inf", "Presets:",
                                                  selected = default$opts_inf(),
                                                  choices = list("Younger children" = 1,
                                                                   "Young adults" = 2,
                                                                   "Middle aged" = 3,
                                                                   "Older adults" = 4))
                                     ),

                              column(width=4,
                                   sliderInput("shape_inf", "Custom shape:", min=0.1, max=10, step=.1, value = default$shape_inf(), animate = TRUE),
                                   sliderInput("scale_inf", "Custom scale:", min=0.1, max=3, step=.1, value = default$scale_inf(), animate = TRUE)
                              )
                  ) # Closes fluidRow
            ), # Closes Conditional Panel

            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done") ))
    )}

    # R.
    else if (setNode()==3) { isolate(
        showModal(modalDialog(
            title = "Recovered",
            size = 'm',
            code(HTML(paste0("R"))),
            helpText("Individuals who are recovered and no longer infectious"),
            numericInput("r_num", NULL, min=0, value=default$r_num()),
            radioButtons("r_num_dist", "Distribution of recovered individuals across age groups",
                         choices = c("Uniform", "Custom"), selected = default$r_num_dist(), inline = TRUE),
            conditionalPanel(condition = "input.r_num_dist == 'Uniform'",

                             renderggiraph({

                               p1 <- dfAgeDist() %>%
                                 mutate(rec = y*default$r_num()) %>%
                                 ggplot(aes(x=x, y=rec, fill=rec)) +
                                 geom_col_interactive(aes(tooltip = paste0(x, "-", x+4, " years, N = ", round(rec, digits=0)))) +
                                 labs(x="Age group", y = "Count") +
                                 scale_fill_viridis() +
                                 theme(legend.position = "none")

                               z <- girafe(code = print(p1))
                               girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150

                             })
            ),

            conditionalPanel(condition = "input.r_num_dist == 'Custom'",
                             fluidRow(
                               column(width=5,
                                      renderggiraph({

                                        p1 <- dfAgeDist() %>%
                                          mutate(rec = recDist()*default$r_num()) %>%
                                          ggplot(aes(x=x, y=rec, fill=rec)) +
                                          geom_col_interactive(aes(tooltip = paste0(x, "-", x+4, " years, N = ", round(rec, digits=0)))) +
                                          labs(x="Age group", y = "Count") +
                                          scale_fill_viridis() +
                                          theme(legend.position = "none")

                                        z <- girafe(code = print(p1))
                                        girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150

                                      })
                               ),

                               column(width = 3,
                                      radioButtons("opts_rec", "Presets:",
                                                   selected = default$opts_rec(),
                                                   choices = list("Younger children" = 1,
                                                                  "Young adults" = 2,
                                                                  "Middle aged" = 3,
                                                                  "Older adults" = 4))
                               ),

                               column(width=4,
                                      sliderInput("shape_rec", "Custom shape", min=0.1, max=10, step=.1, value = default$shape_rec(), animate = TRUE),
                                      sliderInput("scale_rec", "Custom scale", min=0.1, max=3, step=.1, value = default$scale_rec(), animate = TRUE)
                               )
                             ) # Closes fluidRow
            ), # Closes Conditional Panel
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}


) # Closes Observe




##############
### Edges ###
##############

observe(
    # S -> I
    if (setEdge()=='A' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Susceptible to Infectious (S", "&rarr;", "I)")),
            size = 'm',
            helpText(HTML(paste0("The transition rate between the ", strong("Susceptible"), " and ", strong("Infectious"), " compartments
                                 is given by the formula ", "&beta; &times; I &frasl; N."))),
            helpText(HTML(paste0("&beta; is the average number of daily contacts per person (C) multiplied by the probability of disease transmission at each contact (P", tags$sub("inf"), ")" ))),
            helpText(HTML(paste0("I &frasl; N is the fraction of contacts that involve an infectious individual."))),
            hr(),
             # R0
             code(HTML(paste0("R", tags$sub("0")))),
             helpText("The basic reproduction number."),
             sliderInput("r0", NULL, min=0, max=4, step = .1, value = 2.5),
             hr(),
            h4("Number of daily contacts:"),

              fluidRow(
                column(width = 6,
                       selectizeInput("countryAgeMat", "Contact matrix", choices = available_contact_matrices(), selected = default$countryAgeMat(), multiple = FALSE, options = NULL),
                       plotlyOutput("heatMap", height = "220px", width = "220px")
                ),

                column(width = 6,
                       selectizeInput("countryAgeDist", "Age distribution", choices = c(age_distributions_un(), "Not found, please choose manually"), selected = default$countryAgeDist(), multiple = FALSE, options = NULL),
                       ggiraphOutput("ageHist", height = "400px", width = "200px")
                )
                ),
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I -> R
    else if (setEdge()=='B' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Infectious to recovered (I &rarr; R)")),
            size = 'm',
            code(HTML(paste0("&gamma;"))),
            helpText(HTML(paste0("Inverse of the duration of infection."))),
            hr(),
            sliderInput("gammainv", label="Duration of infection in days:", min=1, max=21, step=.5, value=1/default$gamma()),
            htmlOutput("gammainv"),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}



) # Closes observe


### Text updates for user when moving 'inverse' plots
output$gammainv <- renderText(HTML(paste0("&gamma;", " = 1/" , input$gammainv, " = ", round(default$gamma(), digits=3))))


############################################################################
                    ### Set parameter default values ###

# This is neccesary because a lot of the parameter inputs are
# placed in dialogue boxes and consequenlty are NULL until the
# user opens the dialogue box. The default values are the value
# given to an input parameter until the user chooses something else
############################################################################

default <- reactiveValues()

# Initial conditions
default$s_num <- reactive(ifelse(is.null(input$s_num), 4000000, input$s_num))
default$i_num <- reactive(ifelse(is.null(input$i_num), 300, input$i_num))
default$r_num <- reactive(ifelse(is.null(input$r_num), 700, input$r_num))

default$i_num_dist <- reactive(ifelse(is.null(input$i_num_dist), "Uniform", input$i_num_dist))
default$r_num_dist <- reactive(ifelse(is.null(input$r_num_dist), "Uniform", input$r_num_dist))

# Model parameters
default$r0 <- reactive(ifelse(is.null(input$r0), 2.5, input$r0))
default$gamma <- reactive(ifelse(is.null(input$gammainv), 1/4.0, 1/input$gammainv))

# Country choices
default$countryAgeMat <- reactive(ifelse(is.null(input$countryAgeMat), "Australia", input$countryAgeMat))
default$settingAgeMat <- reactive(ifelse(is.null(input$settingAgeMat), "general", input$settingAgeMat))
default$countryAgeDist <- reactive(ifelse(is.null(input$countryAgeDist), "Australia", input$countryAgeDist))


# Calculated parameters
default$beta <- reactive(default$r0()*((default$sigma2())^-1 + (default$gamma1())^-1 + (default$gamma2())^-1)^(-1))


default$opts_inf <- reactive(ifelse(is.null(input$opts_inf), 1, input$opts_inf))
default$opts_rec <- reactive(ifelse(is.null(input$opts_rec), 1, input$opts_rec))

default$shape_inf <- reactive(ifelse(is.null(input$shape_inf), 1, input$shape_inf))
default$scale_inf <- reactive(ifelse(is.null(input$scale_inf), 2, input$scale_inf))
default$shape_rec <- reactive(ifelse(is.null(input$shape_rec), 1, input$shape_rec))
default$scale_rec <- reactive(ifelse(is.null(input$scale_rec), 2, input$scale_rec))


observeEvent(input$opts_inf, {
  if(input$opts_inf==1) {
      updateSliderInput(session, "shape_inf", value = 1)
      updateSliderInput(session, "scale_inf", value = 2)
  }
  if(input$opts_inf==2) {
    updateSliderInput(session, "shape_inf", value = 5)
    updateSliderInput(session, "scale_inf", value = 1.3)
  }
  if(input$opts_inf==3) {
    updateSliderInput(session, "shape_inf", value = 9)
    updateSliderInput(session, "scale_inf", value = 1.3)
  }
  if(input$opts_inf==4) {
    updateSliderInput(session, "shape_inf", value = 10)
    updateSliderInput(session, "scale_inf", value = 2.5)
  }
})

observeEvent(input$opts_rec, {
  if(input$opts_rec==1) {
    updateSliderInput(session, "shape_rec", value = 1)
    updateSliderInput(session, "scale_rec", value = 2)
  }
  if(input$opts_rec==2) {
    updateSliderInput(session, "shape_rec", value = 5)
    updateSliderInput(session, "scale_rec", value = 1.3)
  }
  if(input$opts_rec==3) {
    updateSliderInput(session, "shape_rec", value = 9)
    updateSliderInput(session, "scale_rec", value = 1.3)
  }
  if(input$opts_rec==4) {
    updateSliderInput(session, "shape_rec", value = 10)
    updateSliderInput(session, "scale_rec", value = 2.5)
  }
})

observeEvent(input$intSetting, {
  if("general" %in% input$intSetting) {
    updateCheckboxGroupInput(session, "intSetting", selected = c("general"))
  }
})



############################################################################
                    ### Summary text output ###
############################################################################

# Non-Quarantined population
output$summary1 <- renderText({
    HTML(paste(icon("globe-asia"), em(" Non-quarantined population"), hr(),
        "Susceptible:", code(default$s_num()), br(),
        "Infectious:", code(default$i_num()), br(),
        "Recovered", code(default$r_num())
        ))
})

# # Quarantined population
# output$summary2 <- renderText({
#     HTML(paste(icon("globe-asia"), em(" Quarantined population"), hr(),
#               "Latent (1st Period):", code(default$e1q_num()), br(),
#               "Latent (2nd Period):", code(default$e2q_num()), br(),
#               "Infectious (1st Period):", code(default$i1q_num()), br(),
#               "Infectious (2nd Period):", code(default$i2q_num()), br(),
#               "Managed:", code(default$mq_num()), br(),
#               "Recovered:", code(default$rq_num()), br(),
#               "Recovered from managed:", code(default$rmq_num()), br()
#     ))
# })

# Model parameters (quarantined population)
output$summary3 <- renderText({
    HTML(paste(icon("sliders-h"), em("Transition params"), hr(),
               code(HTML(paste0("R", tags$sub("0")))), round(default$r0(), digits = 3), br(),
               code(HTML(paste0("&gamma;"))), round(default$gamma(), digits = 3)

    ))
})


# output$summary4 <- renderText({
#     HTML(paste(icon("sliders-h"), em("Transition params"), hr(),
#             code(HTML(paste0("&sigma;", tags$sub("1")))), "1/", round(1/default$sigma1(), digits = 3), br(),
#             code(HTML(paste0("&sigma;", tags$sub("2")))), "1/", round(1/default$sigma2(), digits = 3), br(),
#             code(HTML(paste0("&gamma;", tags$sub("1")))), "1/", round(1/default$gamma1(), digits = 3), br(),
#             code(HTML(paste0("&gamma;", tags$sub("2")))), "1/", round(1/default$gamma2(), digits = 3), br(),
#             code(HTML(paste0("&gamma;", tags$sub("1"), tags$sup("q")))), "1/", round(1/default$gamma1q(), digits = 3), br(),
#             code(HTML(paste0("&gamma;", tags$sub("2"), tags$sup("q")))), "1/", round(1/default$gamma2q(), digits = 3), br()
#     ))
# })


############################################################################
              ### Define age distribution based on user choices ###
############################################################################

s_num_vec <- reactive({
  ageDist()*default$s_num() - (i_num_vec() + r_num_vec())
})

i_num_vec <- reactive({
  if(default$i_num_dist() == "Uniform") {ageDist()*default$i_num()}
  else if(default$i_num_dist() == "Custom") {infDist()*default$i_num()}
})


r_num_vec <- reactive({
  if(default$r_num_dist() == "Uniform") {ageDist()*default$r_num()}
  else if(default$r_num_dist() == "Custom") {recDist()*default$r_num()}
})



############################################################################
                ### Define interventions ###
############################################################################




### Intervention in school setting

# 1. set up reactive dataframe
int <- reactiveValues()

# Place holder for clicked values
int$dfSchool <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfWork <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfHome <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfGeneral <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())

# Place holder for values returned when user hovers
int$hvSchool <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvWork <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvHome <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvGeneral <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )

# Create plot
output$intSchool <- renderPlot(clickrPlot(int$dfSchool, input$dateRange[1], input$dateRange[2]))
output$intWork <- renderPlot(clickrPlot(int$dfWork, input$dateRange[1], input$dateRange[2]))
output$intHome <- renderPlot(clickrPlot(int$dfHome, input$dateRange[1], input$dateRange[2]))
output$intGeneral <- renderPlot(clickrPlot(int$dfGeneral, input$dateRange[1], input$dateRange[2]))

# Create Table summary
output$reviewSchool <- renderDT(clickrTable(int$dfSchool, input$dateRange[1]))
output$reviewWork <- renderDT(clickrTable(int$dfWork, input$dateRange[1]))
output$reviewHome <- renderDT(clickrTable(int$dfHome, input$dateRange[1]))
output$reviewGeneral <- renderDT(clickrTable(int$dfGeneral, input$dateRange[1]))

# Update labels that are hovered
observeEvent(input$intSchool_hover, {int$hvSchool <- clickrHover(input$intSchool_hover)})
observeEvent(input$intWork_hover, {int$hvWork <- clickrHover(input$intWork_hover)})
observeEvent(input$intHome_hover, {int$hvHome <- clickrHover(input$intHome_hover)})
observeEvent(input$intGeneral_hover, {int$hvGeneral <- clickrHover(input$intGeneral_hover)})

# Add points that are clicked
observeEvent(input$intSchool_click, {int$dfSchool <- addPoint(int$dfSchool, input$intSchool_click$x, input$intSchool_click$y)})
observeEvent(input$intWork_click, {int$dfWork <- addPoint(int$dfWork, input$intWork_click$x, input$intWork_click$y)})
observeEvent(input$intHome_click, {int$dfHome <- addPoint(int$dfHome, input$intHome_click$x, input$intHome_click$y)})
observeEvent(input$intGeneral_click, {int$dfGeneral <- addPoint(int$dfGeneral, input$intGeneral_click$x, input$intGeneral_click$y)})

# Remove points that are double clicked
observeEvent(input$intSchool_dblclick, {if(nrow(int$dfSchool)) {int$dfSchool <- dropPoint(int$dfSchool, input$intSchool_dblclick$x, input$intSchool_dblclick$y)}})
observeEvent(input$intWork_dblclick, {if(nrow(int$dfWork)) {int$dfWork <- dropPoint(int$dfWork, input$intWork_dblclick$x, input$intWork_dblclick$y)}})
observeEvent(input$intHome_dblclick, {if(nrow(int$dfHome)) {int$dfHome <- dropPoint(int$dfHome, input$intHome_dblclick$x, input$intHome_dblclick$y)}})
observeEvent(input$intGeneral_dblclick, {if(nrow(int$dfGeneral)) {int$dfGeneral <- dropPoint(int$dfGeneral, input$intGeneral_dblclick$x, input$intGeneral_dblclick$y)}})

## Remove last row on actionButton click
observeEvent(input$undoSchool, {int$dfSchool <- undo(int$dfSchool)})
observeEvent(input$undoWork, {int$dfWork <- undo(int$dfWork)})
observeEvent(input$undoHome, {int$dfHome <- undo(int$dfHome)})
observeEvent(input$undoGeneral, {int$dfGeneral <- undo(int$dfGeneral)})

## Reset on click
observeEvent(input$resetSchool, {int$dfSchool <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetWork, {int$dfWork <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetHome, {int$dfHome <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetGeneral, {int$dfGeneral <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})

# Fill in the timeseries
fxSchool <- reactive({if(nrow(int$dfSchool)>=1){fillTime(int$dfSchool, input$dateRange[1], input$dateRange[2])}})
fxWork <- reactive({if(nrow(int$dfWork)>=1){fillTime(int$dfWork, input$dateRange[1], input$dateRange[2])}})
fxHome <- reactive({if(nrow(int$dfHome)>=1){fillTime(int$dfHome, input$dateRange[1], input$dateRange[2])}})
fxGeneral <- reactive({if(nrow(int$dfGeneral)>=1){fillTime(int$dfGeneral, input$dateRange[1], input$dateRange[2])}})

# Spark line for intervention
output$sparklineSchool <- renderSparkline(sparkline(fxSchool()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineWork <- renderSparkline(sparkline(fxWork()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineHome <- renderSparkline(sparkline(fxHome()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineGeneral <- renderSparkline(sparkline(fxGeneral()[[2]], chartRangeMin = 0, chartRangeMin = 1))

# Hover text
output$tooltipSchool <- renderText(paste(int$hvSchool$fulllab))
output$tooltipWork <- renderText(paste(int$hvWork$fulllab))
output$tooltipHome <- renderText(paste(int$hvHome$fulllab))
output$tooltipGeneral <- renderText(paste(int$hvGeneral$fulllab))



### Store contact matrices

cmSchool <- reactive({
  req(default$countryAgeMat())

  if("school" %in% input$intSetting){
  import_contact_matrix(default$countryAgeMat(), setting="school")
  }
  else NULL
})

cmWork <- reactive({
  req(default$countryAgeMat())

  if("work" %in% input$intSetting){
  import_contact_matrix(default$countryAgeMat(), setting="work")
  }
  else NULL
})

cmHome <- reactive({
  req(default$countryAgeMat())

  if("home" %in% input$intSetting){
  import_contact_matrix(default$countryAgeMat(), setting="home")
  }
  else NULL
})

cmGeneral <- reactive({
  req(default$countryAgeMat())

  import_contact_matrix(default$countryAgeMat(), setting="general")

})


### Prepare contact matrix list
cmList <- reactive({

  plist <- list(school = cmSchool(), work = cmWork(), home = cmHome())

  if(is.null(cmSchool())) plist["school"] <- NULL
  if(is.null(cmWork())) plist["work"] <- NULL
  if(is.null(cmHome())) plist["home"] <- NULL

  sum <- Reduce("+", plist)

  general <- if(is.null(sum)) cmGeneral()
  else cmGeneral() - sum


  plist[["general"]] <- general
  plist

})

output$test1 <- reactive(paste("General:", round(sum(cmList()[["general"]]), digits=0)))
output$test2 <- reactive(paste("School:", round(sum(cmList()[["school"]]), digits=0)))
output$test3 <- reactive(paste("Work:", round(sum(cmList()[["work"]]), digits=0)))
output$test4 <- reactive(paste("Home:", round(sum(cmList()[["home"]]), digits=0)))

### Prepare intervention matrix list
intList <- reactive({

  noInt <- data.frame(time = seq(from=1, to = nsteps()),
                c_reduce = rep(1, nsteps())
                )

  intSchool <- if(is.null(fxSchool())) noInt
                else fxSchool()

  intWork <- if(is.null(fxWork())) noInt
  else fxWork()

  intHome <- if(is.null(fxHome())) noInt
  else fxHome()

  intGeneral <- if(is.null(fxGeneral())) noInt
  else fxGeneral()

  out <- list(school = intSchool, work = intWork, home = intHome, general = intGeneral)
  if(is.null(cmSchool())) out["school"] <- NULL
  if(is.null(cmWork())) out["work"] <- NULL
  if(is.null(cmHome())) out["home"] <- NULL
  out

})



############################################################################
            ### Parameterise and run the model ###
############################################################################

### Initial conditions
state0 <- reactive({
    sir_c_state0(S0 = s_num_vec(),
                  I0 = i_num_vec(),
                  R0 = r_num_vec())
})

### Model parameters
param <- reactive({
   sir_c_param(R0 = default$r0(),
                 gamma = default$gamma(),
                 cm = cmList(),
                 dist = ageDist(),
                 intervention = intList()
                 )
})

output$test <- renderText(paste(intList()))

# Run the model when the button is clicked
model <- eventReactive(input$runMod, {
    isolate(
        simulate_c_sir(t = nsteps(),
                        state_t0 = state0(),
                        param = param())
    )
})


# Extract the data in ggplot ready form
mod_df <- reactive({
    prepData(df_epi = model()$epi, day1 = input$dateRange[1], popsize = popcounter())
})


### Save and output data

# Wide format
output$mod_df_wide <- DT::renderDataTable(model()$epi)

# Download data in wide format
output$get_wide_data <- downloadHandler(

    filename = "data-wide.csv",

    content = function(file) {

        write.csv(model()$epi, file, row.names = FALSE)
    }

)


### Model summary
mainPlot <- reactive({plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)})
output$plot <- renderggiraph(mainPlot())

# Make static version the plot for download
staticPlot <- reactive({plotStaticResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)})

# Download the current main plot
output$downloadPlot <- downloadHandler(
    filename = "resultsPlot.png",
    content = function(file) {

        ggsave(file, plot = staticPlot())
    },
    contentType = "image/png"
)

# Interactively update number of days in slider
observe({
    val = min(100, nsteps())
    updateSliderInput(session, "ndays", max=nsteps(), value=val)
    updateSliderInput(session, "ndays_a", max=nsteps(), value=val)
})


## Prepare animation

# Preview
output$animation_preview <- renderggiraph(plotResults(df=mod_df(), scale=input$scale_a, logScale=input$logScale_a, plotvars=input$plotvars_a, ndays=input$ndays_a))

# Animation
animation <- eventReactive(input$runAni, {
    # as per https://shiny.rstudio.com/articles/progress.html#a-more-complex-progress-example
    # but set max value to pre-determined total frame count
    progress <- shiny::Progress$new(max = 100)
    progress$set(message = "Rendering", value = 0)
    on.exit(progress$close())

    updateShinyProgress <- function(detail) {
      progress$inc(1, detail = detail)
    }

    animateResults(df=mod_df(), scale=input$scale_a,
                   logScale=input$logScale_a, plotvars=input$plotvars_a, ndays=input$ndays_a,
                   update_progress = updateShinyProgress)
})

output$animation <- renderImage({

        if(is.null(animation())) {
            return()
        }
        if(!is.null(animation())) {
            animation()
        }

}, deleteFile=TRUE )



## Saving plots for comparison

extra <- reactiveValues() # place holder for additional plots

count <- reactiveValues(nPlots = 0)

observeEvent(input$toreport,
             {
                 if(input$reportFigs==1) {
                     extra$plot1 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot1", value = TRUE)
                     count$nPlots <- 1
                     count$param_p1 <- param()
                     count$state0_p1 <- state0()
                     count$nsteps_p1 <- nsteps()
                     count$scale_p1 <- input$scale
                     count$logScale_p1 <- input$logScale
                     count$plotvars_p1 <- input$plotvars
                     count$ndays_p1 <- input$ndays
                     count$startDate_p1 <- input$dateRange[1]
                     count$popSize_p1 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 2)
                 }

                 if(input$reportFigs==2) {
                     extra$plot2 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot2", value = TRUE)
                     count$nPlots <- 2
                     count$param_p2 <- param()
                     count$state0_p2 <- state0()
                     count$nsteps_p2 <- nsteps()
                     count$scale_p2 <- input$scale
                     count$logScale_p2 <- input$logScale
                     count$plotvars_p2 <- input$plotvars
                     count$ndays_p2 <- input$ndays
                     count$startDate_p2 <- input$dateRange[1]
                     count$popSize_p2 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 3)
                 }

                 if(input$reportFigs==3) {
                     extra$plot3 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot3", value = TRUE)
                     count$nPlots <- 3
                     count$param_p3 <- param()
                     count$state0_p3 <- state0()
                     count$nsteps_p3 <- nsteps()
                     count$scale_p3 <- input$scale
                     count$logScale_p3 <- input$logScale
                     count$plotvars_p3 <- input$plotvars
                     count$ndays_p3 <- input$ndays
                     count$startDate_p3 <- input$dateRange[1]
                     count$popSize_p3 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 4)
                 }

                 if(input$reportFigs==4) {
                     extra$plot4 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot4", value = TRUE)
                     count$nPlots <- 4
                     count$param_p4 <- param()
                     count$state0_p4 <- state0()
                     count$nsteps_p4 <- nsteps()
                     count$scale_p4 <- input$scale
                     count$logScale_p4 <- input$logScale
                     count$plotvars_p4 <- input$plotvars
                     count$ndays_p4 <- input$ndays
                     count$startDate_p4 <- input$dateRange[1]
                     count$popSize_p4 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 5)
                 }

                 if(input$reportFigs==5) {
                     extra$plot5 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot5", value = TRUE)
                     count$nPlots <- 5
                     count$param_p5 <- param()
                     count$state0_p5 <- state0()
                     count$nsteps_p5 <- nsteps()
                     count$scale_p5 <- input$scale
                     count$logScale_p5 <- input$logScale
                     count$plotvars_p5 <- input$plotvars
                     count$ndays_p5 <- input$ndays
                     count$startDate_p5 <- input$dateRange[1]
                     count$popSize_p5 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 6)
                 }

                 if(input$reportFigs==6) {
                     extra$plot6 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot6", value = TRUE)
                     count$nPlots <- 6
                     count$param_p6 <- param()
                     count$state0_p6 <- state0()
                     count$nsteps_p6 <- nsteps()
                     count$scale_p6 <- input$scale
                     count$logScale_p6 <- input$logScale
                     count$plotvars_p6 <- input$plotvars
                     count$ndays_p6 <- input$ndays
                     count$startDate_p6 <- input$dateRange[1]
                     count$popSize_p6 <- popcounter()
                     updateRadioButtons(session, "reportFigs", selected = 1)
                 }
             })

# Render chosen plots for the report
output$extplot1 <- renderggiraph(extra$plot1)
output$extplot2 <- renderggiraph(extra$plot2)
output$extplot3 <- renderggiraph(extra$plot3)
output$extplot4 <- renderggiraph(extra$plot4)
output$extplot5 <- renderggiraph(extra$plot5)
output$extplot6 <- renderggiraph(extra$plot6)

output$nPlots <- reactive(count$nPlots)
outputOptions(output, "nPlots", suspendWhenHidden = FALSE)

## Prepare report
## Download the report
output$downloadReport <- downloadHandler(
        filename = function(){
            name <- ifelse(input$reportname=="", "my-report", input$reportname)
            name1 <- ifelse(input$datelab==TRUE,
                            paste0(Sys.Date(), "-", gsub(" ", "-", name)),
                            gsub(" ", "-", name))
            paste0(name1, '.html')
            },

        content = function(file) {
            withProgress(message = "Compiling your report - this takes a moment. Don't touch your face", {
            src <- normalizePath('report.Rmd')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)


            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            out <- rmarkdown::render('report.Rmd', html_document())
            file.rename(out, file)
            })
        }
)

}) # Closes Shiny Server
