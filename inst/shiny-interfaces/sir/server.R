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

# Update choice of province, based on choice of country
provinceChoice <- reactive({
  covid19_data %>% filter(Country.Region==input$compCountries) %>% select(Province.State) %>% distinct()
})
observe(updateSelectizeInput(session, "compProvince", choices = provinceChoice()))

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
  ifelse(input$countryAgeMat %in% covoid::age_distributions_un(), input$countryAgeMat, "Not found, please choose manually")
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
            this.moveTo({scale:1.5})}") %>%
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
                       selectizeInput("countryAgeDist", "Age distribution", choices = c(covoid::age_distributions_un(), "Not found, please choose manually"), selected = default$countryAgeDist(), multiple = FALSE, options = NULL),
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

# This is necesary because a lot of the parameter inputs are
# placed in dialogue boxes and consequently are NULL until the
# user opens the dialogue box. The default values are the values
# given to an input parameter until the user chooses something else
############################################################################

default <- reactiveValues()

# Initial conditions
default$s_num <- reactive(ifelse(is.null(input$s_num), import_total_population(default$countryAgeMat()) - default$i_num() - default$r_num(), input$s_num))
default$i_num <- reactive(ifelse(is.null(input$i_num), 30, input$i_num))
default$r_num <- reactive(ifelse(is.null(input$r_num), 0, input$r_num))

default$i_num_dist <- reactive(ifelse(is.null(input$i_num_dist), "Uniform", input$i_num_dist))
default$r_num_dist <- reactive(ifelse(is.null(input$r_num_dist), "Uniform", input$r_num_dist))

# Model parameters
default$r0 <- reactive(ifelse(is.null(input$r0), 2.5, input$r0))
default$gamma <- reactive(ifelse(is.null(input$gammainv), 1/10, 1/input$gammainv))

# Country choices
default$countryAgeMat <- reactive(ifelse(is.null(input$countryAgeMat), "Australia", input$countryAgeMat))
default$settingAgeMat <- reactive(ifelse(is.null(input$settingAgeMat), "general", input$settingAgeMat))
default$countryAgeDist <- reactive(ifelse(is.null(input$countryAgeDist), "Australia", input$countryAgeDist))


# Calculated parameters
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

# Model parameters (quarantined population)
output$summary2 <- renderText({
    HTML(paste(icon("sliders-h"), em("Transition params"), hr(),
               code(HTML(paste0("R", tags$sub("0")))), round(default$r0(), digits = 3), br(),
               code(HTML(paste0("&gamma;"))), round(default$gamma(), digits = 3)

    ))
})


output$summary3 <- renderText({
  HTML(
    paste(icon("handshake"), em("Transmission probability"), hr(),
      sparkline::spk_chr(fxGeneral_t()[[2]], chartRangeMin = 0, chartRangeMin = 1), "&mdash;", "General"
      )
    )
})

output$summary4 <- renderText({
  HTML(
    paste(icon("users"), em("Social contacts"), hr(),
          sparkline::spk_chr(fxGeneral_c()[[2]], chartRangeMin = 0, chartRangeMin = 1), "&mdash;", "General", br(),
          sparkline::spk_chr(fxSchool_c()[[2]], chartRangeMin = 0, chartRangeMin = 1), "&mdash;", "School", br(),
          sparkline::spk_chr(fxWork_c()[[2]], chartRangeMin = 0, chartRangeMin = 1), "&mdash;", "Work", br(),
          sparkline::spk_chr(fxHome_c()[[2]], chartRangeMin = 0, chartRangeMin = 1), "&mdash;", "Home"
    )
  )
})



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
int$dfSchool_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfWork_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfHome_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfGeneral_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfSchool_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfWork_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfHome_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())
int$dfGeneral_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())


# Place holder for values returned when user hovers
int$hvSchool_c <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvWork_c <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvHome_c <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvGeneral_c <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvSchool_t <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvWork_t <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvHome_t <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )
int$hvGeneral_t <- data.frame(x = numeric(), y = numeric(), lab = character(), fulllab = character() )

# Create plot
output$intSchool_c <- renderPlot(clickrPlot(int$dfSchool_c, input$dateRange[1], input$dateRange[2]))
output$intWork_c <- renderPlot(clickrPlot(int$dfWork_c, input$dateRange[1], input$dateRange[2]))
output$intHome_c <- renderPlot(clickrPlot(int$dfHome_c, input$dateRange[1], input$dateRange[2]))
output$intGeneral_c <- renderPlot(clickrPlot(int$dfGeneral_c, input$dateRange[1], input$dateRange[2]))
output$intSchool_t <- renderPlot(clickrPlot(int$dfSchool_t, input$dateRange[1], input$dateRange[2]))
output$intWork_t <- renderPlot(clickrPlot(int$dfWork_t, input$dateRange[1], input$dateRange[2]))
output$intHome_t <- renderPlot(clickrPlot(int$dfHome_t, input$dateRange[1], input$dateRange[2]))
output$intGeneral_t <- renderPlot(clickrPlot(int$dfGeneral_t, input$dateRange[1], input$dateRange[2]))

# Create Table summary
output$reviewSchool_c <- renderDT(clickrTable(int$dfSchool_c, input$dateRange[1]))
output$reviewWork_c <- renderDT(clickrTable(int$dfWork_c, input$dateRange[1]))
output$reviewHome_c <- renderDT(clickrTable(int$dfHome_c, input$dateRange[1]))
output$reviewGeneral_c <- renderDT(clickrTable(int$dfGeneral_c, input$dateRange[1]))
output$reviewSchool_t <- renderDT(clickrTable(int$dfSchool_t, input$dateRange[1]))
output$reviewWork_t <- renderDT(clickrTable(int$dfWork_t, input$dateRange[1]))
output$reviewHome_t <- renderDT(clickrTable(int$dfHome_t, input$dateRange[1]))
output$reviewGeneral_t <- renderDT(clickrTable(int$dfGeneral_t, input$dateRange[1]))

# Update labels that are hovered
observeEvent(input$intSchool_hover_c, {int$hvSchool_c <- clickrHover(input$intSchool_hover_c)})
observeEvent(input$intWork_hover_c, {int$hvWork_c <- clickrHover(input$intWork_hover_c)})
observeEvent(input$intHome_hover_c, {int$hvHome_c <- clickrHover(input$intHome_hover_c)})
observeEvent(input$intGeneral_hover_c, {int$hvGeneral_c <- clickrHover(input$intGeneral_hover_c)})
observeEvent(input$intSchool_hover_t, {int$hvSchool_t <- clickrHover(input$intSchool_hover_t)})
observeEvent(input$intWork_hover_t, {int$hvWork_t <- clickrHover(input$intWork_hover_t)})
observeEvent(input$intHome_hover_t, {int$hvHome_t <- clickrHover(input$intHome_hover_t)})
observeEvent(input$intGeneral_hover_t, {int$hvGeneral_t <- clickrHover(input$intGeneral_hover_t)})

# Add points that are clicked
observeEvent(input$intSchool_click_c, {int$dfSchool_c <- addPoint(int$dfSchool_c, input$intSchool_click_c$x, input$intSchool_click_c$y)})
observeEvent(input$intWork_click_c, {int$dfWork_c <- addPoint(int$dfWork_c, input$intWork_click_c$x, input$intWork_click_c$y)})
observeEvent(input$intHome_click_c, {int$dfHome_c <- addPoint(int$dfHome_c, input$intHome_click_c$x, input$intHome_click_c$y)})
observeEvent(input$intGeneral_click_c, {int$dfGeneral_c <- addPoint(int$dfGeneral_c, input$intGeneral_click_c$x, input$intGeneral_click_c$y)})
observeEvent(input$intSchool_click_t, {int$dfSchool_t <- addPoint(int$dfSchool_t, input$intSchool_click_t$x, input$intSchool_click_t$y)})
observeEvent(input$intWork_click_t, {int$dfWork_t <- addPoint(int$dfWork_t, input$intWork_click_t$x, input$intWork_click_t$y)})
observeEvent(input$intHome_click_t, {int$dfHome_t <- addPoint(int$dfHome_t, input$intHome_click_t$x, input$intHome_click_t$y)})
observeEvent(input$intGeneral_click_t, {int$dfGeneral_t <- addPoint(int$dfGeneral_t, input$intGeneral_click_t$x, input$intGeneral_click_t$y)})

# Remove points that are double clicked
observeEvent(input$intSchool_dblclick_c, {if(nrow(int$dfSchool_c)) {int$dfSchool_c <- dropPoint(int$dfSchool_c, input$intSchool_dblclick_c$x, input$intSchool_dblclick_c$y)}})
observeEvent(input$intWork_dblclick_c, {if(nrow(int$dfWork_c)) {int$dfWork_c <- dropPoint(int$dfWork_c, input$intWork_dblclick_c$x, input$intWork_dblclick_c$y)}})
observeEvent(input$intHome_dblclick_c, {if(nrow(int$dfHome_c)) {int$dfHome_c <- dropPoint(int$dfHome_c, input$intHome_dblclick_c$x, input$intHome_dblclick_c$y)}})
observeEvent(input$intGeneral_dblclick_c, {if(nrow(int$dfGeneral_c)) {int$dfGeneral_c <- dropPoint(int$dfGeneral_c, input$intGeneral_dblclick_c$x, input$intGeneral_dblclick_c$y)}})
observeEvent(input$intSchool_dblclick_t, {if(nrow(int$dfSchool_t)) {int$dfSchool_t <- dropPoint(int$dfSchool_t, input$intSchool_dblclick_t$x, input$intSchool_dblclick_t$y)}})
observeEvent(input$intWork_dblclick_t, {if(nrow(int$dfWork_t)) {int$dfWork_t <- dropPoint(int$dfWork_t, input$intWork_dblclick_t$x, input$intWork_dblclick_t$y)}})
observeEvent(input$intHome_dblclick_t, {if(nrow(int$dfHome_t)) {int$dfHome_t <- dropPoint(int$dfHome_t, input$intHome_dblclick_t$x, input$intHome_dblclick_t$y)}})
observeEvent(input$intGeneral_dblclick_t, {if(nrow(int$dfGeneral_t)) {int$dfGeneral_t <- dropPoint(int$dfGeneral_t, input$intGeneral_dblclick_t$x, input$intGeneral_dblclick_t$y)}})

## Remove last row on actionButton click
observeEvent(input$undoSchool_c, {int$dfSchool_c <- undo(int$dfSchool_c)})
observeEvent(input$undoWork_c, {int$dfWork_c <- undo(int$dfWork_c)})
observeEvent(input$undoHome_c, {int$dfHome_c <- undo(int$dfHome_c)})
observeEvent(input$undoGeneral_c, {int$dfGeneral_c <- undo(int$dfGeneral_c)})
observeEvent(input$undoSchool_t, {int$dfSchool_t <- undo(int$dfSchool_t)})
observeEvent(input$undoWork_t, {int$dfWork_t <- undo(int$dfWork_t)})
observeEvent(input$undoHome_t, {int$dfHome_t <- undo(int$dfHome_t)})
observeEvent(input$undoGeneral_t, {int$dfGeneral_t <- undo(int$dfGeneral_t)})

## Reset on click
observeEvent(input$resetSchool_c, {int$dfSchool_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetWork_c, {int$dfWork_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetHome_c, {int$dfHome_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetGeneral_c, {int$dfGeneral_c <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetSchool_t, {int$dfSchool_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetWork_t, {int$dfWork_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetHome_t, {int$dfHome_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})
observeEvent(input$resetGeneral_t, {int$dfGeneral_t <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric(), lab = character())})

# Fill in the timeseries
nmsteps <-reactive(nsteps()-1)
fxSchool_c <- reactive({if(nrow(int$dfSchool_c)>=1){fillTime(int$dfSchool_c, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxWork_c <- reactive({if(nrow(int$dfWork_c)>=1){fillTime(int$dfWork_c, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxHome_c <- reactive({if(nrow(int$dfHome_c)>=1){fillTime(int$dfHome_c, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxGeneral_c <- reactive({if(nrow(int$dfGeneral_c)>=1){fillTime(int$dfGeneral_c, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxSchool_t <- reactive({if(nrow(int$dfSchool_t)>=1){fillTime(int$dfSchool_t, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxWork_t <- reactive({if(nrow(int$dfWork_t)>=1){fillTime(int$dfWork_t, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxHome_t <- reactive({if(nrow(int$dfHome_t)>=1){fillTime(int$dfHome_t, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})
fxGeneral_t <- reactive({if(nrow(int$dfGeneral_t)>=1){fillTime(int$dfGeneral_t, input$dateRange[1], input$dateRange[2])} else data.frame(time = seq(from=0, to=nmsteps(), by=1), reduce = rep(1,nsteps()))})

# Spark line for intervention
output$sparklineSchool_c <- renderSparkline(sparkline(fxSchool_c()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineWork_c <- renderSparkline(sparkline(fxWork_c()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineHome_c <- renderSparkline(sparkline(fxHome_c()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineGeneral_c <- renderSparkline(sparkline(fxGeneral_c()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineSchool_t <- renderSparkline(sparkline(fxSchool_t()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineWork_t <- renderSparkline(sparkline(fxWork_t()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineHome_t <- renderSparkline(sparkline(fxHome_t()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparklineGeneral_t <- renderSparkline(sparkline(fxGeneral_t()[[2]], chartRangeMin = 0, chartRangeMin = 1))
output$sparkTest <- renderSparkline(sparkline(seq(1:10)))

# Hover text
output$tooltipSchool_c <- renderText(paste(int$hvSchool_c$fulllab))
output$tooltipWork_c <- renderText(paste(int$hvWork_c$fulllab))
output$tooltipHome_c <- renderText(paste(int$hvHome_c$fulllab))
output$tooltipGeneral_c <- renderText(paste(int$hvGeneral_c$fulllab))
output$tooltipSchool_t <- renderText(paste(int$hvSchool_t$fulllab))
output$tooltipWork_t <- renderText(paste(int$hvWork_t$fulllab))
output$tooltipHome_t <- renderText(paste(int$hvHome_t$fulllab))
output$tooltipGeneral_t <- renderText(paste(int$hvGeneral_t$fulllab))


### Store contact matrices

cmSchool <- reactive({
  req(default$countryAgeMat())

  if("school" %in% input$intSetting_c){
  import_contact_matrix(default$countryAgeMat(), setting="school")
  }
  else NULL
})

cmWork <- reactive({
  req(default$countryAgeMat())

  if("work" %in% input$intSetting_c){
  import_contact_matrix(default$countryAgeMat(), setting="work")
  }
  else NULL
})

cmHome <- reactive({
  req(default$countryAgeMat())

  if("home" %in% input$intSetting_c){
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

# output$test1 <- reactive(paste("General:", round(sum(cmList_t()[["general"]]), digits=0)))

### Prepare contact intervention matrix list
intList_c <- reactive({

  noInt <- data.frame(
    time = seq(from=0, to = nsteps()-1),
    reduce = rep(1, nsteps())
    )

  intSchool <- if(is.null(fxSchool_c())) noInt
                else fxSchool_t()

  intWork <- if(is.null(fxWork_c())) noInt
  else fxWork_c()

  intHome <- if(is.null(fxHome_c())) noInt
  else fxHome_c()

  intGeneral <- if(is.null(fxGeneral_c())) noInt
  else fxGeneral_c()

  out <- list(school = intSchool, work = intWork, home = intHome, general = intGeneral)
  if(is.null(cmSchool())) out["school"] <- NULL
  if(is.null(cmWork())) out["work"] <- NULL
  if(is.null(cmHome())) out["home"] <- NULL
  out

})

### Prepare contact intervention matrix list for transmission interventions
intList_t <- reactive({

  noInt <- data.frame(
    time = seq(from=0, to = nsteps()-1),
    reduce = rep(.32, nsteps())
  )

  intSchool <- if(is.null(fxSchool_t())) noInt
  else fxSchool_t()

  intWork <- if(is.null(fxWork_t())) noInt
  else fxWork_t()

  intHome <- if(is.null(fxHome_t())) noInt
  else fxHome_t()

  intGeneral <- if(is.null(fxGeneral_t())) noInt
  else fxGeneral_t()

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
                 contact_intervention = intList_c(),
                 transmission_intervention = fxGeneral_t()
                 )
})

# output$testa <- renderText({ paste(fxGeneral_t()) })

# Run the model when the button is clicked
model <- eventReactive(input$runMod, {
    isolate(
        simulate_sir_c(t = nsteps(),
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

# output$testx <- renderText(input$compCountries)

### Model summary
mainPlot <- reactive({
  plotResults(df=mod_df(),
              scale=input$scale,
              logScale=input$logScale,
              plotvars=input$plotvars,
              ndays=input$ndays,
              xtraC=input$compCountries,
              xtraP=input$compProvince)
  })

output$plot <- renderggiraph(mainPlot())

# Make static version the plot for download
staticPlot <- reactive({
  plotStaticResults(df=mod_df(),
                    scale=input$scale,
                    logScale=input$logScale,
                    plotvars=input$plotvars,
                    ndays=input$ndays
                    )
  })

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

output$testa <- renderText(input$compCountries=="")

## Prepare animation

# Preview
output$animation_preview <- renderggiraph(plotResults(df=mod_df(), scale=input$scale_a, logScale=input$logScale_a,
                                                      plotvars=input$plotvars_a, ndays=input$ndays_a,
                                                      xtraC = input$compCountries, xtraP = input$compProvince
                                                      )
                                          )

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
                     extra$plot1 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot1", value = TRUE)
                     count$nPlots <- 1
                     count$param_p1 <- param()
                     count$model_p1 <- model()
                     count$state0_p1 <- state0()
                     count$nsteps_p1 <- nsteps()
                     count$scale_p1 <- input$scale
                     count$logScale_p1 <- input$logScale
                     count$plotvars_p1 <- input$plotvars
                     count$ndays_p1 <- input$ndays
                     count$xtraC_p1 <- input$compCountries
                     count$xtraP_p1 <- input$compProvince
                     count$dateRange_p1 <- input$dateRange
                     count$popSize_p1 <- popcounter()
                     count$intGeneral_t_p1 = fxGeneral_t()
                     count$intGeneral_c_p1 = fxGeneral_c()
                     count$intSchool_c_p1 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p1 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p1 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
                     updateRadioButtons(session, "reportFigs", selected = 2)
                 }

                 if(input$reportFigs==2) {
                     extra$plot2 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot2", value = TRUE)
                     count$nPlots <- 2
                     count$param_p2 <- param()
                     count$model_p2 <- model()
                     count$state0_p2 <- state0()
                     count$nsteps_p2 <- nsteps()
                     count$scale_p2 <- input$scale
                     count$logScale_p2 <- input$logScale
                     count$plotvars_p2 <- input$plotvars
                     count$ndays_p2 <- input$ndays
                     count$xtraC_p2 <- input$compCountries
                     count$xtraP_p2 <- input$compProvince
                     count$dateRange_p2 <- input$dateRange
                     count$popSize_p2 <- popcounter()
                     count$intGeneral_t_p2 = fxGeneral_t()
                     count$intGeneral_c_p2 = fxGeneral_c()
                     count$intSchool_c_p2 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p2 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p2 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
                     updateRadioButtons(session, "reportFigs", selected = 3)
                 }

                 if(input$reportFigs==3) {
                     extra$plot3 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot3", value = TRUE)
                     count$nPlots <- 3
                     count$param_p3 <- param()
                     count$model_p3 <- model()
                     count$state0_p3 <- state0()
                     count$nsteps_p3 <- nsteps()
                     count$scale_p3 <- input$scale
                     count$logScale_p3 <- input$logScale
                     count$plotvars_p3 <- input$plotvars
                     count$xtraC_p3 <- input$compCountries
                     count$xtraP_p3 <- input$compProvince
                     count$ndays_p3 <- input$ndays
                     count$dateRange_p3 <- input$dateRange
                     count$popSize_p3 <- popcounter()
                     count$intGeneral_t_p3 = fxGeneral_t()
                     count$intGeneral_c_p3 = fxGeneral_c()
                     count$intSchool_c_p3 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p3 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p3 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
                     updateRadioButtons(session, "reportFigs", selected = 4)
                 }

                 if(input$reportFigs==4) {
                     extra$plot4 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot4", value = TRUE)
                     count$nPlots <- 4
                     count$param_p4 <- param()
                     count$model_p4 <- model()
                     count$state0_p4 <- state0()
                     count$nsteps_p4 <- nsteps()
                     count$scale_p4 <- input$scale
                     count$logScale_p4 <- input$logScale
                     count$plotvars_p4 <- input$plotvars
                     count$ndays_p4 <- input$ndays
                     count$xtraC_p4 <- input$compCountries
                     count$xtraP_p4 <- input$compProvince
                     count$dateRange_p4 <- input$dateRange
                     count$popSize_p4 <- popcounter()
                     count$intGeneral_t_p4 = fxGeneral_t()
                     count$intGeneral_c_p4 = fxGeneral_c()
                     count$intSchool_c_p4 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p4 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p4 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
                     updateRadioButtons(session, "reportFigs", selected = 5)
                 }

                 if(input$reportFigs==5) {
                     extra$plot5 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot5", value = TRUE)
                     count$nPlots <- 5
                     count$param_p5 <- param()
                     count$model_p5 <- model()
                     count$state0_p5 <- state0()
                     count$nsteps_p5 <- nsteps()
                     count$scale_p5 <- input$scale
                     count$logScale_p5 <- input$logScale
                     count$plotvars_p5 <- input$plotvars
                     count$ndays_p5 <- input$ndays
                     count$xtraC_p5 <- input$compCountries
                     count$xtraP_p5 <- input$compProvince
                     count$dateRange_p5 <- input$dateRange
                     count$popSize_p5 <- popcounter()
                     count$intGeneral_t_p5 = fxGeneral_t()
                     count$intGeneral_c_p5 = fxGeneral_c()
                     count$intSchool_c_p5 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p5 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p5 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
                     updateRadioButtons(session, "reportFigs", selected = 6)
                 }

                 if(input$reportFigs==6) {
                     extra$plot6 <- plotResults(df=mod_df(), scale=input$scale, logScale=input$logScale, plotvars=input$plotvars, ndays=input$ndays, xtraC=input$compCountries, xtraP=input$compProvince)
                     updateMaterialSwitch(session, "toreport", value = FALSE)
                     updateMaterialSwitch(session, "inclPlot6", value = TRUE)
                     count$nPlots <- 6
                     count$param_p6 <- param()
                     count$model_p6 <- model()
                     count$state0_p6 <- state0()
                     count$nsteps_p6 <- nsteps()
                     count$scale_p6 <- input$scale
                     count$logScale_p6 <- input$logScale
                     count$plotvars_p6 <- input$plotvars
                     count$ndays_p6 <- input$ndays
                     count$xtraC_p6 <- input$compCountries
                     count$xtraP_p6 <- input$compProvince
                     count$dateRange_p6 <- input$dateRange
                     count$popSize_p6 <- popcounter()
                     count$intGeneral_t_p6 = fxGeneral_t()
                     count$intGeneral_c_p6 = fxGeneral_c()
                     count$intSchool_c_p6 = if ("school" %in% input$intSetting_c) fxSchool_c() else NULL
                     count$intWork_c_p6 = if ("work" %in% input$intSetting_c) fxWork_c() else NULL
                     count$intHome_c_p6 = if ("home" %in% input$intSetting_c) fxHome_c() else NULL
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
