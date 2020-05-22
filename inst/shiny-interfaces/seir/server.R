###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# Extensions to the Doherty model
# incl. Time varying interventions

###################################################################################


shinyServer(function(session, input, output) {


# Add up the different populations and display the population count
popcounter <- reactive({
    sum(input$s_num, input$e_num, input$i_num, input$r_num)
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

# Update comparison country based on initial country choice. (Uncomment to apply)
# observe(updateSelectizeInput(session, "compCountries", selected = input$countryChoice))


# Visualise the contact matrix as a heatmap
ageMat <- reactive({
  req(input$countryChoice)
  import_contact_matrix(input$countryChoice, setting="general")
  })


output$heatMap <- renderPlotly({
  heatmaply(round(ageMat(), digits=1), dendrogram = FALSE, hide_colorbar = TRUE, margins = c(2,2,2,2), fontsize_row=6, fontsize_col=6,
            titleX = FALSE, titleY = FALSE, label_names = c('Age group Y', 'Age group X', 'Value'))
})

# Age distribution based on selected country
ageDist <- reactive({
  req(input$countryChoice)
  import_age_distribution(input$countryChoice)

})

## Custom distribution of E, I, R populations

# E
expDist <- reactive({
  req(input$scale_exp, input$shape_exp)
  dgamma(seq(1:16), shape = input$shape_exp, scale = input$scale_exp)/sum(dgamma(seq(1:16), shape = input$shape_exp, scale = input$scale_exp))
})

# I
infDist <- reactive({
  req(input$scale_inf, input$shape_inf)
  dgamma(seq(1:16), shape = input$shape_inf, scale = input$scale_inf)/sum(dgamma(seq(1:16), shape = input$shape_inf, scale = input$scale_inf))
})

# R
recDist <- reactive({
  req(input$scale_rec, input$shape_rec)
  dgamma(seq(1:16), shape = input$shape_rec, scale = input$scale_rec)/sum(dgamma(seq(1:16), shape = input$shape_rec, scale = input$scale_rec))
})

dfAgeDist <- reactive({
                req(input$countryChoice)

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

    nodes <- data.frame(id = 1:4,

        # add labels on nodes
        label = c('S', 'E', 'I', 'R'),

        # size adding value
        size = c(rep(20, 4)),

        # Hierarchical level
        level = rep(0,4),

        # Group
        group = c("Susceptible", "Exposed", "Infectious", "Recovered"),

        # control shape of nodes
        shape = c(rep("square", 4)),

        font.color = c(rep("grey", 4)),

        # Don't need physics
        physics = rep(FALSE, 4),

        # tooltip (html or character), when the mouse is above
        title = c(HTML(paste0("S", br(), 'Susceptible', br(), "N =", formatC(input$s_num, format='d', big.mark=","))),
                  HTML(paste0("E", br(), "Exposed", br(), "N =", formatC(input$e_num, format='d', big.mark=","))),
                  HTML(paste0("I", br(), "Infectious", br(), "N =", formatC(input$i_num, format='d', big.mark=","))),
                  HTML(paste0("R", br(), "Recovered", br(), "N =", formatC(input$r_num, format='d', big.mark=",")))
        )

    )

    edges <- data.frame(
              from = seq(1,3),
              to = seq(2,4),
              id = LETTERS[seq(1:3)],
              dashes = c(rep(FALSE,3)
                   ),

        # Label text
        label = c(
            round(param()$pt, digits=3),
            round(default$sigma(), digits=3),
            round(default$gamma(), digits=3)
            ),

        # Label colour
        font.color = c(rep("grey", 3)),

        # Hover titles\
        title = c(
          HTML('&beta;I/N'),
          HTML('&sigma;'),
          HTML('&gamma;')
        )
  )

    visNetwork(nodes, edges, main = "SEIR Model", submain = 'Compartmental diagram') %>%
        visHierarchicalLayout(direction = "UD", levelSeparation = 40) %>%
        visEdges(arrows = "to") %>%
        visGroups(groupname = "Susceptible", color = "#2daae2") %>%
        visGroups(groupname = "Exposed", color = "#ff8200") %>%
        visGroups(groupname = "Infectious", color = "#ff635d") %>%
        visGroups(groupname = "Recovered", color = "#1ac987") %>%
        visLegend(enabled = FALSE) %>%
        visInteraction(hover = TRUE) %>%
        visPhysics(stabilization = FALSE) %>%
        visEvents(startStabilizing = "function() {
            this.moveTo({scale:0.8})}")
})


###################################################################
### Modal dialogues to distribute compartments across age bands ###
###################################################################

# Exposed
observe({
  if(input$e_num_dist == "Custom") {isolate(
  showModal(modalDialog(easyClose = TRUE,
    title = "Age distribution of exposed individuals",
    fluidRow(
      column(width=5,
             renderggiraph({

               p1 <- dfAgeDist() %>%
                 mutate(exp = input$e_num*expDist()) %>%
                 ggplot(aes(x=x, y=exp, fill=exp)) +
                 geom_col_interactive(aes(tooltip = paste0(x, "-", x+4, " years, N = ", round(exp, digits=0)))) +

                 labs(x="Age group", y = "Count") +
                 scale_fill_viridis() +
                 theme(legend.position = "none")

               z <- girafe(code = print(p1))
               girafe_options(z, opts_tooltip(zindex = 9999)) # Because of this issue: https://github.com/davidgohel/ggiraph/issues/150

             })
      ),

      column(width = 3,
             radioButtons("opts_exp", "Presets:",
                          selected = 3,
                          choices = list("Younger children" = 1,
                                         "Young adults" = 2,
                                         "Middle aged" = 3,
                                         "Older adults" = 4))
      ),

      column(width=4,
             sliderInput("shape_exp", "Custom shape:", min=0.1, max=10, step=.1, value = default$shape_exp(), animate = TRUE),
             sliderInput("scale_exp", "Custom scale:", min=0.1, max=3, step=.1, value = default$scale_exp(), animate = TRUE)
      )
    ) # Closes fluidRow
  ))
)}})


# Infectious
observe({
  if(input$i_num_dist == "Custom") {isolate(
    showModal(modalDialog(easyClose = TRUE,
      title = "Age distribution of infectious individuals",
      fluidRow(
        column(width=5,
               renderggiraph({

                 p1 <- dfAgeDist() %>%
                   mutate(inf = input$i_num*infDist()) %>%
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
                            selected = 3,
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
    ))
  )}})


# Recovered
observe({
  if(input$r_num_dist == "Custom") {isolate(
    showModal(modalDialog(easyClose = TRUE,
      title = "Age distribution of exposed individuals",
      fluidRow(
        column(width=5,
               renderggiraph({

                 p1 <- dfAgeDist() %>%
                   mutate(rec = input$r_num*recDist()) %>%
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
                            selected = 3,
                            choices = list("Younger children" = 1,
                                           "Young adults" = 2,
                                           "Middle aged" = 3,
                                           "Older adults" = 4))
        ),

        column(width=4,
               sliderInput("shape_rec", "Custom shape:", min=0.1, max=10, step=.1, value = default$shape_rec(), animate = TRUE),
               sliderInput("scale_rec", "Custom scale:", min=0.1, max=3, step=.1, value = default$scale_rec(), animate = TRUE)
        )
      ) # Closes fluidRow
    ))
  )}})


## Sparklines for age distributions
output$sparklineAge_e <- sparkline::renderSparkline({
  sparkline(round(e_num_vec(), digits=0), chartRangeMin = 0)
})

output$sparklineAge_i <- sparkline::renderSparkline({
  sparkline(round(i_num_vec(), digits=0), chartRangeMin = 0)
})

output$sparklineAge_r <- sparkline::renderSparkline({
  sparkline(round(r_num_vec(), digits=0), chartRangeMin = 0)
})






############################################################################
                    ### Set parameter default values ###

# This is necesary because a lot of the parameter inputs are
# placed in dialogue boxes and consequently are NULL until the
# user opens the dialogue box. The default values are the values
# given to an input parameter until the user chooses something else
############################################################################

default <- reactiveValues()

# Initial conditions
default$s_num <- reactive(ifelse(is.null(input$s_num), import_total_population(input$countryChoice) - default$e_num() - default$i_num() - default$r_num(), input$s_num))
default$e_num <- reactive(ifelse(is.null(input$e_num), 70, input$e_num))
default$i_num <- reactive(ifelse(is.null(input$i_num), 30, input$i_num))
default$r_num <- reactive(ifelse(is.null(input$r_num), 0, input$r_num))

default$i_num_dist <- reactive(ifelse(is.null(input$i_num_dist), "Uniform", input$i_num_dist))
default$e_num_dist <- reactive(ifelse(is.null(input$e_num_dist), "Uniform", input$e_num_dist))
default$r_num_dist <- reactive(ifelse(is.null(input$r_num_dist), "Uniform", input$r_num_dist))

# Model parameters
default$r0 <- reactive(ifelse(is.null(input$r0), 2.5, input$r0))
default$sigma <- reactive(ifelse(is.null(input$sigmainv), 1/10, 1/input$sigmainv))
default$gamma <- reactive(ifelse(is.null(input$gammainv), 1/10, 1/input$gammainv))

# Country choices
default$countryAgeMat <- reactive(ifelse(is.null(input$countryAgeMat), "Australia", input$countryAgeMat))
default$settingAgeMat <- reactive(ifelse(is.null(input$settingAgeMat), "general", input$settingAgeMat))
default$countryAgeDist <- reactive(ifelse(is.null(input$countryAgeDist), "Australia", input$countryAgeDist))


# Calculated parameters (Defaults to older adults)
default$opts_exp <- reactive(ifelse(is.null(input$opts_exp), 3, input$opts_exp))
default$opts_inf <- reactive(ifelse(is.null(input$opts_inf), 3, input$opts_inf))
default$opts_rec <- reactive(ifelse(is.null(input$opts_rec), 3, input$opts_rec))

# Shape of custom age distribution when distributing E, I and R numbers across age groups
default$shape_exp <- reactive(ifelse(is.null(input$shape_exp), 1, input$shape_exp))
default$scale_exp <- reactive(ifelse(is.null(input$scale_exp), 2, input$scale_exp))
default$shape_inf <- reactive(ifelse(is.null(input$shape_inf), 1, input$shape_inf))
default$scale_inf <- reactive(ifelse(is.null(input$scale_inf), 2, input$scale_inf))
default$shape_rec <- reactive(ifelse(is.null(input$shape_rec), 1, input$shape_rec))
default$scale_rec <- reactive(ifelse(is.null(input$scale_rec), 2, input$scale_rec))

# Update sliders based on presets

# Exposed
observeEvent(input$opts_exp, {
  if(input$opts_exp==1) {
    updateSliderInput(session, "shape_exp", value = 1)
    updateSliderInput(session, "scale_exp", value = 2)
  }
  if(input$opts_exp==2) {
    updateSliderInput(session, "shape_exp", value = 5)
    updateSliderInput(session, "scale_exp", value = 1.3)
  }
  if(input$opts_exp==3) {
    updateSliderInput(session, "shape_exp", value = 9)
    updateSliderInput(session, "scale_exp", value = 1.3)
  }
  if(input$opts_exp==4) {
    updateSliderInput(session, "shape_exp", value = 10)
    updateSliderInput(session, "scale_exp", value = 2.5)
  }
})

# Infectious
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

# Recovered
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


###########################################################################
              ### Summary plot of interventions
###########################################################################

output$intSummary <- renderggiraph({

  ln <- length(fxGeneral_t()[[2]])
  df <- data.frame(

      date = c(
        as.Date(seq(1,ln) + input$dateRange[1], origin = "1970-01-01")
      ),

      target = c(
        rep("Transmission probability (General)", ln),
        rep("Social contacts (General)", ln),
        rep("Social contacts (School)", ln),
        rep("Social contacts (Work)", ln),
        rep("Social contacts (Home)", ln)
      ),

      int = c(
        fxGeneral_t()[[2]],
        fxGeneral_c()[[2]],
        fxSchool_c()[[2]],
        fxWork_c()[[2]],
        fxHome_c()[[2]]
      ),

      include = c(
        rep(TRUE, ln),
        rep(TRUE, ln),
        rep("school" %in% input$intSetting_c, ln),
        rep("work" %in% input$intSetting_c, ln),
        rep("home" %in% input$intSetting_c, ln)
      )

    ) %>%
    mutate(tt = sprintf("%s<br/><strong>%s</strong>", date, int) %>% lapply(htmltools::HTML)) %>%
    filter(include)

intCols <- c("Transmission probability (General)" = "#3f61c4",
             "Social contacts (General)" = "#ff635d",
             "Social contacts (School)" = "#2daae2",
             "Social contacts (Work)" = "#fa91b6",
             "Social contacts (Home)" = "#1ac987")

intBreaks <- c("Transmission probability (General)",
             "Social contacts (General)",
             "Social contacts (School)",
             "Social contacts (Work)",
             "Social contacts (Home)")

p <- ggplot(data=df, aes(x=date, y=int, group=target)) +
  geom_line(aes(color=target), size=2, alpha=.2) +
  geom_point_interactive(aes(color=target, tooltip = tt)) +
  scale_x_date(date_labels="%d%b") +
  coord_cartesian(xlim = c(input$dateRange[1], input$dateRange[2]), ylim = c(0, 1)) +
  scale_colour_manual(values = intCols, breaks = intBreaks) +
  labs(x="Date", y="Value relative to pre-pandemic levels") +
  guides(colour = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggiraph::girafe(code = print(p))
})



############################################################################
              ### Define age distribution based on user choices ###
############################################################################

s_num_vec <- reactive({
  ageDist()*input$s_num - (e_num_vec() + i_num_vec() + r_num_vec())
})

e_num_vec <- reactive({
  if(input$e_num_dist == "Uniform") {ageDist()*input$e_num}
  else if(input$e_num_dist == "Custom") {expDist()*input$e_num}
})

i_num_vec <- reactive({
  if(input$i_num_dist == "Uniform") {ageDist()*input$i_num}
  else if(input$i_num_dist == "Custom") {infDist()*input$i_num}
})

r_num_vec <- reactive({
  if(input$r_num_dist == "Uniform") {ageDist()*input$r_num}
  else if(input$r_num_dist == "Custom") {recDist()*input$r_num}
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
    seir_c_state0(
      S = s_num_vec(),
      E = e_num_vec(),
      I = i_num_vec(),
      R = r_num_vec()
  )
})

### Model parameters
param <- reactive({
   seir_c_param(R0 = default$r0(),
                 sigma = default$sigma(),
                 gamma = default$gamma(),
                 cm = cmList(),
                 dist = ageDist(),
                 contact_intervention = intList_c(),
                 transmission_intervention = fxGeneral_t()
                 )
})


# Run the model when the button is clicked
model <- eventReactive(input$runMod, {
    isolate(
        simulate_seir_c(t = nsteps(),
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
              ndays=input$ndays,
              xtraC=input$compCountries,
              xtraP=input$compProvince)
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


# Interactively update number susceptibles based on choice of country
observe({
req(input$countryChoice)
updateNumericInput(session, "s_num", value = import_total_population(input$countryChoice))
})

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
