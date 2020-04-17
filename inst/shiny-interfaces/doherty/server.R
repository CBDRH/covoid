###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# McVernon et al 2020 ("The Doherty Model")

###################################################################################


shinyServer(function(session, input, output) {

# Add up the different populations and display the population count
popcounter <- reactive({
    sum(input$s_num, input$e_num, input$i1_num, input$i2_num, input$iq1_num, input$iq2_num,
        input$h_num, input$hq_num, input$r_num, input$rq_num, input$rh_num, input$rqh_num)
})

# Output the population count
output$popcount = renderText({
    HTML(paste("Population Size: ", formatC(popcounter(), format="d", big.mark=',')))
})

# Output the simulation days
output$simdays = renderText({
    HTML(paste("Population Size: ", formatC(popcounter(), format="d", big.mark=',')))
})


################################################################################################
                        ### Defining the Model Compartments Diagram ###
################################################################################################

output$network_d <- renderVisNetwork({

    nodes <- data.frame(id = 1:15,

        # add labels on nodes
        label = c('S', 'E1', 'E2', 'I1', 'I2', 'M', 'R', 'Rm',
                  'E1q', 'E2q', 'I1q', 'I2q', 'Mq', 'Rq', 'Rmq'),

        # size adding value
        size = rep(10, 15),

        # Hierarchical level
        level = c(0,1,1,1,1,2,1,2,3,3,3,3,4,3,4),

        # Group
        group = c("Susceptible", rep("Latent", 2), rep("Infectious", 2), "Managed", rep("Recovered", 2),
                  rep("Latent", 2), rep("Infectious", 2), "Managed", rep("Recovered", 2)
                  ),

        # control shape of nodes
        shape = c(rep("square", 15)),

        # Don't need physics
        physics = rep(FALSE, 15),

        shadow = c(rep(FALSE,8), rep(TRUE,7)),

        # tooltip (html or character), when the mouse is above
        title = c(HTML(paste0("S", br(), 'Susceptible')),
                  HTML(paste0("E", tags$sub("1"), br(), "Latent period", br(), em("(First stage)"))),
                  HTML(paste0("E", tags$sub("2"), br(), "Latent period", br(), em("(Second stage)"))),
                  HTML(paste0("I", tags$sub("1"), br(), "Infectious period", br(), em("(First stage)"))),
                  HTML(paste0("I", tags$sub("2"), br(), "Infectious period", br(), em("(Second stage)"))),
                  HTML(paste0("M", br(), 'Managed cases from I', tags$sub("1"))),
                  HTML(paste0("R", br(), 'Recovered')),
                  HTML(paste0("R", tags$sub("M"), br(), "Recovered individuals that were managed")),
                  HTML(paste0("E", tags$sub("1"), tags$sup("q"), " [Quarantined]", br(), "Latent period", br(), em("(First stage)"))),
                  HTML(paste0("E", tags$sub("2"), tags$sup("q"), " [Quarantined]", br(), "Latent period", br(), em("(Second stage)"))),
                  HTML(paste0("I", tags$sub("1"), tags$sup("q"), " [Quarantined]", br(), "Infectious period", br(), em("(First stage)"))),
                  HTML(paste0("I", tags$sub("2"), tags$sup("q"), " [Quarantined]", br(), "Infectious period", br(), em("(Second stage)"))),
                  HTML(paste0("M", tags$sup("q"), " [Quarantined]", br(), 'Managed cases from I', tags$sub("1"), tags$sup("q"))),
                  HTML(paste0("R", tags$sup("q"), " [Quarantined]", br(), 'Recovered')),
                  HTML(paste0("R", tags$sub("M"),  tags$sup("q"), " [Quarantined]", br(), "Recovered individuals that were managed"))
        )

    )

    edges <- data.frame(from = c(1, 2, 3, 4, 4, 5, 6, 1, 9, 10, 11, 11, 12, 13),
        to = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        id = LETTERS[seq( from = 1, to = 14 )],
        dashes = c(rep(FALSE,7), rep(TRUE,7)),
        #label = rep(input$alpha, 14),
        title = c(HTML('&lambda;(1-&rho;&sdot;&Theta;<sub>M</sub>)'),
                  HTML('&sigma;<sub>1</sub>'),
                  HTML('&sigma;<sub>2</sub>'),
                  HTML('&gamma;<sub>1</sub>(1 - &alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>1</sub>(&alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>2</sub>'),
                  HTML('&gamma;<sub>2</sub>'),
                  HTML('&lambda;(&rho;&sdot;&Theta;<sub>M</sub>)'),
                  HTML('&sigma;<sub>1</sub><sup>q</sup>'),
                  HTML('&sigma;<sub>2</sub><sup>q</sup>'),
                  HTML('&gamma;<sub>1</sub><sup>q</sup>(1 - &alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>1</sub><sup>q</sup>(&alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>2</sub><sup>q</sup>'),
                  HTML('&gamma;<sub>2</sub><sup>q</sup>')
                  )
    )

    visNetwork(nodes, edges, height = "300px", width = "100%") %>%
        visHierarchicalLayout(direction = "UD", levelSeparation = 80) %>%
        visEdges(arrows = "to") %>%
        visGroups(groupname = "Susceptible", color = "lightblue") %>%
        visGroups(groupname = "Latent", color = "orange") %>%
        visGroups(groupname = "Infectious", color = "red") %>%
        visGroups(groupname = "Managed", color = "pink") %>%
        visGroups(groupname = "Recovered", color = "lightgreen") %>%
        visLegend(width = 0.1, position = "left", main = "Compartment") %>%
        visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
        visInteraction(hover = TRUE) %>%
        visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('node_id', nodes.nodes[0]);
                ;}", selectEdge = "function(edges) {
    Shiny.onInputChange('edge_id', edges.edges[0]);
    ;}")
})

output$test1 <- renderText({paste0(input$node_id)})
output$test2 <- renderText({paste0(input$edge_id)})


################################################################################################
                    ### Set parameters by clicking on a network node ###
################################################################################################

## Set current node on click
setNode <- reactive({
    ifelse(is.null(input$node_id), 0, input$node_id)
})

setEdge <- reactive({
    ifelse(is.null(input$edge_id), 'Z', input$edge_id)
})

observe(
## Nodes

    # S.
    if (setNode()==1) { isolate(
            showModal(modalDialog(
                title = "Susceptible individuals",
                size = 's',
                code("S"),
                helpText("The initial number of susceptible individuals"),
                numericInput("s_num", label=NULL, min=0, value=default$s_num()),
                easyClose = TRUE,
                footer = modalButton("Done")))
        )}

    # E1.
    else if (setNode()==2) { isolate(
        showModal(modalDialog(
            title = "Latent period (first stage)",
            size = 's',
            code(HTML(paste0("E", tags$sub(1)))),
            helpText("Individuals who are infected but not contagious (Stage 1"),
            numericInput("e1_num", NULL, min=0, value=default$e1_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # E2.
    else if (setNode()==3) { isolate(
        showModal(modalDialog(
            title = "Latent period (second stage)",
            size = 's',
            code(HTML(paste0("E", tags$sub(2)))),
            helpText("Individuals who are infected but not contagious (Stage 2)"),
            numericInput("e2_num", NULL, min=0, value=default$e2_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # I1.
    else if (setNode()==4) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage)",
            size = 's',
            code(HTML(paste0("I", tags$sub(1)))),
            helpText("Individuals who are infected and are contagious (First stage)"),
            numericInput("i1_num", NULL, min=0, value=default$i1_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # I2.
    else if (setNode()==5) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage)",
            size = 's',
            code(HTML(paste0("I", tags$sub(2)))),
            helpText("Individuals who are infected but not contagious (Second stage)"),
            numericInput("i2_num", NULL, min=0, value=default$i2_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # M.
    else if (setNode()==6) { isolate(
        showModal(modalDialog(
            title = "Managed Cases",
            size = 's',
            code("M"),
            helpText(HTML(paste0("Managed cases, ascertained upon leaving I", tags$sub(1), " and less infectious than individuals in I", tags$sub(2)))),
            numericInput("m_num", NULL, min=0, value=default$m_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # R.
    else if (setNode()==7) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals",
            size = 's',
            code("R"),
            helpText("Recovered individuals"),
            numericInput("r_num", NULL, min=0, value=default$r_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # Rm.
    else if (setNode()==8) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals (managed)",
            size = 's',
            code(HTML(paste0("R", tags$sub("M")))),
            helpText("Recovered individuals that were managed cases"),
            numericInput("rm_num", NULL, min=0, value=default$rm_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # E1q.
    else if (setNode()==9) { isolate(
        showModal(modalDialog(
            title = "Latent period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("E", tags$sub(1), tags$sup("q") ))),
            helpText("Quarantined individuals who are infected but not contagious (Stage 1)"),
            numericInput("e1q_num", NULL, min=0, value=default$e1q_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # E2q.
    else if (setNode()==10) { isolate(
        showModal(modalDialog(
            title = "latent period (second stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("E", tags$sub(2), tags$sup("q") ))),
            helpText("Quarantined individuals who are infected but not contagious (Stage 2)"),
            numericInput("e2q_num", NULL, min=0, value=default$e2q_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # I1q.
    else if (setNode()==11) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("I", tags$sub(1), tags$sup("q") ))),
            helpText("Individuals who are infected and are contagious (First stage)"),
            numericInput("i1q_num", NULL, min=0, value=default$i1q_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # I2q.
    else if (setNode()==12) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("I", tags$sub(2), tags$sup("q") ))),
            helpText("Individuals who are infected but not contagious (Second stage)"),
            numericInput("i2q_num", NULL, min=0, value=default$i2q_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # Mq.
    else if (setNode()==13) { isolate(
        showModal(modalDialog(
            title = "Managed Cases [Quarantined]",
            size = 's',
            code(HTML(paste0("M", tags$sup("q") ))),
            helpText(HTML(paste0("Managed cases, ascertained upon leaving I", tags$sub(1), " and less infectious than individuals in I", tags$sub(2)))),
            numericInput("mq_num", NULL, min=0, value=default$mq_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # Rq.
    else if (setNode()==14) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals [Quarantined]",
            size = 's',
            code(HTML(paste0("R", tags$sup("q") ))),
            helpText("Quarantined iindividuals that have recovered"),
            numericInput("rq_num", NULL, min=0, value=default$rq_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # Rmq.
    else if (setNode()==15) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals (managed) [Quarantined]",
            size = 's',
            code(HTML(paste0("R", tags$sub("M"),  tags$sup("q")))),
            helpText("Quarantined individuals that were managed cases and have recovered"),
            numericInput("rmq_num", NULL, min=0, value=default$rmq_num()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

## Edges

    # S -> E1
    else if (setEdge()=='A') { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Susceptible to latent (S -> E1)")),
            size = 's',
            code(HTML(paste0("&lambda;"))),
            helpText(HTML(paste0("The net force of infection"))),
            sliderInput("lambda", label=NULL, min=0, max=1, step=.01, value=0.3),
            hr(),
            code(HTML(paste0("&rho;"))),
            helpText(HTML(paste0("The proportion of contacts (of ascertained cases) that will self-quarantine"))),
            sliderInput("rho", label=NULL, min=0, max=1, step=.01, value=default$rho()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # E1 -> E2
    else if (setEdge()=='B') { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Latent first period to second period (E1 -> E2)")),
            size = 's',
            code(HTML(paste0("&sigma;", tags$sub("1")))),
            helpText(HTML(paste0("Inverse of the first latent period."))),
            sliderInput("sigma1", label=NULL, min=0, max=3, step=.1, value=default$sigma1()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # E2 -> I1
    else if (setEdge()=='C') { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Latent second period to infectious first period (E2 -> I1)")),
            size = 's',
            code(HTML(paste0("&sigma;", tags$sub("2")))),
            helpText(HTML(paste0("Inverse of the second latent period."))),
            sliderInput("sigma2", label=NULL, min=0, max=3, step=.1, value=default$sigma2()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

    # I1 -> I2
    else if (setEdge()=='D') { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Inverse of first infectious period (I1 -> I2)")),
            size = 's',
            code(HTML(paste0("&gamma;", tags$sub("1")))),
            helpText(HTML(paste0("Inverse of the second latent period."))),
            sliderInput("gamma1", label=NULL, min=0, max=8, step=.1, value=default$gamma1()),
            hr(),
            code(HTML(paste0("&alpha;"))),
            helpText(HTML(paste0("Net proportion of people who present."))),
            sliderInput("alpha", label=NULL, min=0, max=1, step=.01, value=default$alpha()),
            hr(),
            code(HTML(paste0("P", tags$sub("M")))),
            helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
            sliderInput("pm", label=NULL, min=0, max=1, step=.01, value=default$pm()),
            easyClose = TRUE,
            footer = modalButton("Done")))
    )}

# I1 -> M
else if (setEdge()=='E') { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Latent second period to infectious first period (E2 -> I1)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("1")))),
        helpText(HTML(paste0("Inverse of the second latent period."))),
        hr(),
        code(HTML(paste0("&alpha;"))),
        helpText(HTML(paste0("Net proportion of people who present."))),
        hr(),
        code(HTML(paste0("P", tags$sub("M")))),
        helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
        hr(),
        helpText(HTML(paste0("These parameters can be modified in the I1 -> I2 dialogue box"))),
        easyClose = TRUE,
        footer = modalButton("Done")))
    )}

# I2 -> R
else if (setEdge()=='F') { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Infectious second period to recovery (I2 -> R)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of second infectious period."))),
        sliderInput("gamma2", label=NULL, min=1, max=10, step=.01, value=default$gamma2()),
        easyClose = TRUE,
        footer = modalButton("Done")))
    )}

# I2 -> R
else if (setEdge()=='G') { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Managed to recoery (M -> R)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of second infectious period."))),
        hr(),
        helpText(HTML(paste0("This parameter can be modified in the I2 -> R dialogue box"))),
        easyClose = TRUE,
        footer = modalButton("Done")))
    )}

) # Closes observe



############################################################################
                    ### Set parameter default values ###
############################################################################

default <- reactiveValues()

# Initial conditions
default$s_num <- reactive(ifelse(is.null(input$s_num), 9997, input$s_num))
default$e1_num <- reactive(ifelse(is.null(input$e1_num), 3, input$e1_num))
default$e2_num <- reactive(ifelse(is.null(input$e2_num), 0, input$e2_num))
default$i1_num <- reactive(ifelse(is.null(input$i1_num), 0, input$i1_num))
default$i2_num <- reactive(ifelse(is.null(input$i2_num), 0, input$i2_num))
default$m_num <- reactive(ifelse(is.null(input$m_num), 0, input$m_num))
default$r_num <- reactive(ifelse(is.null(input$r_num), 0, input$r_num))
default$rm_num <- reactive(ifelse(is.null(input$rm_num), 0, input$rm_num))
default$e1q_num <- reactive(ifelse(is.null(input$e1q_num), 0, input$e1q_num))
default$e2q_num <- reactive(ifelse(is.null(input$e2q_num), 0, input$e2q_num))
default$i1q_num <- reactive(ifelse(is.null(input$i1q_num), 0, input$i1q_num))
default$i2q_num <- reactive(ifelse(is.null(input$i2q_num), 0, input$i2q_num))
default$mq_num <- reactive(ifelse(is.null(input$mq_num), 0, input$mq_num))
default$rq_num <- reactive(ifelse(is.null(input$rq_num), 0, input$rq_num))
default$rmq_num <- reactive(ifelse(is.null(input$rmq_num), 0, input$rmq_num))

# Model parameters
default$lambda <- reactive(ifelse(is.null(input$lambda), 9997, input$lambda))
default$rho <- reactive(ifelse(is.null(input$rho), 0, input$rho))
default$sigma1 <- reactive(ifelse(is.null(input$sigma1), 0, input$sigma1))
default$sigma2 <- reactive(ifelse(is.null(input$sigma2), 0, input$sigma2))
default$gamma1 <- reactive(ifelse(is.null(input$gamma1), 0, input$gamma1))
default$gamma2 <- reactive(ifelse(is.null(input$gamma2), 0, input$gamma2))
default$alpha <- reactive(ifelse(is.null(input$alpha), 0, input$alpha))
default$pm <- reactive(ifelse(is.null(input$pm), 0, input$pm))
default$gamma1q <- reactive(ifelse(is.null(input$gamma1q), 0, input$gamma1q))
default$gamma2q <- reactive(ifelse(is.null(input$gamma2q), 0, input$gamma2q))

############################################################################
                    ### Summary text output ###
############################################################################

# Non-Quarantined population
output$summary1 <- renderText({
    HTML(paste(icon("globe-asia"), em(" Non-quarantined population"), hr(),
        "Susceptible:", code(default$s_num()), br(),
        "Latent (1st Period):", code(default$e1_num()), br(),
        "Latent (2nd Period):", code(default$e2_num()), br(),
        "Infectious (1st Period):", code(default$i1_num()), br(),
        "Infectious (2nd Period):", code(default$i2_num()), br(),
        "Managed:", code(default$m_num()), br(),
        "Recovered:", code(default$r_num()), br(),
        "Recovered from managed:", code(default$rm_num()), br()
        ))
})

# Quarantined population
output$summary2 <- renderText({
    HTML(paste(icon("globe-asia"), em(" Quarantined population"), hr(),
              "Latent (1st Period):", code(default$e1q_num()), br(),
              "Latent (2nd Period):", code(default$e2q_num()), br(),
              "Infectious (1st Period):", code(default$i1q_num()), br(),
              "Infectious (2nd Period):", code(default$i2q_num()), br(),
              "Managed:", code(default$mq_num()), br(),
              "Recovered:", code(default$rq_num()), br(),
              "Recovered from managed:", code(default$rmq_num()), br()
    ))
})

# Model parameters (quarantined population)
output$summary3 <- renderText({
    HTML(paste(icon("sliders-h"), em("Transition parameters (Non-quar)"), hr(),
              "&lambda;"," ", code(default$lambda()), br(),
              "&rho;", code(default$rho()), br(),
              "&sigma;", tags$sub("1"), code(default$sigma1()), br(),
              "&sigma;", tags$sub("2"), code(default$sigma2()), br(),
              "&gamma;", tags$sub("1"), code(default$gamma1()), br(),
              "&gamma;", tags$sub("2"), code(default$gamma2()), br(),
              "&gamma;", tags$sub("1"), tags$sup("q"), code(default$gamma1q()), br(),
              "&gamma;", tags$sub("2"), tags$sup("q"), code(default$gamma2q()), br()
    ))
})

#### SEIQHRF Model

### Initial conditions
state0 <- reactive({
    seihrq_state0(S0 = input$s_num,
                  E0 = input$e_num,
                  I10 = input$i1_num,
                  I20 = input$i2_num,
                  H0 = input$h_num,
                  Rh0 = input$rh_num,
                  R0 = input$r_num,
                  Iq10 = input$iq1_num,
                  Iq20 = input$iq2_num,
                  Hq0 = input$hq_num,
                  Rqh0 = input$rqh_num,
                  Rq0 = input$rq_num)
})

### Model parameters
param <- reactive({
    seihrq_param(R0 = input$r0,
                 #beta = input$beta,
                 sigma = input$sigma,
                 gamma1 = input$gamma1,
                 gamma2 = input$gamma2,
                 gamma3 = input$gamma3,
                 Qeff = input$q_eff,
                 Heff = input$h_eff,
                 rho = input$rho,
                 alpha = input$alpha,
                 eta = input$eta)
})

### Model
model <- eventReactive(input$runMod, {
    isolate(
        simulate_seihrq(t = input$nsteps,
                     state_t0 = state0(),
                     param = param())
    )
})

### Summarise output
output$summary <- renderPrint({
    model()
})

# Extract the data in ggplot ready form
mod_df <- reactive({
    mod_df <- pivot_longer(model()$epi, -t, names_to = "compartment", values_to = "count") %>%
        mutate(complong = recode(compartment,
                                 S = "Susceptible",
                                 E = "Exposed",
                                 Is = "Infected",
                                 Hp = "Hospitalised",
                                 Rc = "Recovered",
                                 F = "Fatalities",
                                 I1 = "Infected (stage 1)",
                                 I2 = "Infected (stage 2)",
                                 H = "Hospitalised no quar",
                                 Rh = "Recovered hosp",
                                 R = "Recovered",
                                 Iq1 = "Inf quar (stage 1)",
                                 Iq2 = "Inf quar (stage 1)",
                                 Hq = "Hosp quar",
                                 Rqh = "Rec hosp quar",
                                 Rq = "Rec quar",
                                 N = "Population"))
    mod_df
})

### Prepare data
output$mod_df <- renderTable({
mod_df()
})

### Model summary
output$plot <- renderggiraph({

    compcols <- c("S" = "yellow", "E" = "orange", "Is" = "red", "Hp" = "magenta", "Rc" = "lightgreen", "F" = "black")
    complabels <- c("S" = "Susceptible", "E" = "Exposed", "Is" = "Infected", "Hp" = "Hospitalised", "Rc" = "Recovered", "F" = "Case fatality")

    if (input$scale == "Linear") {
    p <- mod_df() %>%
        filter(compartment %in% input$plotvars) %>%
        filter(t <= input$ndays) %>%
        mutate(lab = sprintf("<strong>%s</strong><strong>%s</strong><br/><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><br/>%s",
                             "Day: ", t,
                             "N: ", round(count, digits = 0),
                             " (", round(100*count/popcounter(), digits = 1), "%)",
                             complong) %>% lapply(htmltools::HTML)) %>%
        ggplot(aes(x=t, y=count, colour=compartment)) +
        geom_line(size=2, alpha=0.7) +
        theme_dark() +
        theme(legend.position = "right", legend.title = element_blank()) +
        guides(col = guide_legend(ncol = 1)) +
        labs(x="Days since beginning of epidemic", y="Prevalence (persons)") +
        scale_colour_manual(values = compcols, labels=complabels) +
        geom_point_interactive(aes(tooltip = lab))
    }

    if (input$scale == "Log") {
        p <- mod_df() %>%
            filter(compartment %in% input$plotvars) %>%
            filter(t <= input$ndays) %>%
            mutate(lab = sprintf("<strong>%s</strong><strong>%s</strong><br/><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><br/>%s",
                                 "Day: ", t,
                                 "N: ", round(count, digits = 0),
                                 " (", round(100*count/popcounter(), digits = 1), "%)",
                                 complong) %>% lapply(htmltools::HTML)) %>%
            ggplot(aes(x=t, y=count, colour=compartment)) +
            geom_line(size=2, alpha=0.7) +
            theme_dark() +
            theme(legend.position = "right", legend.title = element_blank()) +
            guides(col = guide_legend(ncol = 1)) +
            labs(x="Days since beginning of epidemic", y="Prevalence (persons - log scale)") +
            scale_colour_manual(values = compcols, labels=complabels) +
            geom_point_interactive(aes(tooltip = lab)) +
            scale_y_log10()
    }

    if (input$scale == "Percentage") {
        p <- mod_df() %>%
            filter(compartment %in% input$plotvars) %>%
            filter(t <= input$ndays) %>%
            mutate(lab = sprintf("<strong>%s</strong><strong>%s</strong><br/><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><br/>%s",
                                 "Day: ", t,
                                 "N: ", round(count, digits = 0),
                                 " (", round(100*count/popcounter(), digits = 1), "%)",
                                 complong) %>% lapply(htmltools::HTML)) %>%
            ggplot(aes(x=t, y=100*count/popcounter(), colour=compartment)) +
            geom_line(size=2, alpha=0.7) +
            theme_dark() +
            theme(legend.position = "right", legend.title = element_blank()) +
            guides(col = guide_legend(ncol = 1)) +
            labs(x="Days since beginning of epidemic", y="Percentage (%)") +
            scale_colour_manual(values = compcols, labels=complabels) +
            geom_point_interactive(aes(tooltip = lab))
    }

    girafe(code = print(p))
})


# Interactively update number of days in slider
observe({
    val = min(100, input$nsteps)
    updateSliderInput(session, "ndays", max=input$nsteps, value=val)
})

## Prepare report

output$report <- downloadHandler(
        filename = function(){
            name <- ifelse(input$reportname=="", "my-report", input$reportname)
            name1 <- ifelse(input$datelab==TRUE,
                            paste0(Sys.Date(), "-", gsub(" ", "-", name)),
                            gsub(" ", "-", name))
            ext <- paste0(".", input$reporttype)
            paste0(name1, ext)
            },

        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), ifelse(input$reporttype=="pdf", "pdfreport.Rmd", "htmlreport.Rmd"))
            file.copy(ifelse(input$reporttype=="pdf", "pdfreport.Rmd", "htmlreport.Rmd"), tempReport, overwrite = TRUE)

            # Set up parameters to pass to Rmd document
            params <- list(n = input$nsteps)

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )



}) # Closes Shiny Server
