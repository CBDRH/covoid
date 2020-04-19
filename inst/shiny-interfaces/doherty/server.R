###################################################################################
## COVOID-d: COvid-19 Open-source Infection Dynamics
###################################################################################

# Moss et al 2020 ("The Doherty Institute Model")

###################################################################################


shinyServer(function(session, input, output) {


# Add up the different populations and display the population count
popcounter <- reactive({
    sum(default$s_num(), default$e1_num(), default$e2_num(), default$i1_num(), default$i2_num(), default$m_num(), default$r_num(), default$rm_num(),
        default$e1q_num(), default$e2q_num(), default$i1q_num(), default$i2q_num(), default$mq_num(), default$rq_num(), default$rmq_num())
})



# Output the population count
output$popcount = renderText({
    HTML(paste("Population Size: ", formatC(popcounter(), format="d", big.mark=',')))
})

# Calculate the number of simulation days
nsteps <- reactive({as.numeric( input$dateRange[2] - input$dateRange[1]) })


################################################################################################
                        ### Defining the Model Compartments Diagram ###
################################################################################################

output$network_d <- renderVisNetwork({

    nodes <- data.frame(id = 1:15,

        # add labels on nodes
        label = c('S', 'E1', 'E2', 'I1', 'I2', 'M', 'R', 'Rm',
                  'E1q', 'E2q', 'I1q', 'I2q', 'Mq', 'Rq', 'Rmq'),

        # size adding value
        size = c(rep(10, 15)),

        # Hierarchical level
        level = c(0,1,1,1,1,2,1,2,3,3,3,3,4,3,4),

        # Group
        group = c("Susceptible", rep("Latent", 2), rep("Infectious", 2), "Managed", rep("Recovered", 2),
                  rep("Latent", 2), rep("Infectious", 2), "Managed", rep("Recovered", 2)
                  ),

        # control shape of nodes
        shape = c(rep("square", 15)),

        font.color = c(rep("grey", 15)),

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

        # Label text
        label = c(
            round(default$lambda()*(1-(default$rho()*default$thetam())), digits=3),
            round(default$sigma1(), digits=3),
            round(default$sigma2(), digits=3),
            round(default$gamma1()*(1 - default$alpha()*default$pm()), digits=3),
            round(default$gamma1()*(default$alpha()*default$pm()), digits=3),
            round(default$gamma2(), digits=3),
            round(default$gamma2(), digits=3),
            round(default$lambda()*(default$rho()*default$thetam()), digits=3),
            round(default$sigma1(), digits=3),
            round(default$sigma2(), digits=3),
            round(default$gamma1q()*(1 - default$alpha()*default$pm()), digits=3),
            round(default$gamma1q()*(default$alpha()*default$pm()), digits=3),
            round(default$gamma2q(), digits=3),
            round(default$gamma2q(), digits=3)
            ),

        # Label colour
        font.color = c(rep("darkgrey", 14)),

        # Hover titles
        title = c(HTML('&lambda;(1-&rho;&sdot;&Theta;<sub>M</sub>)'),
                  HTML('&sigma;<sub>1</sub>'),
                  HTML('&sigma;<sub>2</sub>'),
                  HTML('&gamma;<sub>1</sub>(1 - &alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>1</sub>(&alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>2</sub>'),
                  HTML('&gamma;<sub>2</sub>'),
                  HTML('&lambda;(&rho;&sdot;&Theta;<sub>M</sub>)'),
                  HTML('&sigma;<sub>1</sub>'),
                  HTML('&sigma;<sub>2</sub>'),
                  HTML('&gamma;<sub>1</sub><sup>q</sup>(1 - &alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>1</sub><sup>q</sup>(&alpha;P<sub>M</sub>)'),
                  HTML('&gamma;<sub>2</sub><sup>q</sup>'),
                  HTML('&gamma;<sub>2</sub><sup>q</sup>')
                  )
    )


    visNetwork(nodes, edges, submain = HTML("See <a href='https://www.doherty.edu.au/uploads/content_doc/McVernon_Modelling_COVID-19_07Apr1_with_appendix.pdf' target='_blank'>
                                            Moss <em>et al</em> (2020)</a> Technical Appendix, Figure 1")) %>%
        visHierarchicalLayout(direction = "UD", levelSeparation = 80) %>%
        visEdges(arrows = "to") %>%
        visGroups(groupname = "Susceptible", color = "lightblue") %>%
        visGroups(groupname = "Latent", color = "orange") %>%
        visGroups(groupname = "Infectious", color = "red") %>%
        visGroups(groupname = "Managed", color = "pink") %>%
        visGroups(groupname = "Recovered", color = "lightgreen") %>%
        visGroups(groupname = "Other", shape = "icon", icon = list(code = "f7d9", size = 25)) %>%
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

output$test1 <- renderText({paste0("Node: ", values$setNode, " Edge: ", values$setEdge, "days: ", nsteps() )})
output$test2 <- renderUI({
paste(default$thetam())
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
                size = 's',
                code("S"),
                helpText("The initial number of susceptible individuals"),
                numericInput("s_num", label=NULL, min=0, value=default$s_num()),
                easyClose = FALSE,
                footer = actionButton("modal_done", label = "Done") ))
        )}

    # E1.
    else if (setNode()==2) { isolate(
        showModal(modalDialog(
            title = "Latent period (first stage)",
            size = 's',
            code(HTML(paste0("E", tags$sub(1)))),
            helpText("Individuals who are infected but not contagious (Stage 1"),
            numericInput("e1_num", NULL, min=0, value=default$e1_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done") ))
    )}

    # E2.
    else if (setNode()==3) { isolate(
        showModal(modalDialog(
            title = "Latent period (second stage)",
            size = 's',
            code(HTML(paste0("E", tags$sub(2)))),
            helpText("Individuals who are infected but not contagious (Stage 2)"),
            numericInput("e2_num", NULL, min=0, value=default$e2_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I1.
    else if (setNode()==4) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage)",
            size = 's',
            code(HTML(paste0("I", tags$sub(1)))),
            helpText("Individuals who are infected and are contagious (First stage)"),
            numericInput("i1_num", NULL, min=0, value=default$i1_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I2.
    else if (setNode()==5) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage)",
            size = 's',
            code(HTML(paste0("I", tags$sub(2)))),
            helpText("Individuals who are infected but not contagious (Second stage)"),
            numericInput("i2_num", NULL, min=0, value=default$i2_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # M.
    else if (setNode()==6) { isolate(
        showModal(modalDialog(
            title = "Managed Cases",
            size = 's',
            code("M"),
            helpText(HTML(paste0("Managed cases, ascertained upon leaving I", tags$sub(1), " and less infectious than individuals in I", tags$sub(2)))),
            numericInput("m_num", NULL, min=0, value=default$m_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # R.
    else if (setNode()==7) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals",
            size = 's',
            code("R"),
            helpText("Recovered individuals"),
            numericInput("r_num", NULL, min=0, value=default$r_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # Rm.
    else if (setNode()==8) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals (managed)",
            size = 's',
            code(HTML(paste0("R", tags$sub("M")))),
            helpText("Recovered individuals that were managed cases"),
            numericInput("rm_num", NULL, min=0, value=default$rm_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # E1q.
    else if (setNode()==9) { isolate(
        showModal(modalDialog(
            title = "Latent period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("E", tags$sub(1), tags$sup("q") ))),
            helpText("Quarantined individuals who are infected but not contagious (Stage 1)"),
            numericInput("e1q_num", NULL, min=0, value=default$e1q_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # E2q.
    else if (setNode()==10) { isolate(
        showModal(modalDialog(
            title = "latent period (second stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("E", tags$sub(2), tags$sup("q") ))),
            helpText("Quarantined individuals who are infected but not contagious (Stage 2)"),
            numericInput("e2q_num", NULL, min=0, value=default$e2q_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I1q.
    else if (setNode()==11) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("I", tags$sub(1), tags$sup("q") ))),
            helpText("Individuals who are infected and are contagious (First stage)"),
            numericInput("i1q_num", NULL, min=0, value=default$i1q_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I2q.
    else if (setNode()==12) { isolate(
        showModal(modalDialog(
            title = "Infectious period (first stage) [Quarantined]",
            size = 's',
            code(HTML(paste0("I", tags$sub(2), tags$sup("q") ))),
            helpText("Individuals who are infected but not contagious (Second stage)"),
            numericInput("i2q_num", NULL, min=0, value=default$i2q_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # Mq.
    else if (setNode()==13) { isolate(
        showModal(modalDialog(
            title = "Managed Cases [Quarantined]",
            size = 's',
            code(HTML(paste0("M", tags$sup("q") ))),
            helpText(HTML(paste0("Managed cases, ascertained upon leaving I", tags$sub(1), " and less infectious than individuals in I", tags$sub(2)))),
            numericInput("mq_num", NULL, min=0, value=default$mq_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # Rq.
    else if (setNode()==14) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals [Quarantined]",
            size = 's',
            code(HTML(paste0("R", tags$sup("q") ))),
            helpText("Quarantined iindividuals that have recovered"),
            numericInput("rq_num", NULL, min=0, value=default$rq_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # Rmq.
    else if (setNode()==15) { isolate(
        showModal(modalDialog(
            title = "Recovered individuals (managed) [Quarantined]",
            size = 's',
            code(HTML(paste0("R", tags$sub("M"),  tags$sup("q")))),
            helpText("Quarantined individuals that were managed cases and have recovered"),
            numericInput("rmq_num", NULL, min=0, value=default$rmq_num()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

) # Closes Observe

##############
### Edges ###
##############

observe(
    # S -> E1
    if (setEdge()=='A' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Susceptible to latent (S", "&rarr;", "E1)")),
            size = 'm',
            helpText(HTML(paste0("The net force of infection ", "&lambda;", " is a function of several key parameters:"))),
            helpText(HTML(paste0("Currently ", "&lambda;", "="))),
            accordion(
                accordionItem(
                    id = 1,
                    title = "Key parameters",
                    color = "info",
                    collapsed = TRUE,
                    # R0
                    code(HTML(paste0("R", tags$sub("0")))),
                    helpText("The basic reproduction number."),
                    sliderInput("r0", NULL, min=0, max=8, step = .01, value = default$r0()),
                    hr(),
                    # Lambda imp
                    code(HTML(paste0("&lambda;", tags$sub("imp")))),
                    helpText("The force of infection from importation."),
                    sliderInput("lambdaimp", NULL, min=0, max=1, step=.01, value = default$lambdaimp()),
                    hr(),
                    # Qeff
                    code(HTML(paste0("Q", tags$sub("eff")))),
                    helpText("The reduction in infectiousness due to quarantine."),
                    sliderInput("qeff", NULL, min=0, max=1, step=.01, value = default$qeff()),
                    hr(),
                    # Meff
                    code(HTML(paste0("M", tags$sub("eff")))),
                    helpText("The reduction in infectiousness due to case management."),
                    sliderInput("meff", NULL, min=0, max=1, step=.01, value = default$meff()),
                    hr(),
                    # Rho
                    code(HTML(paste0("&rho;"))),
                    helpText("The proportion of contacts (of ascertained cases) that will self-quarantine."),
                    sliderInput("meff", NULL, min=0, max=1, step=.01, value = default$rho()),
                    hr()
                ),
                accordionItem(
                    id = 2,
                    title = "Other parameters",
                    color = "warning",
                    collapsed = TRUE,
                    # CTm
                    code(HTML(paste0("CT", tags$sub("M")))),
                    helpText(HTML(paste0("Contacts of managed cases who will enter E", tags$sub("1"), tags$sup("q"), " if they become infected"))),
                    sliderInput("ctm_num", NULL, min=0, max=26, step=1, value = default$ctm_num()),
                    hr(),
                    # CTnm
                    code(HTML(paste0("CT", tags$sub("NM")))),
                    helpText("Contacts of unmanaged cases"),
                   sliderInput("ctnm_num", NULL, min=0, max=26, step=1, value = default$ctnm_num()),
                    hr()
                )
            ),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # E1 -> E2
    else if (setEdge()=='B' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Latent first period to second period (E1", "&rarr;", "E2)")),
            size = 's',
            code(HTML(paste0("&sigma;", tags$sub("1")))),
            helpText(HTML(paste0("Inverse of the first latent period."))),
            sliderInput("sigma1", label=NULL, min=0, max=3, step=.1, value=default$sigma1()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # E2 -> I1
    else if (setEdge()=='C' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Latent second period to infectious first period (E2", "&rarr;", "I1)")),
            size = 's',
            code(HTML(paste0("&sigma;", tags$sub("2")))),
            helpText(HTML(paste0("Inverse of the second latent period."))),
            sliderInput("sigma2", label=NULL, min=0, max=3, step=.1, value=default$sigma2()),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

    # I1 -> I2
    else if (setEdge()=='D' & setNode()==0) { isolate(
        showModal(modalDialog(
            title = HTML(paste0("Inverse of first infectious period (I1", "&rarr;", "I2)")),
            size = 'm',
            accordion(
                accordionItem(
                    id = 1,
                    title = "Key parameters",
                    color = "info",
                    collapsed = FALSE,
                    # gamma1
                    code(HTML(paste0("&gamma;", tags$sub("1")))),
                    helpText(HTML(paste0("Inverse of first infectious period."))),
                    sliderInput("gamma1", label=NULL, min=0, max=8, step=.1, value=default$gamma1()),
                    hr(),
                    code(HTML(paste0("P", tags$sub("M")))),
                    helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
                    sliderInput("pm", label=NULL, min=0, max=1, step=.01, value=default$pm())
                ),
                accordionItem(
                    id = 2,
                    title = "Other parameters",
                    color = "warning",
                    collapsed = TRUE,
                  # alpham
                    code(HTML(paste0("&alpha;", tags$sub("m")))),
                    helpText(HTML(paste0("Proportion of non-severe people who present (“mild”)."))),
                    sliderInput("alpham", label=NULL, min=0, max=1, step=.01, value=default$alpham()),
                    hr(),
                    code(HTML(paste0("&eta;"))),
                    helpText(HTML(paste0("Scaling factor for hospitalisation proportion (“severe”)."))),
                    sliderInput("eta", label=NULL, min=0, max=1, step=.01, value=default$eta())
                )
            ),
            easyClose = FALSE,
            footer = actionButton("modal_done", label = "Done")))
    )}

# I1 -> M
else if (setEdge()=='E' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Latent second period to infectious first period (E2", "&rarr;", "I1)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("1")))),
        helpText(HTML(paste0("Inverse of the second latent period."))),
        hr(),
        code(HTML(paste0("P", tags$sub("M")))),
        helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
        hr(),
        code(HTML(paste0("&alpha;", tags$sub("m")))),
        helpText(HTML(paste0("Proportion of non-severe people who present (“mild”)."))),
        hr(),
        code(HTML(paste0("&eta;"))),
        helpText(HTML(paste0("Scaling factor for hospitalisation proportion (“severe”)."))),
        hr(),
        helpText(HTML(paste0("These parameters can be modified in the I", tags$sub("1"), "&rarr;", "I", tags$sub("2"), " dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
    )}

# I2 -> R
else if (setEdge()=='F' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Infectious second period to recovery (I2", "&rarr;", "R)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of second infectious period."))),
        sliderInput("gamma2", label=NULL, min=1, max=10, step=.01, value=default$gamma2()),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
    )}

# I2 -> R
else if (setEdge()=='G' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Managed to recoery (M", "&rarr;", "R)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of second infectious period."))),
        hr(),
        helpText(HTML(paste0("This parameter can be modified in the I2", "&rarr;", "R dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
    )}

## Quarantined arm

# S -> E1q
else if (setEdge()=='H' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Susceptible to latent (S", "&rarr;", "E", tags$sub("1"),")")),
        size = 's',
        code(HTML(paste0("&lambda;"))),
        helpText(HTML(paste0("The net force of infection"))),
        code(HTML(paste0("&rho;"))),
        helpText(HTML(paste0("The proportion of contacts (of ascertained cases) that will self-quarantine"))),
        hr(),
        helpText(HTML(paste0("These parameters can be changed from the S", "&rarr;", "E", tags$sub("1"), " dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# E1q -> E2q
else if (setEdge()=='I' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Latent first period to second period (E", tags$sub("1"), tags$sup("q"), "&rarr;", "E2q)")),
        size = 's',
        code(HTML(paste0("&sigma;", tags$sub("1")))),
        helpText(HTML(paste0("Inverse of the first latent period (Assumed to be the same for quarantined and non-quarantined individuals."))),
        hr(),
        helpText(HTML(paste0("This parameter can be changed in the E", tags$sub("1"), "&rarr;", "E", tags$sub("2"), " dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# # E2q -> I1q
else if (setEdge()=='J' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Latent second period to infectious first period (E", tags$sub("2"), tags$sup("q"), "&rarr;", "I", tags$sub("1"), tags$sup("q"), ")")),
        size = 's',
        code(HTML(paste0("&sigma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of the first latent period (Assumed to be the same for quarantined and non-quarantined individuals."))),
        hr(),
        helpText(HTML(paste0("This parameter can be changed in the E", tags$sub("2"), "&rarr;", "I", tags$sub("1"), " dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# I1q -> I2q
else if (setEdge()=='K' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Inverse of first infectious period (I", tags$sub("1"), tags$sup("q"), "&rarr;", "I", tags$sub("2"), tags$sup("q"), ")")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("1"), tags$sup("q") ))),
        helpText(HTML(paste0("Inverse of the first infectious period for quarantined individuals."))),
        sliderInput("gamma1q", label=NULL, min=0, max=8, step=.1, value=default$gamma1q()),
        hr(),
        code(HTML(paste0("&alpha;"))),
        helpText(HTML(paste0("Net proportion of people who present."))),
        code(HTML(paste0("P", tags$sub("M")))),
        helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
        hr(),
        helpText(HTML(paste0("The parameters ", "&alpha;", " and P", tags$sub("M"), " are assumed to be the same for quarantined and non-quarantinied
                             individuals, and can be changed in the I", tags$sub("1"), "&rarr;", "I", tags$sub("2"), " dialogue box." ))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# I1q -> Mq
else if (setEdge()=='L' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Infectious first period to Managed (I", tags$sub("1"), "&rarr;", "M)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("1"), tags$sub("q")))),
        helpText(HTML(paste0("Inverse of first infectious period for quarantined cases. This parameter can be modified in the I",
                             tags$sub(1), tags$sup("q"), "&rarr;", "I", tags$sub(2), tags$sup("q"), " dialogue box." ))),
        hr(),
        code(HTML(paste0("&alpha;"))),
        helpText(HTML(paste0("Net proportion of people who present."))),
        code(HTML(paste0("P", tags$sub("M")))),
        helpText(HTML(paste0("Probability of presenting cases being effectively managed."))),
        hr(),
        helpText(HTML(paste0("The parameters ", "&alpha;", " and P", tags$sub("M"), " are assumed to be the same for quarantined and non-quarantinied
                             individuals, and can be changed in the I", tags$sub("1"), "&rarr;", "I", tags$sub("2"), " dialogue box." ))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# I2q -> Rq
else if (setEdge()=='M' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Infectious second period to recovery (I", tags$sub("2"), tags$sup("q"), "&rarr;", "R", tags$sup("q"), ")")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2")))),
        helpText(HTML(paste0("Inverse of second infectious period for quarantined cases."))),
        sliderInput("gamma2q", label=NULL, min=1, max=10, step=.01, value=default$gamma2q()),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
)}

# Mq -> Rmq
else if (setEdge()=='N' & setNode()==0) { isolate(
    showModal(modalDialog(
        title = HTML(paste0("Managed to recoery for quarantined cases (M", "&rarr;", "R)")),
        size = 's',
        code(HTML(paste0("&gamma;", tags$sub("2"), tags$sup("q")))),
        helpText(HTML(paste0("Inverse of second infectious period for quarantined cases."))),
        hr(),
        helpText(HTML(paste0("This parameter can be modified in the I", tags$sub("2"), tags$sup("q"), "&rarr;", "R", tags$sub("M"), tags$sup("q"), " dialogue box"))),
        easyClose = FALSE,
        footer = actionButton("modal_done", label = "Done")))
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
default$ctnm_num <- reactive(ifelse(is.null(input$ctnm_num), 20, input$ctnm_num))
default$ctm_num <- reactive(ifelse(is.null(input$ctm_num), 2, input$ctm_num))

# Model parameters
default$r0 <- reactive(ifelse(is.null(input$r0), 2.53, input$r0))
default$lambdaimp <- reactive(ifelse(is.null(input$lambdaimp), .3, input$lambdaimp))
default$rho <- reactive(ifelse(is.null(input$rho), 0.8, input$rho))
default$sigma1 <- reactive(ifelse(is.null(input$sigma1), 1/1.6, input$sigma1))
default$sigma2 <- reactive(ifelse(is.null(input$sigma2), 1/1.6, input$sigma2))
default$gamma1 <- reactive(ifelse(is.null(input$gamma1), 1/4.0, input$gamma1))
default$gamma2 <- reactive(ifelse(is.null(input$gamma2), 1/4.0, input$gamma2))
default$alpham <- reactive(ifelse(is.null(input$alpham), 0, input$alpham))
default$pm <- reactive(ifelse(is.null(input$pm), 1, input$pm))
default$gamma1q <- reactive(ifelse(is.null(input$gamma1q), 1/5.68, input$gamma1q))
default$gamma2q <- reactive(ifelse(is.null(input$gamma2q), 1/5.68, input$gamma2q))
default$qeff <- reactive(ifelse(is.null(input$qeff), 0.5, input$qeff))
default$meff <- reactive(ifelse(is.null(input$meff), 0.8, input$meff))
default$eta <- reactive(ifelse(is.null(input$eta), 1/sqrt(2), input$eta))
default$alphambeta <- 0.5
default$probHospGivenInf <- 0.09895
default$delta <- 1/14
default$kappa <- 20

# Calculated parameters
default$beta <- reactive(default$r0()*((default$sigma2())^-1 + (default$gamma1())^-1 + (default$gamma2())^-1)^(-1))
default$betamq <- reactive(default$beta()*max(default$qeff(), default$meff() ))
default$thetam <- reactive( (default$s_num()/popcounter())* ((default$ctm_num())/(default$ctm_num() + default$ctnm_num())))
default$thetanm <- reactive( (default$s_num()/popcounter())* ((default$ctnm_num())/(default$ctm_num() + default$ctnm_num())))
default$lambda <- reactive(default$lambdaimp() +
                           default$beta() * ( default$e2_num() + default$i1_num() + default$i2_num() ) +
                           default$beta() * ( 1 - default$qeff() ) * ( default$e2q_num() + default$i1q_num() + default$i2q_num() ) +
                           default$beta() * ( 1 - default$meff() ) * ( default$m_num() ) +
                           default$betamq() * ( default$m_num())
                          )

default$alpha <- reactive(default$eta() + default$alpham()*(1 - default$eta()))


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
    HTML(paste(icon("sliders-h"), em("Transition params"), hr(),
               code(HTML(paste0("R", tags$sub("0")))), round(default$r0(), digits = 3), br(),
               code(HTML(paste0("&lambda;", tags$sub("imp")))), round(default$lambdaimp(), digits = 3), br(),
               code(HTML(paste0("M", tags$sub("eff")))), round(default$meff(), digits = 3), br(),
               code(HTML(paste0("Q", tags$sub("eff")))), round(default$qeff(), digits = 3), br(),
               code(HTML("&rho;")), round(default$rho(), digits = 3), br(),
               code(HTML("&beta;")), round(default$beta(), digits = 3), br(),
               code(HTML(paste0("&Theta;", tags$sub("M")))), round(default$thetam(), digits = 3), br(),
               code(HTML("&lambda;")), round(default$lambda(), digits = 3), br()

    ))
})


output$summary4 <- renderText({
    HTML(paste(icon("sliders-h"), em("Transition params"), hr(),
            code(HTML(paste0("&sigma;", tags$sub("1")))), "1/", round(1/default$sigma1(), digits = 3), br(),
            code(HTML(paste0("&sigma;", tags$sub("2")))), "1/", round(1/default$sigma2(), digits = 3), br(),
            code(HTML(paste0("&gamma;", tags$sub("1")))), "1/", round(1/default$gamma1(), digits = 3), br(),
            code(HTML(paste0("&gamma;", tags$sub("2")))), "1/", round(1/default$gamma2(), digits = 3), br(),
            code(HTML(paste0("&gamma;", tags$sub("1"), tags$sup("q")))), "1/", round(1/default$gamma1q(), digits = 3), br(),
            code(HTML(paste0("&gamma;", tags$sub("2"), tags$sup("q")))), "1/", round(1/default$gamma2q(), digits = 3), br()
    ))
})



# "&theta,;", tags$sub("m"),  code(default$thetam()), br(),
# "&sigma;", tags$sub("1"), code(default$sigma1()), br(),
# "&sigma;", tags$sub("2"), code(default$sigma2()), br(),
# "&gamma;", tags$sub("1"), code(default$gamma1()), br(),
# "&gamma;", tags$sub("2"), code(default$gamma2()), br(),
# "&gamma;", tags$sub("1"), tags$sup("q"), code(default$gamma1q()), br(),
# "&gamma;", tags$sub("2"), tags$sup("q"), code(default$gamma2q()), br()

############################################################################
            ### Parameterise and run the model ###
############################################################################

### Initial conditions
state0 <- reactive({
    seimrqc_state0(S0 = default$s_num(),
                  E10 = default$e1_num(),
                  E20 = default$e2_num(),
                  I10 = default$i1_num(),
                  I20 = default$i2_num(),
                  R0 = default$r_num(),
                  M0 = default$m_num(),
                  Rm0 = default$rm_num(),
                  Eq10 = default$e1q_num(),
                  Eq20 = default$e2q_num(),
                  Iq10 = default$i1q_num(),
                  Iq20 = default$i2q_num(),
                  Rq0 = default$rq_num(),
                  Mq0 = default$mq_num(),
                  Rqm0 = default$rmq_num(),
                  CTm0 = default$ctm_num(),
                  CTnm0 = default$ctnm_num())
})

### Model parameters
param <- reactive({
   seimrqc_param(R0 = default$r0(),
                 lambdaimp = default$lambdaimp(),
                 sigma1 = default$sigma1(),
                 sigma2 = default$sigma2(),
                 gamma1 = default$gamma1(),
                 gamma2 = default$gamma2(),
                 gammaq1 = default$gamma1q(),
                 gammaq2 = default$gamma2q(),
                 Qeff = default$qeff(),
                 Meff = default$meff(),
                 rho = default$rho(),
                 eta = default$eta(),
                 alphamBeta = default$alphambeta, #
                 probHospGivenInf = default$probHospGivenInf, #
                 delta = default$delta, #
                 kappa = default$kappa, #
                 pm = default$pm()
                 )
})


# Run the model when the button is clicked
model <- eventReactive(input$runMod, {
    isolate(
        simulate_seimrqc(t = nsteps(),
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
                                 E1 = "Exposed (first stage)",
                                 E2 = "Exposed (second stage)",
                                 I1 = "Infected (first stage)",
                                 I2 = "Infected (second stage)",
                                 R = "Recovered non-quar",
                                 M = "Managed non-quar",
                                 Rm = "Recovered from managed)",
                                 Eq1 = "Exp quar (first stage)",
                                 Eq2 = "Exp quar (second stage)",
                                 Iq1 = "Inf quar (first stage)",
                                 Iq2 = "Inf quar (second stage)",
                                 Rq = "Recovered quar",
                                 Rqm = "Rec quar managed",
                                 CTm = "Contacts of managed cases",
                                 CTnm = "Contacts of unmanaged cases",
                                 Itotal = "Infected",
                                 I = "",
                                 Ictotal = "",
                                 Mtotal = "Managed",
                                 H = "Hospitalised",
                                 D = "Fatalities",
                                 Q = "Quarantined",
                                 N = "Population"),
               date = as.Date(t + input$dateRange[1] - 1, origin = "1970-01-01")) %>%
        group_by(compartment) %>%
        arrange(t) %>%
        mutate(cum_sum = cumsum(count)) %>%
        mutate(lab1 = sprintf("<strong>%s</strong><strong>%s</strong><br/><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><br/>%s",
                             "Day: ", t,
                             "Count: ", formatC(round(count, digits = 0), format="d", big.mark=','),
                             " (", round(100*count/popcounter(), digits = 1), "%)",
                             complong) %>% lapply(htmltools::HTML)) %>%
        mutate(lab2 = sprintf("<strong>%s</strong><strong>%s</strong><br/><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><strong>%s</strong><br/>%s",
                             "Day: ", t,
                             "Cuml: ", formatC(round(cum_sum, digits = 0), format="d", big.mark=','),
                             " (", round(100*cum_sum/popcounter(), digits = 1), "%)",
                             complong) %>% lapply(htmltools::HTML))
    mod_df
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


### Update tooltip based on choice of graph
tooltip <- reactive({
    if (input$yvar=="count"){"lab1"}
    else if (input$yvar=="cum_sum"){"lab2"}
})

### Model summary
output$plot <- renderggiraph({

    compcols <- c("S" = "lightblue", "Itotal" = "red", "Mtotal" = "pink", "H" = "maroon", "Q" = "Purple", "Rtotal" = "lightgreen", "D" = "Black" )
    complabels <- c("S" = "Susceptible", "Itotal" = "Infected", "MTotal" = "Managed", "H" = "Hospitalised", "Q" = "Quarantined", "Rtotal" = "Recovered", "D" = "Case fatality")

    if (input$scale == "Linear") {
    p <- mod_df() %>%
        filter(compartment %in% input$plotvars) %>%
        filter(t <= input$ndays) %>%
        ggplot(aes_string(x="date", y=input$yvar, colour="compartment")) +
        geom_line(size=2, alpha=0.7) +
        scale_x_date(date_labels="%d%b%Y") +
        theme_dark() +
        theme(legend.position = "right", legend.title = element_blank()) +
        guides(col = guide_legend(ncol = 1)) +
        labs(x="Date", y="Prevalence (persons)") +
        scale_colour_manual(values = compcols, labels=complabels) +
        geom_point_interactive(aes_string(tooltip = tooltip()))
    }

    if (input$scale == "Log") {
        p <- mod_df() %>%
            filter(compartment %in% input$plotvars) %>%
            filter(t <= input$ndays) %>%
            ggplot(aes_string(x="date", y=input$yvar, colour="compartment")) +
            geom_line(size=2, alpha=0.7) +
            scale_x_date(date_labels="%d%b%Y") +
            theme_dark() +
            theme(legend.position = "right", legend.title = element_blank()) +
            guides(col = guide_legend(ncol = 1)) +
            labs(x="Date", y="Prevalence (persons - log scale)") +
            scale_colour_manual(values = compcols, labels=complabels) +
            geom_point_interactive(aes_string(tooltip = tooltip())) +
            scale_y_log10()
    }

    if (input$scale == "Percentage") {
        p <- mod_df() %>%
            filter(compartment %in% input$plotvars) %>%
            filter(t <= input$ndays) %>%
            ggplot(aes_string(x="date", y=input$yvar, colour="compartment")) +
            geom_line(size=2, alpha=0.7) +
            scale_x_date(date_labels="%d%b%Y") +
            theme_dark() +
            theme(legend.position = "right", legend.title = element_blank()) +
            guides(col = guide_legend(ncol = 1)) +
            labs(x="Date", y="Percentage (%)") +
            scale_colour_manual(values = compcols, labels=complabels) +
            geom_point_interactive(aes_string(tooltip = tooltip()))
    }

    girafe(code = print(p))
})


# Interactively update number of days in slider
observe({
    val = min(100, nsteps())
    updateSliderInput(session, "ndays", max=nsteps(), value=val)
})

## Prepare report

output$downloadReport <- downloadHandler(
        filename = function(){
            name <- ifelse(input$reportname=="", "my-report", input$reportname)
            name1 <- ifelse(input$datelab==TRUE,
                            paste0(Sys.Date(), "-", gsub(" ", "-", name)),
                            gsub(" ", "-", name))
            paste(name1, sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
            },

        content = function(file) {
            src <- normalizePath('report.Rmd')

            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)


            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            out <- rmarkdown::render('report.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )



}) # Closes Shiny Server
