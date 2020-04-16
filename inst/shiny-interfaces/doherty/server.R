
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
                        size = rep(25, 15),

                        # Hierarchical level
                        level = c(0,1,1,1,1,2,1,2,3,3,3,3,4,3,4),

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
                        ),

                        # color
                        color = c("black", rep("blue", 2), rep("red",2), "orange", rep("green", 2),
                                  rep("blue", 2), rep("red",2), "orange", rep("green", 2))

    )

    edges <- data.frame(from = c(1, 2, 3, 4, 4, 5, 6, 1, 9, 10, 11, 11, 12, 13),
                        to = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        id = LETTERS[seq( from = 1, to = 14 )],
                        dashes = c(rep(FALSE,7), rep(TRUE,7)),
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
        visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE)) %>%
        visInteraction(hover = TRUE) %>%
        visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('unique_id', nodes);
                ;}", hoverEdge = "function(edges) {
    Shiny.onInputChange('unique_id', edges);
    ;}")
})


output$test2 <- renderText({paste0("answer = ", input$unique_id )})


################################################################################################
                    ### Set parameters by clicking on a network node ###
################################################################################################

## Set current node on click
setNode <- reactive({
    ifelse(is.null(input$network_d_selected), 0, input$network_d_selected)
})

## Set current edge on click
setEdge <- reactive({
    ifelse(is.null(input$current_edge_id), 0, input$current_edge_id)
})



observe(
## Nodes

    # S.
    if (setNode()==1) {
            showModal(modalDialog(
                title = "Susceptible individuals",
                code("S"),
                helpText("The initial number of susceptible individuals"),
                numericInput("s_num", label=NULL, min=0, value=9997),
                easyClose = TRUE,
                footer = modalButton("Done")))
    }

    # E1.
    else if (setNode()==2) {
        showModal(modalDialog(
            title = "Latent period (first stage)",
            code(HTML(paste0("E", tags$sub(1)))),
            helpText("The initial individuals who are infected but not contagious"),
            numericInput("i1_num", NULL, min=0, value=0),
            easyClose = TRUE,
            footer = modalButton("Done")))
    }

    # E2.
    else if (setNode()==3) {
        showModal(modalDialog(
            title = "Latent period (second stage)",
            code(HTML(paste0("E", tags$sub(2)))),
            helpText("The initial individuals who are infected but not contagious"),
            numericInput("i1_num", NULL, min=0, value=0),
            easyClose = TRUE,
            footer = modalButton("Done")))
    }

## Edges
else if (setEdge()==1) {
    showModal(modalDialog(
        title = span(HTML("Susceptible individuals")),
        code("lawks"),
        helpText("The initial number of susceptible individuals"),
        #numericInput("s_num", label=NULL, min=0, value=9997),
        easyClose = TRUE,
        footer = modalButton("Done")))
}

) # Closes observe





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
