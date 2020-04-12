
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
