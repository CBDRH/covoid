
shinyServer(function(input, output) {

# Add up the different populations and display the population count
popcounter <- reactive({
    sum(input$s_num, input$e_num, input$i1_num, input$i2_num, input$iq1_num, input$iq2_num,
        input$h_num, input$hq_num, input$r_num, input$rq_num, input$rh_num, input$rqh_num)
})

output$popcount = renderText({
    HTML(paste("Population Size: ", popcounter()))
})

#### SIR Model

### Initial conditions
sir_state0 <- reactive({
    sir_state0(input$R0, input$beta, 1/input$invgamma)
})

})
