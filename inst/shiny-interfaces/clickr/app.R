library(ggplot2)

ui <- fluidPage(
    h2("Exploring interactive ways to specify time-varying functions"),
    helpText("Click on the grid below to define your intervention effectiveness over time"),
    fluidRow(
        column(width=3,
               dateRangeInput("daterange", "Enter the date range of your scenario",
                              start = "2020-01-01", end = "2020-12-31", min = "2020-01-01", max = "2021-12-31"),
               sliderInput("impact", label = h3("The plausible range of your intervention"), min = 0,
                           max = 100, value = c(40, 60))
               ),
        column(width = 5,
               plotOutput("plot1", height = 350,
                          click = "plot1_click",
                          dblclick = "plot1_dblclick",
                          hover = hoverOpts("plot1_hover", delay=100, delayType = "debounce")),
               helpText("Click to add a point. Double-click to remove."),
               actionButton("reset", "Reset"),
               actionButton("undo", "Undo"),
               sparklineOutput("test")
        ),
        column(width=4,
               DT::dataTableOutput("tabout")
               )
    )
)

server <- function(input, output) {

    # 1. set up reactive dataframe
    values <- reactiveValues()

    # Place holder for clicked values
    values$DT <- data.frame(x = numeric(), y = numeric(), dropx = numeric(), dropy = numeric(), dist = numeric())

    # Place holder for values returned when user hovers
    values$hover <- data.frame(x = numeric(), y = numeric(), lab = character() )

    # Dynamically calculate scale of plot (used to determine offset for labels)
    xscale <- reactive({(input$daterange[2] - input$daterange[1])/10})
    yscale <- reactive({(input$impact[2] - input$impact[1])/10})

    # Dynamically set digits
   digits <- reactive({
       ifelse(input$impact[2] < 10, 2, 0)
   })


    output$plot1 <- renderPlot({
        ggplot(values$DT, aes(as.Date(x, origin = '1970-01-01'), y)) +
        geom_point(shape=15, size=8, color = "red", alpha = 0.25) +
        geom_line(color = "red", size=3) +
        geom_label(aes(label = round(y, digits = digits()))) +
        geom_label(data=values$hover, aes(label = lab), nudge_y = yscale(), label.size=.01 ) +
        scale_x_date(date_labels="%d%b%Y") +
        coord_cartesian(xlim = c(input$daterange[1], input$daterange[2]), ylim = c(input$impact[1], input$impact[2])) +
        labs(x = "Date", y = "Effectiveness") +
            theme(panel.background=element_rect(fill = "gray20"),
                  panel.grid = element_line(colour = "lightblue1"))
    })

    # Add points that are clicked
    observeEvent(input$plot1_click, {
        add_row <- data.frame(x = input$plot1_click$x,
                              y = input$plot1_click$y,
                              dropx = 0,
                              dropy = 0,
                              dist = 0)
        # add row to the data.frame
        values$DT <- rbind(values$DT, add_row)
    })

    # Remove points that are double clicked
    observeEvent(input$plot1_dblclick, {
        values$DT$dropx <- as.numeric(input$plot1_dblclick$x)
        values$DT$dropy <- as.numeric(input$plot1_dblclick$y)
        values$DT$dist <- sqrt((values$DT$x-values$DT$dropx)^2 + (values$DT$y-values$DT$dropy)^2)

        # Drop selected from the data.frame
        values$DT <- filter(values$DT, dist > 20)
    })


    # compile the function
    fx <- reactive({

        timeframe <- data.frame(t = seq(1, as.numeric(input$daterange[2]-input$daterange[1]), 1))

        tidydata <- data.frame(
            t = floor((values$DT$x) - as.numeric(input$daterange[1])) + 1,
            value = round(values$DT$y, digits=2))

        fx <- left_join(timeframe, tidydata, by='t')
        fx %>%
            as.ts() %>% imputeTS::na_interpolation(option='linear')
    })

    observe({
        input$plot1_click
        ifx <<- fx()
    })

    output$test <- renderSparkline({
        sparkline(fx()[,2], chartRangeMin = min(fx()[,2]))
                  })

    # Label that are hovered
    observeEvent(input$plot1_hover, {
        values$hover <- data.frame(x = input$plot1_hover$x,
                              y = input$plot1_hover$y,
                              lab = paste0(format(as.Date(input$plot1_hover$x, origin = '1970-01-01'), "%d %b"),
                                           "\n", "Value = ", round(input$plot1_hover$y, digits=digits()))
        )
    })


    ## Remove last row on actionButton click
    observeEvent(input$undo, {
        rem_row <- values$DT[-nrow(values$DT), ]
        values$DT <- rem_row
    })

    ## Reset on click
    observeEvent(input$reset, {
        values$DT <- data.frame(x = numeric(),
                                y = numeric())
    })

    # Present selected points
    output$tabout <- DT::renderDataTable({
        fx()
    })

}

shinyApp(ui, server)
