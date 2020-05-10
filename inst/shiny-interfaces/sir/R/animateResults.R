animateResults <- function(df, scale, logScale, plotvars, ndays, update_progress = NULL) {

    # A temp file to save the output. This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')

    compcols <- c("S" = "lightblue", "I" = "red", "R" = "lightgreen")
    complabels <- c("S" = "Susceptible", "I" = "Infectious", "R" = "Recovered")

    ### Update graph based on choice of scale and cumulative

    # Incidence
    if(scale=="Count") {
        yvar <- "count"
        part1 <- "Incidence"
        part2 <- "(Persons)"
        tt <- "lab1"
    }

    # Percent
    if(scale=="Percent") {
        yvar <- 'pct'
        part1 <- "Incidence"
        part2 <- "(Percent)"
        tt <- "lab1"
    }

    # Per 100,000
    if(scale=="Per 100,000") {
        yvar = 'pht'
        part1 <- "Incidence"
        part2 <- "(Per 100,000 population)"
        tt <- "lab1"
    }


    ### Update graph based on choice of log scale
    if(!logScale) {
        part3 <- NULL
    }
    if(logScale) {
        part3 <- "[Log scale]"
    }

    ### Dynamically create Y title
    ytitle <- paste(part1, part2, part3)

    p <- df %>%
        filter(compartment %in% plotvars) %>%
        filter(t <= ndays) %>%
        ggplot(aes(x=date, y=!!as.name(yvar), group = complong)) +
        geom_line(aes(colour=complong)) +
        geom_segment(aes(xend = max(date), yend = !!as.name(yvar)), linetype = 2, colour = 'grey') +
        geom_point(size = 2) +
        geom_label(aes(x = max(date), label = scales::comma(round(!!as.name(yvar), digits=0))), hjust = 0) +
        transition_reveal(t) +
        coord_cartesian(clip = 'off') +
        labs(x = "Date", y = ytitle, color = "Compartment") +
        theme_minimal() +
        theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

    # Add log sclae where neccessary
    if(logScale) {
        p <- p + scale_y_log10()
    }

    anim_save("outfile.gif", animate(p, nframes = 100, update_progress = update_progress), height = '20px', width = '100px')

    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif')






}
