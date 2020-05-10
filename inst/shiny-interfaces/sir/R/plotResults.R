plotResults <- function(df, scale, logScale, plotvars, ndays) {

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
            ggplot(aes(x=date, y=!!as.name(yvar), colour=compartment)) +
            geom_line(size=2, alpha=0.7) +
            scale_x_date(date_labels="%d%b%Y") +
            theme_dark() +
            theme(legend.position = "right", legend.title = element_blank()) +
            guides(col = guide_legend(ncol = 1)) +
            labs(x="Date", y=ytitle) +
            scale_colour_manual(values = compcols, labels=complabels, breaks = c("S", "I", "R")) +
            geom_point_interactive(aes_string(tooltip = tt))

        # Add log sclae where neccessary
        if(logScale) {
            p <- p + scale_y_log10()
        }

        girafe(code = print(p))
}
