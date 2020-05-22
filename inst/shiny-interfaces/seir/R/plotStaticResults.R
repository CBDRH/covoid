plotStaticResults <- function(df, scale, logScale, plotvars, ndays, xtraC = NULL, xtraP = NULL) {

    # Dynamically set colors, labels and breaks, based on extra specified countries

    if (xtraC!="") {
        x1 <- c('gray90')
        names(x1) = xtraC
        compcols <- c(c("S" = "#2daae2", "E" = "#ff8200", "I" = "#ff635d", "R" = "#1ac987"), x1)

        x2 <- if (xtraP=="") xtraC else xtraP
        names(x2) = xtraC
        complabels <- c(c("S" = "Susceptible", "E" = "Exposed", "I" = "Infectious", "R" = "Recovered"), x2)

        compbreaks <- c(c("S", "E", "I", "R"), xtraC)
    }
    else {
        compcols <- c("S" = "#2daae2", "E" = "#ff8200", "I" = "#ff635d", "R" = "#1ac987")
        complabels <- c("S" = "Susceptible", "E" = "Exposed", "I" = "Infectious", "R" = "Recovered")
        compbreaks <- c("S", "E", "I", "R")
    }

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
        ggplot(aes(x=date, y=!!as.name(yvar), colour=compartment))

    # Comparison country if requested
    if(xtraC != "") {
        dx <- covid19_data %>%
            filter(Province.State==xtraP & Country.Region==xtraC & type=="confirmed")
        p <- p + geom_point(data=dx,
                                        aes(x=date, y=cases,
                                            colour = Country.Region,
                                            tooltip = paste("Cases =", formatC(cases, format="d", big.mark=',')))
        ) +
            geom_line(data=dx, aes(x=date, y=cases, color=Country.Region))
    }

    p <- p +
        geom_line(size=2, alpha=0.7) +
        scale_x_date(date_labels="%d%b") +
        theme_dark() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        guides(col = guide_legend(nrow = 1)) +
        labs(x="Date", y=ytitle) +
        scale_colour_manual(values = compcols, labels = complabels, breaks = compbreaks) +
        scale_y_continuous(labels = scales::comma) +
        geom_point()




    # Add log scale where necessary
    if(logScale) {
        p <- p + scale_y_log10()
    }

    p
}
