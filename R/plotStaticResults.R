plotStaticResults <- function(df, cuml, scale, logScale, plotvars, ndays) {

    compcols <- c("S" = "lightblue", "Itotal" = "red", "Mtotal" = "pink", "H" = "maroon", "Q" = "Purple", "Rtotal" = "lightgreen", "D" = "Black" )
    complabels <- c("S" = "Susceptible", "Itotal" = "Infected", "MTotal" = "Managed", "H" = "Hospitalised", "Q" = "Quarantined", "Rtotal" = "Recovered", "D" = "Case fatality")


    ### Update graph based on choice of cumulative
    if(!cuml) {
        yvar <- "count"
        part1 <- "Incidence"
        tt <- "lab1"
    }
    if(cuml) {
        yvar <- "cum_sum"
        part1 <- "Cumulative incidence"
        tt <- "lab2"
    }

    # Determine scale
    if(scale=="Count") {
        yscale = 1
        part2 = "(Persons)"
    }
    if(scale=="Percent") {
        yscale = 100
        part2 = "(Percent)"
    }
    if(scale=="Per 100,000") {
        yscale = 100000
        part2 = "(Per 100,000 population)"
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
        ggplot(aes(x=date, y=(!!as.name(yvar))/yscale, colour=compartment)) +
        geom_line(size=2, alpha=0.7) +
        scale_x_date(date_labels="%d%b%Y") +
        theme_dark() +
        theme(legend.position = "right", legend.title = element_blank()) +
        guides(col = guide_legend(ncol = 1)) +
        labs(x="Date", y=ytitle) +
        scale_colour_manual(values = compcols, labels=complabels)

    # Add log sclae where neccessary
    if(logScale) {
        p <- p + scale_y_log10()
    }

p
}
