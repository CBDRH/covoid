prepData <- function(df_epi, day1, popsize) {


# Extract the data
df_epi %>%
pivot_longer(-t, names_to = "compartment", values_to = "count") %>%
    mutate(complong = recode(compartment,
                             S = "Susceptible",
                             I = "Infectious",
                             R = "Recovered"),
           date = as.Date(t + day1 - 1, origin = "1970-01-01")) %>%
    group_by(compartment) %>%
    arrange(t) %>%
    mutate(
           pct = 100*count/popsize,
           pht = 100000*count/popsize,
           diff = c(0, diff(count)),
           cum_sum = cumsum(diff),
           cum_pct = cumsum(pct),
           cum_pht = cumsum(pht),
           lab1 = sprintf("<strong>%s</strong><em>%s</em><em>%s</em><br/><strong>%s</strong><strong>%s</strong><em>%s</em><em>%s</em><em>%s</em><br/>%s",
                          format(date, "%d-%b-%y"), " Day ", t,
                          "Count: ", formatC(round(count, digits = 0), format="d", big.mark=','),
                          " (", round(100*count/popsize, digits = 1), "%)",
                          complong) %>% lapply(htmltools::HTML),
           lab2 = sprintf("<strong>%s</strong><em>%s</em><em>%s</em><br/><strong>%s</strong><strong>%s</strong><em>%s</em><em>%s</em><em>%s</em><br/>%s",
                          format(date, "%d-%b-%y"), " Day ", t,
                          "Cuml. count: ", formatC(round(cum_sum, digits = 0), format="d", big.mark=','),
                          " (", round(100*cum_sum/popsize, digits = 1), "%)",
                          complong) %>% lapply(htmltools::HTML)
    ) %>%
        group_by(compartment) %>%
        mutate(
            diff = c(0, diff(count)),
            cuml = cumsum(diff)
        )
}
