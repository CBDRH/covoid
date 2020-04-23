prepData <- function(df_epi, day1, popsize) {


# Extract the data
pivot_longer(df_epi, -t, names_to = "compartment", values_to = "count") %>%
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
           date = as.Date(t + day1 - 1, origin = "1970-01-01")) %>%
    group_by(compartment) %>%
    arrange(t) %>%
    mutate(cum_sum = cumsum(count)) %>%
    mutate(lab1 = sprintf("<strong>%s</strong><em>%s</em><em>%s</em><br/><strong>%s</strong><strong>%s</strong><em>%s</em><em>%s</em><em>%s</em><br/>%s",
                          format(date, "%d-%b-%y"), " Day ", t,
                          "Count: ", formatC(round(count, digits = 0), format="d", big.mark=','),
                          " (", round(100*count/popsize, digits = 1), "%)",
                          complong) %>% lapply(htmltools::HTML)) %>%
    mutate(lab2 = sprintf("<strong>%s</strong><em>%s</em><em>%s</em><br/><strong>%s</strong><strong>%s</strong><em>%s</em><em>%s</em><em>%s</em><br/>%s",
                          format(date, "%d-%b-%y"), " Day ", t,
                          "Cuml. count: ", formatC(round(cum_sum, digits = 0), format="d", big.mark=','),
                          " (", round(100*cum_sum/popsize, digits = 1), "%)",
                          complong) %>% lapply(htmltools::HTML))
}
