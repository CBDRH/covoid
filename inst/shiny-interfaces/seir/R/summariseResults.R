# Summarise model results

summariseResults <- function(epi, state0, param, dates, ig_t = NULL, ig_c = NULL, is_c = NULL, iw_c = NULL, ih_c = NULL){

summary <- c(

    format(dates[1], "%d %b %Y"), # Start
    format(dates[2], "%d %b %Y"), # End
    formatC(as.numeric(dates[2] - dates[1]), format="d", big.mark=','), # Days
    formatC(sum(state0[1:(length(state0)/3)]), format="d", big.mark=','), # Susceptible
    formatC(sum(state0[(1+(length(state0)/3)):(2*(length(state0)/3))]), format="d", big.mark=','), # Infectious
    formatC(sum(state0[(1+2*(length(state0)/3)):(length(state0))]), format="d", big.mark=','), # Recovered
    param$R0, # R0
    formatC(round(1/param$gamma, digits=1), format="d", big.mark=','), # Infectious Days
    sparkline::spk_chr(ig_t[[2]], chartRangeMin = 0, chartRangeMin = 1), # Interventions on transmission probability
    sparkline::spk_chr(ig_c[[2]], chartRangeMin = 0, chartRangeMin = 1), # Intervention on contact (General)
    sparkline::spk_chr(is_c[[2]], chartRangeMin = 0, chartRangeMin = 1), # Intervention on contact (School)
    sparkline::spk_chr(iw_c[[2]], chartRangeMin = 0, chartRangeMin = 1), # Intervention on contact (Work)
    sparkline::spk_chr(ih_c[[2]], chartRangeMin = 0, chartRangeMin = 1), # Intervention on contact (Home)
    format(dates[1] + epi$t[epi$I==max(epi$I)], "%d %b %Y"), # Peak
    formatC(max(epi$I), format="d", big.mark=',') # Number infectious at peak
)

summary
}
