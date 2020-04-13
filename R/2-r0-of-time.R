#' Add an intervention
#'
#' Use `add_intervention` to model the impact of a epidemic control measure, for
#' example "social distancing", on the trajectory of a epidemic.
#' Is this measure adopted instantly or gradually over a number of units of time?
#'
#' @param R Either a vector of the reproduction number for each step of the period being modeled
#' or a function that returns the reproduction number.
#' @param start The start time of the intervention
#' @param stop The end time of the intervention. For never enter Inf.
#' @param c_reduce The reduction in the proportion of daily contact as a result of
#' this measure
#' @param r_reduce An alternative to `pcontact_reduce`. The reduction in the reproduction
#' number.
#' @param delay The delay until full uptake of the measure.
#' For instant uptake (e.g. as a result of immediate strict enforcement by authorities)
#' enter 0, else some positive number that indicates the time to full uptake.
#' @param delay_dist The distribution of time to full uptake for non-zero delay times.
#' By default an exponential decay form "exp" is assumed where 99% uptake occurs by start+delay.
#' Alteratives include the uniform "unif" distribution where 100% uptake occurs at start+delay..
#'
#'
add_intervention <- function(R,start,stop,c_reduce,r_reduce,delay,delay_dist="exp") {
    times = seq(from = start,to = stop,by = 1)
    c_reduce = rep(c_reduce,times = length(times))
    if (delay != 0) {
        lambda = -log(1-0.99)/delay
        delay_p = pexp(q = times-(start-1),rate = lambda)[1:delay]
        c_reduce[1:delay] = c_reduce*delay_p
    }

    # return
    R

}
