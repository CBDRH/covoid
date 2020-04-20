#' Add an intervention
#'
#' Use `add_intervention` to model the impact of a epidemic control measure, for
#' example "social distancing", on the trajectory of a epidemic.
#' Is this measure adopted instantly or gradually over a number of units of time?
#'
#' @param R Either a vector of the (effective) reproduction number for each time step of the period being modeled
#' or a function that returns the reproduction number.
#' @param start The start time of the intervention
#' @param stop The end time of the intervention.
#' @param c_reduce The reduction in the proportion of daily contact as a result of
#' this measure
#' @param r_reduce An alternative to `pcontact_reduce`. Not implemented!
#' The expected reduction in the effective reproduction number.
#' @param delay The delay until full uptake of the measure.
#' For instant uptake (e.g. as a result of immediate strict enforcement by authorities)
#' enter 0, else some positive number that indicates the time to full uptake.
#' @param delay_dist The distribution of time to full uptake for non-zero delay times.
#' By default an exponential decay form "exp" is assumed where 99% uptake occurs by start+delay.
#' Alteratives include the uniform "unif" distribution where 100% uptake occurs at start+delay..
#'
#' @examples
#' # function as input
#' R <- function(t) 2.5
#' R_updated <- add_intervention(R,5,25,0.5,start_delay=10,stop_delay=10)
#' R_updated(9)
#' plot(1:50,sapply(1:50,R_updated))  # the function is not vectorised
#'
#' # vector as input
#' R <- rep(2.5,30)
#' R_updated <- add_intervention(R,5,25,0.5,delay=10)
#' R_updated[9]
#' plot(1:30,R_updated)
#'
#' @export
add_intervention <- function(R,start,stop,c_reduce,r_reduce,start_delay,start_delay_dist="logis",
                             stop_delay,stop_delay_dist="logis") {
    stopifnot(c_reduce > 0 & c_reduce < 1)
    stopifnot(start_delay >= 0)
    stopifnot(stop_delay >= 0)

    times = seq(from = start,to = stop+stop_delay-1,by = 1)
    c_reduce = rep(c_reduce,times = length(times))

    if (start_delay != 0) {
        delay_p = switch(start_delay_dist,
                         exp=exp_decay(start_delay,start_stop="start"),
                         logis=logis_decay(start_delay,start_stop="start"))
        vals = c_reduce[1:start_delay]
        c_reduce[1:start_delay] = vals + (1-vals)*delay_p
    }

    if (stop_delay != 0) {
        delay_p = switch(stop_delay_dist,
                         exp=exp_decay(start_delay,start_stop="stop"),
                         logis=logis_decay(start_delay,start_stop="stop"))
        offset = stop-start+1
        vals = c_reduce[offset:(offset+stop_delay-1)]
        c_reduce[offset:(offset+stop_delay-1)] = vals + (1-vals)*delay_p
    }

    if(is.function(R)) {
        # return objects
        Rnew = function(t) {
            if(t >= start & t <= (stop+stop_delay-1)) {
                R(t)*(c_reduce[t-start+1])
            } else {
                R(t)
            }
        }
    } else {
        Rnew = R
        Rnew[start:(stop+stop_delay)] = R[start:(stop+stop_delay)]*(1-c_reduce)
    }

    # return
    Rnew
}

#' Exponential decay for interventions
#'
#' @param delay delay until start of intervention
#' @param start_stop whether at beginning or end of intervention
#'
exp_decay <- function(delay,start_stop) {
    lambda = -log(1-0.99)/delay
    if(start_stop == "start") {
        delay_p = rev(pexp(q = 1:delay,rate = lambda,lower.tail = TRUE))
    } else if(start_stop == "stop") {
        delay_p = pexp(q = 1:delay,rate = lambda,lower.tail = TRUE)
    }
    delay_p
}

#' Logistic decay for interventions
#'
#' @param delay delay until start of intervention
#' @param start_stop whether at beginning or end of intervention
#'
logis_decay <- function(delay,start_stop) {
    s = (delay/2)/log(0.99/(1-0.99))
    if(start_stop == "start") {
        delay_p = rev(plogis(q = 1:delay,location=delay/2,scale=s,lower.tail = TRUE))
    } else if(start_stop == "stop") {
        delay_p = plogis(q = 1:delay,location=delay/2,scale=s,lower.tail = TRUE)
    }
    delay_p
}
