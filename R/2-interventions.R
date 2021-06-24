#' Contact intervention
#'
#' Use `contact_intervention` to model the impact of a epidemic control measure, for
#' example "social distancing", on the trajectory of a epidemic.
#' Is this measure adopted instantly or gradually over a number of units of time?
#'
#' @param start The start time of the intervention
#' @param stop The end time of the intervention.
#' @param reduce The relative reduction in the proportion of daily contacts
#' as a result of this measure, e.g. enter 0.8 to indicate contact has been reduced to 80% of pre-intervention
#' levels
#' @param delay The delay until full uptake of the measure.
#' For instant uptake (e.g. as a result of immediate strict enforcement by authorities)
#' enter 0, else some positive number that indicates the time to full uptake.
#' @param delay_dist The distribution of time to full uptake for non-zero delay times.
#'
#' @examples
#'
#' @export
contact_intervention <- function(start,stop,reduce,start_delay=0,start_delay_dist="logis",
                                stop_delay=0,stop_delay_dist="logis") {
    create_intervention(start=start,
                        stop=stop,
                        reduce=reduce,
                        type="contact",
                        start_delay=start_delay,
                        start_delay_dist=start_delay_dist,
                        stop_delay=stop_delay,
                        stop_delay_dist=stop_delay_dist)
}


#' Transmission intervention
#'
#'
#' Use `transmission_intervention` to model the impact of a epidemic control measure, for
#' example wearing of PPE, on the trajectory of a epidemic.
#' Is this measure adopted instantly or gradually over a number of units of time?
#'
#' @param start The start time of the intervention
#' @param stop The end time of the intervention.
#' @param reduce The relative reduction in the probability of transmission
#' as a result of this measure, e.g. enter 0.8 to indicate contact has been reduced to 80% of pre-intervention
#' levels
#' @param delay The delay until full uptake of the measure.
#' For instant uptake (e.g. as a result of immediate strict enforcement by authorities)
#' enter 0, else some positive number that indicates the time to full uptake.
#' @param delay_dist The distribution of time to full uptake for non-zero delay times.
#'
#' @examples
#'
#' @export
transmission_intervention <- function(start,stop,reduce,start_delay=0,start_delay_dist="logis",
                                 stop_delay=0,stop_delay_dist="logis") {
    create_intervention(start=start,
                        stop=stop,
                        reduce=reduce,
                        type="transmission",
                        start_delay=start_delay,
                        start_delay_dist=start_delay_dist,
                        stop_delay=stop_delay,
                        stop_delay_dist=stop_delay_dist)
}


#' Create an intervention
#'
#' @description
#' Use `create_intervention` to model the impact of a epidemic control measure, for
#' example "social distancing", on the trajectory of a epidemic.
#' Is this measure adopted instantly or gradually over a number of units of time?
#'
#' @param start The start time of the intervention
#' @param stop The end time of the intervention.
#' @param reduce The relative reduction in the proportion of daily contacts or transmission probability
#' as a result of this measure, e.g. enter 0.8 to indicate contact has been reduced to 80% of pre-intervention
#' levels
#' @param type Either "contact" or "transmission". Does the intervention reduce contact between individuals
#' (e.g. physical distancing) or reduce the transmission probability (e.g. PPE) given contact?
#' @param delay The delay until full uptake of the measure.
#' For instant uptake (e.g. as a result of immediate strict enforcement by authorities)
#' enter 0, else some positive number that indicates the time to full uptake.
#' @param delay_dist The distribution of time to full uptake for non-zero delay times.
#' By default an exponential decay form "exp" is assumed where 99% uptake occurs by start+delay.
#' Alteratives include the uniform "unif" distribution where 100% uptake occurs at start+delay..
#'
create_intervention <- function(start,stop,reduce,type,start_delay=0,start_delay_dist="logis",
                             stop_delay=0,stop_delay_dist="logis") {
    stopifnot(reduce > 0 & reduce < 1)
    stopifnot(start_delay >= 0)
    stopifnot(stop_delay >= 0)
    stopifnot(type %in% c("contact","transmission"))

    times <- seq(from = start,to = stop+stop_delay-1,by = 1)
    reduce <- rep(reduce,times = length(times))

    if (start_delay != 0) {
        delay_p <- switch(start_delay_dist,
                         exp=exp_decay(start_delay,start_stop="start"),
                         logis=logis_decay(start_delay,start_stop="start"))
        vals <- reduce[1:start_delay]
        reduce[1:start_delay] = vals + (1-vals)*delay_p
    }

    if (stop_delay != 0) {
        delay_p <- switch(stop_delay_dist,
                         exp=exp_decay(start_delay,start_stop="stop"),
                         logis=logis_decay(start_delay,start_stop="stop"))
        offset <- stop-start+1
        vals <- reduce[offset:(offset+stop_delay-1)]
        reduce[offset:(offset+stop_delay-1)] <- vals + (1-vals)*delay_p
    }

    # return
    reduce <- data.frame(time = start:(stop+stop_delay-1), reduce = reduce)
    class(reduce) <- c(class(reduce),"intervention")
    attr(reduce, "type") <- type
    reduce
}


#' Exponential decay for interventions
#'
#' @param delay delay until start of intervention
#' @param start_stop whether at beginning or end of intervention
#'
exp_decay <- function(delay,start_stop) {
    lambda <- -log(1-0.99)/delay
    if(start_stop == "start") {
        delay_p <- rev(pexp(q = 1:delay,rate = lambda,lower.tail = TRUE))
    } else if(start_stop == "stop") {
        delay_p <- pexp(q = 1:delay,rate = lambda,lower.tail = TRUE)
    }
    delay_p
}


#' Logistic decay for interventions
#'
#' @param delay delay until start of intervention
#' @param start_stop whether at beginning or end of intervention
#'
logis_decay <- function(delay,start_stop) {
    s <- (delay/2)/log(0.99/(1-0.99))
    if(start_stop == "start") {
        delay_p <- rev(plogis(q = 1:delay,location=delay/2,scale=s,lower.tail = TRUE))
    } else if(start_stop == "stop") {
        delay_p <- plogis(q = 1:delay,location=delay/2,scale=s,lower.tail = TRUE)
    }
    delay_p
}

#' Calculate current contact matrix
#'
#' @param cm ...
#' @param intervention ...
#' @param t ...
#'
#'
calculate_current_cm <- function(cm,intervention,t,dist) {
    # interventions
    cm_cur <- cm
    if(!is.null(intervention)) {
        if (is.list(intervention) & !is.data.frame(intervention)) {

            for (nm in names(intervention)) {
                int <- intervention[names(intervention) == nm][[1]]
                if (t >= int$time[1] && t <= int$time[nrow(int)]) {
                    vals <- int$reduce[t > int$time-1 & t < int$time+1]
                    int_m <- diag(mean(vals),length(dist))
                    cm_cur[names(cm_cur) == nm][[1]] = int_m %*% cm_cur[names(cm_cur) == nm][[1]]
                }
                # else leave as is
            }
        } else {
            if (t >= intervention$time[1] && t <= intervention$time[nrow(intervention)]) {
                vals <- intervention$reduce[t > intervention$time-1 & t < intervention$time+1]
                int_m <- diag(mean(vals),nrow = length(dist))
                cm_cur <- int_m %*% cm_cur
            }
            # else leave as is
        }
    }

    if (is.list(cm_cur)) {
        cm_cur <- Reduce('+', cm_cur)
    }
    cm_cur
}


check_if_mult_interventions <- function(x) {
    sum(sapply(x,function(x) sum("intervention" %in% class(x))))
}


#' Calculate current probability of transmission
#'
#' @param pt ...
#' @param intervention ...
#' @param t ...
#'
#'
calculate_current_pt <- function(pt,intervention,t) {

    pt_cur <- pt
        if(!is.null(intervention)) {
            if (t >= intervention$time[1] && t <= intervention$time[nrow(intervention)]) {
                val <- intervention$reduce[t > intervention$time-1 & t < intervention$time+1]
                pt_cur <- pt_cur * val
            }
            # else leave as is
        }
    pt_cur
}

#' Apply an intervention to a contact matrix or probability of transmission
#'
#' @param x contact matrix or probability of transmission
#' @param reduce amount by which we reduce the parameter under the intervention
#' @param dist distribution of groups in population
#'
apply_intervention <- function(x,reduce,dist) UseMethod("apply_intervention")

#' Apply an intervention to a contact matrix or probability of transmission
#'
#' @inheritParams apply_intervention
apply_intervention.default <- function(x,reduce,dist) x*reduce

#' Apply an intervention to a contact matrix or probability of transmission
#'
#' @inheritParams apply_intervention
apply_intervention.contact_matrix <- function(x,reduce,dist) {
    vals <- reduce
    int_m <- diag(mean(vals),nrow = length(dist))
    x <- int_m %*% x
    x
}


#' Calculate the effect of and update a reactive intervention at each time step
#'
#'
#' @param x a contact matrix or the probability of transmission
#' @param intervention a reactive intervention
#' @param incRows the number of cases per group used to define the intervention
#' beginning and ending
#' @param dist population distribution of the groups
#'
#'
calculate_reactive <- function(x,intervention,incRows,dist=NULL) {
    x_cur <- x
    infect <- sum(incRows)
    new_intervention <- intervention
    if(!is.null(intervention)) {
        if ((infect > intervention$threshold) & (intervention$state$inplace == FALSE)) {
            new_intervention$state$inplace <- TRUE
            new_intervention$state$days0cases <- 0
            x_cur <- apply_intervention(x_cur,intervention$reduce,dist)
        } else if (intervention$state$inplace == TRUE & (infect > intervention$state$lowerbound)) {
            new_intervention$state$days0cases <- 0
            x_cur <- apply_intervention(x_cur,intervention$reduce,dist)
        } else if ((intervention$state$inplace == TRUE) & (infect <= (intervention$state$lowerbound - 2*.Machine$double.eps))) {
            new_intervention$state$days0cases <- new_intervention$state$days0cases + 1
            if (new_intervention$state$days0cases >= intervention$state$length) {
                # should we turn off the intervention?
                new_intervention$state$inplace <- FALSE
                x_cur <- x_cur
            } else {
                x_cur <- apply_intervention(x_cur,intervention$reduce,dist)
            }
        } else {
            # else leave as is
            x_cur <- x_cur
        }
    }
    list(param=x_cur,intervention=new_intervention)
}


#' Reactive intervention
#'
#' @param threshold threshold (I - prevalence) for an intervention to commence
#' @param reduce reduction in contact or transmission probability
#' @param state state of the intervention: see `reactive_state`
#'
#' @export
reactive_intervention <- function(threshold,reduce,state) {
    # check arguments
    int <- list(threshold=threshold,reduce=reduce,state=state)
    class(int) <- c(class(int),"intervention")
    int
}

#' Reactive state
#' Set parameters for reactive interventions
#'
#' @param inplace is the intervention in place (logical)
#' @param length length+1 of time the intervention should remain in place after
#' the lower bound condition is met
#' @param lowerbound the number of case (I - prevalence) at which the intervention
#' should continue for `length` units of time
#'
#' @export
reactive_state <- function(inplace=FALSE,length=7,lowerbound=0) {
    list(days0cases=NA,inplace=inplace,length=length,lowerbound=lowerbound)
}

#' #' Calculate probability of transmission as a reactive function of incidence
#' #'
#' #' @param pt ...
#' #' @param intervention ...
#' #' @param incRows ...
#' #'
#' #'
#' calculate_reactive_pt <- function(pt, intervention, incRows){
#'     pt_cur <- pt
#'     infect <- sum(incRows)
#'     new_intervention <- intervention
#'     if(!is.null(intervention)) {
#'         if ((infect > intervention$threshold) & (intervention$state$inplace == FALSE)) {
#'             new_intervention$state$inplace <- TRUE
#'             new_intervention$state$days0cases <- 0
#'             pt_cur <- pt_cur * intervention$reduce
#'         } else if (intervention$state$inplace == TRUE & (infect > intervention$state$lowerbound)) {
#'             new_intervention$state$days0cases <- 0
#'             pt_cur <- pt_cur * intervention$reduce
#'         } else if ((intervention$state$inplace == TRUE) & (infect <= (intervention$state$lowerbound - 2*.Machine$double.eps))) {
#'             new_intervention$state$days0cases <- new_intervention$state$days0cases + 1
#'             if (new_intervention$state$days0cases >= intervention$state$length) {
#'                 # should we turn off the intervention?
#'                 new_intervention$state$inplace <- FALSE
#'                 pt_cur <- pt_cur
#'             } else {
#'                 pt_cur <- pt_cur * intervention$reduce
#'             }
#'         } else {
#'             # else leave as is
#'             pt_cur <- pt_cur
#'         }
#'     }
#'     list(pt_cur=pt_cur,intervention=new_intervention)
#' }


#' #' Calculate current contact matrix
#' #'
#' #' @param cm ...
#' #' @param intervention ...
#' #' @param t ...
#' #'
#' #'
#' calculate_reactive_cm <- function(cm,intervention,incRows,dist) {
#'     # interventions
#'     cm_cur <- cm
#'     infect <- sum(incRows)
#'     new_intervention <- intervention
#'     if(!is.null(intervention)) {
#'         if ((infect > intervention$threshold) & (intervention$state$inplace == FALSE)) {
#'             # update reactive
#'             new_intervention$state$inplace <- TRUE
#'             new_intervention$state$days0cases <- 0
#'             # apply intervention
#'             vals <- intervention$reduce
#'             int_m <- diag(mean(vals),nrow = length(dist))
#'             cm_cur <- int_m %*% cm_cur
#'
#'         } else if (intervention$state$inplace == TRUE & (infect > intervention$state$lowerbound)) {
#'             # update reactive
#'             new_intervention$state$days0cases <- 0
#'             # apply intervention
#'             vals <- intervention$reduce
#'             int_m <- diag(mean(vals),nrow = length(dist))
#'             cm_cur <- int_m %*% cm_cur
#'
#'         } else if ((intervention$state$inplace == TRUE) & (infect <= (intervention$state$lowerbound - 2*.Machine$double.eps))) {
#'             new_intervention$state$days0cases <- new_intervention$state$days0cases + 1
#'             if (new_intervention$state$days0cases >= intervention$state$length) {
#'                 # should we turn off the intervention?
#'                 new_intervention$state$inplace <- FALSE
#'                 # apply no intervention
#'                 cm_cur <- cm_cur
#'             } else {
#'                 # update reactive
#'                 vals <- intervention$reduce
#'                 int_m <- diag(mean(vals),nrow = length(dist))
#'                 # apply intervention
#'                 cm_cur <- int_m %*% cm_cur
#'             }
#'
#'         } else {
#'             # else leave as is
#'             cm_cur <- cm_cur
#'         }
#'     }
#'     list(cm_cur=cm_cur,intervention=new_intervention)
#' }

