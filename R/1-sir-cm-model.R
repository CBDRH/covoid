
#' Simulate a deterministic SIR model
#'
#' S = Susceptible
#' I = Infectious
#' R = Removed
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?sir_state0)
#' @param param Model parameters (see ?sir_param)
#'
#' @return Object of class covoidd and dcm (from the package EpiModels)
#'
#' @examples
#'
#' cm_oz <- import_contact_matrix("Australia","general")
#' cm_oz <- 0.5*(cm_oz +t(cm_oz))  # while no age groups available
#' param <- sir_c_param(R0 = 2.5,gamma = 0.1,cm=cm_oz)
#' nJ = ncol(cm_oz)
#' S0 = rep(100,nJ)
#' I0 = rep(1,nJ)
#' R0 = rep(0,nJ)
#' state0 <- sir_c_state0(S0 = S0,I0 = I0,R0 = R0)
#' res <- simulate_c_sir(t = 150,state_t0 = state0,param = param)
#' plot(res,y=c("S","I","R"),main="Heterogeneous mixing")
#'
#' # compare with homogeneous
#' param <- sir_param(R0 = 2.5,gamma = 0.1)
#' state0 <- sir_state0(S0 = 1600,I0 = 16,R0 = 0)
#' res <- simulate_sir(t = 150,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"),main="Homogeneous mixing")
#'
#' @export
simulate_c_sir <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "sir_c_state0")
    stopifnot(class(param) == "sir_c_param")

    # simulation
    mod = deSolve::ode(y=state_t0,
                       times=1:t,
                       func=sir_c_model,
                       parms=list(pt=param$pt,
                                  gamma=param$gamma,
                                  cm=param$cm,
                                  pt=param$pt,
                                  dist=param$dist,
                                  intervention=param$intervention))

    # make the output a dataframe
    out = list(param = param,
               epi = data.frame(mod))
    names(out$epi)[1] = "t"

    # return
    class(out) = c("covoidd",class(out))
    out
}


#' SIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> I), can be a function of t.
#' @param gamma Inverse of the average length of infectious period (I -> R)
#' @param cm Contact matrix or a named list of contact matrices where the sum is taken to
#' be all contacts in the population.
#' @param dist Proportion of the population in each age category. Should sum to 1.
#' @param intervention An intervention created using `create_intervention` or named list
#' of interventions with the names matching the list of contact matrices.
#'
#' @return List of SIR model parameters
#'
#' @export
sir_c_param <- function(R0,gamma,cm,dist,intervention=NULL) {
    # assertions
    if (is.list(cm)) {
        stopifnot(!is.null(names(cm)))
        stopifnot(all(sapply(cm,function(x) nrow(x) == ncol(x))))
        stopifnot(all(sapply(cm,function(x) nrow(x) == length(dist))))
        if (!is.null(intervention)) {
            stopifnot(is.list(intervention))
            stopifnot(all(names(intervention) %in% names(cm)))
        }
    } else {
        stopifnot(ncol(cm) == nrow(cm))
        stopifnot(length(dist) == ncol(cm))
        if (!is.null(intervention)) {
            stopifnot("intervention" %in% class(intervention))
        }
    }
    stopifnot(all.equal(sum(dist),1.0))

    # calculate prob transmission
    if (is.list(cm)) {
        cm_gen = Reduce('+', cm)
    } else {
        cm_gen = cm
    }
    J = ncol(cm_gen)
    V = diag(rep(gamma,J))
    F1 = cm_gen
    for (i in 1:J) {
        for (j in 1:J) {
            F1[i,j] = dist[i]/dist[j]*F1[i,j]
        }
    }
    K1 = F1 %*% solve(V)
    l1 = max(Re(eigen(K1)$values))
    pt = R0/l1

    # output with class sir_param
    param = list(R0=R0,gamma=gamma,cm=cm,pt=pt,dist=dist,intervention=intervention)
    class(param) = "sir_c_param"

    # return
    param
}

#' SIR model inital state
#'
#' Setup function
#'
#' @param S0 Initial number of susceptibles
#' @param I0 Initial number of infected
#' @param R0 Initial number of removed
#'
#' @return List of SIR model initial states
#'
#' @export
sir_c_state0 <- function(S0,I0,R0) {
    # assertions
    stopifnot(length(S0) == length(I0))
    stopifnot(length(I0) == length(R0))
    stopifnot(any(I0 >= 1))

    # output with class sir_state0
    state0 = c(S=S0,I=I0,R=R0)
    class(state0) = "sir_c_state0"

    # return
    state0
}




#' SIR with heterogeneous contact differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,I,R)
#' @param param The model parameters (beta,R0,gamma,cm)
#'
#' @return derivatives of SIR model states with respect to time
#'
sir_c_model <- function(t,y,parms) {
    with(as.list(c(y, parms)), {

        # interventions
        cm_cur = cm
        if(!is.null(intervention)) {
            if (is.list(intervention) & !is.data.frame(intervention)) {

                for (nm in names(intervention)) {
                    int = intervention[names(intervention) == nm][[1]]
                    if (t >= int$time[1] && t <= int$time[nrow(int)]) {
                        vals = int$c_reduce[t > int$time-1 & t < int$time+1]
                        int_m = diag(mean(vals),length(dist))
                        cm_cur[names(cm_cur) == nm][[1]] = int_m %*% cm_cur[names(cm_cur) == nm][[1]]
                    }
                    # else leave as is
                }
            } else {
                if (t >= intervention$time[1] && t <= intervention$time[nrow(intervention)]) {
                    vals = intervention$c_reduce[t > intervention$time-1 & t < intervention$time+1]
                    int_m = diag(mean(vals),nrow = length(dist))
                    cm_cur = int_m %*% cm_cur
                }
                # else leave as is
            }
        }

        if (is.list(cm_cur)) {
            cm_cur = Reduce('+', cm_cur)
        }

        # population size
        J = ncol(cm_cur)
        S = y[1:J]
        I = y[(J+1):(2*J)]
        R = y[(2*J+1):(3*J)]
        N = S + I + R

        # derived parameters
        # none

        # differential equations
        dS = numeric(length=J)
        dI = numeric(length=J)
        dR = numeric(length=J)
        for (i in 1:J) {
            dS[i] = -(S[i])*sum(pt*cm_cur[i,]*(I/N))
            dI[i] = (S[i])*sum(pt*cm_cur[i,]*(I/N)) - gamma*I[i]
            dR[i] = gamma*I[i]
        }

        # return
        list(c(dS,dI,dR),
             S=sum(S),
             I=sum(I),
             R=sum(R),
             Ntotal=sum(S) + sum(I) + sum(R))
    })
}
