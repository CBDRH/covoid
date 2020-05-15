
#' Simulate a deterministic age structured SEIR model
#'
#' S = Susceptible
#' E = Exposed (incubating)
#' I = Infectious
#' R = Removed (recovered or died)
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?seir_state0)
#' @param param Model parameters (see ?seir_param)
#'
#' @return Object of class covoidd and dcm (from the package EpiModels)
#'
#' @examples
#'
#' cm_oz <- import_contact_matrix("Australia","general")
#' dist_oz <- import_age_distribution("Australia")
#' param <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,cm=cm_oz,dist=dist_oz)
#' nJ = ncol(cm_oz)
#' S = rep(100,nJ)
#' E = rep(1,nJ)
#' I = rep(1,nJ)
#' R = rep(0,nJ)
#' state0 <- seir_c_state0(S = S,E = E,I = I,R = R)
#' res <- simulate_seir_c(t = 150,state_t0 = state0,param = param)
#' plot(res,y=c("S","E","I","R"),main="Heterogeneous mixing")
#'
#' @export
simulate_seir_c <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir_c_state0")
    stopifnot(class(param) == "seir_c_param")

    # simulation
    mod = deSolve::ode(y=state_t0,
                       times=1:t,
                       func=seir_c_model,
                       parms=list(pt=param$pt,
                                  sigma=param$sigma,
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
    class(out) = c("covoid",class(out))
    out
}


#' Age structured SEIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> E).
#' @param sigma Inverse of the average length of the incubation period (E -> I)
#' @param gamma Inverse of the average length of infectious period (I -> R)
#' @param cm Contact matrix or a named list of contact matrices where the sum is taken to
#' be all contacts in the population.
#' @param dist Proportion of the population in each age category. Should sum to 1.
#' @param intervention An intervention created using `create_intervention` or named list
#' of interventions with the names matching the list of contact matrices.
#'
#' @return List of SEIR model parameters
#'
#' @export
seir_c_param <- function(R0,sigma,gamma,cm,dist,intervention=NULL) {
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

    # output with class seir_param
    param = list(R0=R0,sigma=sigma,gamma=gamma,cm=cm,pt=pt,dist=dist,intervention=intervention)
    class(param) = "seir_c_param"

    # return
    param
}

#' SEIR model inital state
#'
#' Setup function
#'
#' @param S Initial number of susceptibles
#' @param E Initial number of incubating
#' @param I Initial number of infectious
#' @param R Initial number of removed
#'
#' @return List of SEIR model initial states
#'
#' @export
seir_c_state0 <- function(S,E,I,R) {
    # assertions
    stopifnot(length(S) == length(E))
    stopifnot(length(E) == length(I))
    stopifnot(length(I) == length(R))
    stopifnot(any(I >= 0))

    # output with class seir_state0
    state0 = c(S=S,E=E,I=I,R=R)
    class(state0) = "seir_c_state0"

    # return
    state0
}



#' SEIR with heterogeneous contact differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E,I,R)
#' @param param The model parameters (R0,sigma,gamma,cm,dist,intervention)
#'
#' @return derivatives of SEIR model states with respect to time
#'
seir_c_model <- function(t,y,parms) {
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
        E = y[(J+1):(2*J)]
        I = y[(2*J+1):(3*J)]
        R = y[(3*J+1):(4*J)]
        N = S + E + I + R

        # derived parameters
        # none

        # differential equations
        dS = numeric(length=J)
        dE = numeric(length=J)
        dI = numeric(length=J)
        dR = numeric(length=J)
        for (i in 1:J) {
            dS[i] = -(S[i])*sum(pt*cm_cur[i,]*(I/N))
            dE[i] = (S[i])*sum(pt*cm_cur[i,]*(I/N)) - sigma*E[i]
            dI[i] = sigma*E[i] - gamma*I[i]
            dR[i] = gamma*I[i]
        }

        # return
        list(c(dS,dE,dI,dR),
             S=sum(S),
             E=sum(E),
             I=sum(I),
             R=sum(R),
             Ntotal=sum(S) + sum(E) + sum(I) + sum(R))
    })
}
