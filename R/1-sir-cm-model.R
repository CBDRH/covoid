
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
#' param <- sir_c_param(R0 = 2.5,gamma = 0.1,cm=cm_oz)
#' nJ = ncol(cm_oz)
#' S0 = rep(100,nJ)
#' I0 = rep(1,nJ)
#' R0 = rep(0,nJ)
#' state0 <- sir_c_state0(S0 = S0,I0 = I0,R0 = R0)
#' res <- simulate_c_sir(t = 100,state_t0 = state0,param = param)
#'
#' @export
simulate_c_sir <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "sir_c_state0")
    stopifnot(class(param) == "sir_c_param")
    stopifnot(ncol(param$cm) == length(S0))

    # simulation
    mod = deSolve::ode(y=state0,
                       times=1:t,
                       func=sir_c_model,
                       parms=list(pt=param$pt,
                                  gamma=param$gamma,
                                  cm=param$cm))

    # make the output a dataframe
    out = list(param = param,
               epi = data.frame(mod))

    # return
    class(mod) = c("covoid")
    mod
}


#' SIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> I), can be a function of t.
#' @param gamma Inverse of the average length of infectious period (I -> R)
#' @param cm Contact matrix
#'
#' @return List of SIR model parameters
#'
#' @export
sir_c_param <- function(R0,gamma,cm) {
    # assertions
    stopifnot(ncol(cm) == nrow(cm))

    # calculate prob transmission
    lambda_c = max(Re(eigen(cm*(1/gamma))$values))
    pt = R0/lambda_c

    # output with class sir_param
    param = list(R0=R0,gamma=gamma,cm=cm,pt=pt)
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
sir_c_model <- function(t,state_t0,param) {
    with(as.list(c(state_t0, param)), {

        # population size
        J = ncol(cm)
        S = state_t0[1:J]
        I = state_t0[(J+1):(2*J)]
        R = state_t0[(2*J+1):(3*J)]
        N = S + I + R

        # derived parameters
        lambda = numeric(length=J)
        for (i in 1:J) {
            lambda[i] = sum(pt*cm[i,]*I)
        }

        # differential equations
        dS = numeric(length=J)
        dI = numeric(length=J)
        dR = numeric(length=J)
        for (i in 1:J) {
            dS[i] = -(S[i]/N[i])*lambda[i]
            dI[i] = (S[i]/N[i])*lambda[i] - gamma*I[i]
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
