
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
#' @return Object of class covoid and dcm (from the package EpiModels)
#'
#' @examples
#' param <- sir_param(R0 = 2.5,gamma = 0.1)
#' state0 <- sir_state0(S = 100,I = 1,R = 0)
#' res <- simulate_sir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"))
#'
#' # if the reproduction number is time varying (e.g. due to control measures)
#' R_t <- function(t) {
#' if (t < 30) 2.5
#' else 1.1
#' }
#' param <- sir_param(R0 = R_t,gamma = 0.1)
#' state0 <- sir_state0(S = 100,I = 1,R = 0)
#' res <- simulate_sir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"))
#'
#' @export
simulate_sir <- function(t,state_t0,param) {
  # assertions
  stopifnot(class(state_t0) == "sir_state0")
  stopifnot(class(param) == "sir_param")

  # simulation
  mod <- deSolve::ode(y=state_t0,
                      times=1:t,
                      func=sir_model,
                      parms=list(gamma=param$gamma,
                                 beta=param$beta,
                                 R0=param$R0))

  # make the output a dataframe
  out <- list(param = param,
              epi = data.frame(mod))
  names(out$epi)[1] = "t"

  # return
  class(out) = c("covoid",class(out))
  out
}

#' SIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> I), can be a function of t.
#' @param beta Rate of potential new infections per infected (S -> I), can be a function of t.
#' Pass either R0 or beta.
#' @param gamma Inverse of the average length of infectious period (I -> R)
#'
#' @return List of SIR model parameters
#'
#' @export
sir_param <- function(R0,beta,gamma) {

  if(missing(R0)) {
    if (is.function(beta)) {
      R0 = function(t) beta(t)/gamma
    } else {
      beta_val = beta
      R0 = function(t) beta_val/gamma
      beta = function(t) beta_val
    }
  } else if(missing(beta)) {
    if (is.function(R0)) {
      beta = function(t) gamma*R0(t)
    } else {
      R0_val = R0
      beta = function(t) gamma*R0_val
      R0 = function(t) R0_val
    }
  } else if(!missing(R0) & ! missing(beta)) {
    stop("Supply either R0 or beta")
  }

  # output with class sir_param
  param = list(R0=R0,beta=beta,gamma=gamma)
  class(param) = "sir_param"

  # return
  param
}

#' SIR model inital state
#'
#' Setup function
#'
#' @param S Initial number of susceptibles
#' @param I Initial number of infected
#' @param R Initial number of removed
#'
#' @return List of SIR model initial states
#'
#' @export
sir_state0 <- function(S,I,R=0) {
  # assertions
  stopifnot(I >= 1)

  # output with class sir_state0
  state0 = c(S=S,I=I,R=R)
  class(state0) = "sir_state0"

  # return
  state0
}

#' SIR differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,I,R)
#' @param param The model parameters (beta,R0,gamma)
#'
#' @return derivatives of SIR model states with respect to time
#'
sir_model <- function(t,state_t0,param) {
  with(as.list(c(state_t0, param)), {

    # population size
    N = S + I + R

    # derived parameters
    lambda = beta(t)*I

    # differential equations
    dS = -(S/N)*lambda
    dI = (S/N)*lambda - gamma*I
    dR = gamma*I

    # return
    list(c(dS,dI,dR),
           N=N)
  })
}

