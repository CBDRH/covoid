
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
#' param <- sir_param(R0 = 2.5,gamma = 0.1)
#' state0 <- sir_state0(S0 = 100,I0 = 1,R0 = 0)
#' res <- simulate_sir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"))
#'
#' # if the reproduction number is time varying (e.g. due to control measures)
#' R_t <- function(t) {
#' if (t < 30) 2.5
#' else 1.1
#' }
#' param <- sir_param(R0 = R_t,gamma = 0.1)
#' state0 <- sir_state0(S0 = 100,I0 = 1,R0 = 0)
#' res <- simulate_sir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"))
#'
#' @export
simulate_sir <- function(t,state_t0,param) {
  # assertions
  stopifnot(class(state_t0) == "sir_state0")
  stopifnot(class(param) == "sir_param")

  # simulation
  param = EpiModel::param.dcm(gamma=param$gamma,
                              beta=param$beta,
                              R0=param$R0)
  init = EpiModel::init.dcm(S=state_t0$S,
                            I=state_t0$I,
                            R=state_t0$R)
  control = EpiModel::control.dcm(nsteps=t,
                                  dt=0.1,
                                  new.mod=sir_model)
  mod = EpiModel::dcm(param,init,control)

  # make the output a dataframe
  epi_tmp = lapply(mod$epi, function(x) x$run1)
  mod$epi = data.frame(epi_tmp)
  mod$epi$t = mod$control$timesteps
  # keep the integer time steps
  mod$epi = mod$epi[mod$epi$t == round(mod$epi$t),]
  rownames(mod$epi) <- NULL

  # return
  class(mod) = c("covoidd",class(mod))
  mod
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
      R0 = function(t) beta/gamma
    }
  } else if(missing(beta)) {
    if (is.function(R0)) {
      beta = function(t) gamma*R0(t)
    } else {
      beta = function(t) gamma*R0
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
#' @param S0 Initial number of susceptibles
#' @param I0 Initial number of infected
#' @param R0 Initial number of removed
#'
#' @return List of SIR model initial states
#'
#' @export
sir_state0 <- function(S0,I0,R0=0) {
  # assertions
  stopifnot(I0 >= 1)

  # output with class sir_state0
  state0 = list(S=S0,I=I0,R=R0)
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
         dS=dS,
           N=N)
  })
}

