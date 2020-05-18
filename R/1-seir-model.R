
#' Simulate a deterministic SEIR model
#'
#' S = Susceptible
#' E = Exposed (non-infectious)
#' I = Infectious
#' R = Removed
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see seir_state0)
#' @param param Model parameters (see seir_param)
#'
#' @return Object of class covoid and dcm (from the package EpiModels)
#'
#' @examples
#' param <- seir_param(R0 = 2.5,gamma = 0.1,sigma=0.1)
#' state0 <- seir_state0(S = 100,E = 1, I = 0,R = 0)
#' res <- simulate_seir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","I","R"))
#'
#' # if the reproduction number is time varying (e.g. due to control measures)
#' R_t <- function(t) {
#' if (t < 30) 2.5
#' else 1.1
#' }
#' param <- seir_param(R0 = R_t,gamma = 0.1,sigma=0.1)
#' state0 <- seir_state0(S = 100,E = 1, I = 0,R = 0)
#' res <- simulate_seir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","I","R"))
#'
#' @export
simulate_seir <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir_state0")
    stopifnot(class(param) == "seir_param")

    # simulation
    mod <- deSolve::ode(y=state_t0,
                        times=1:t,
                        func=seir_model,
                        parms=list(gamma=param$gamma,
                                   beta=param$beta,
                                   sigma=param$sigma,
                                   R0=param$R0))

    # make the output a dataframe
    out <- list(param = param,
                epi = data.frame(mod))
    names(out$epi)[1] = "t"

    # return
    class(out) = c("covoid",class(out))
    out
}

#' SEIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> E), can be a function of t.
#' @param beta Rate of potential new infections per infected (S -> E), can be a function of t.
#' Pass either R0 or beta.
#' @param sigma Inverse of the average length of latent period (E -> I)
#' @param gamma Inverse of the average length of infectious period (I -> R)
#'
#' @return List of SEIR model parameters
#'
#' @export
seir_param <- function(R0,beta,sigma,gamma) {
    # work out missing parameter
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

    # output with class seir_param
    param = list(R0=R0,beta=beta,sigma=sigma,gamma=gamma)
    class(param) = "seir_param"

    # return
    param
}

#' SEIR model inital state
#'
#' Setup function
#'
#' @param S0 Initial number of susceptibles
#' @param E0 Initial number of exposed (non-infectious)
#' @param I0 Initial number of infected
#' @param R0 Initial number of removed
#'
#' @return List of SEIR model initial states
#'
#' @export
seir_state0 <- function(S,E,I=0,R=0) {
    # assertions
    stopifnot(E >= 1)

    # output with class seir_state0
    state0 = c(S=S,E=E,I=I,R=R)
    class(state0) = "seir_state0"

    # return
    state0
}

#' SEIR differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E,I,R)
#' @param param The model parameters (R0,beta,sigma,gamma)
#'
#' @return derivatives of SEIR model states with respect to time
#'
seir_model <- function(t,state_t0,param) {
    with(as.list(c(state_t0, param)), {

        # population size
        N <- S + E + I + R

        # derived parameters
        lambda = beta(t)*I

        # differential equations
        dS = -(S/N)*lambda
        dE = (S/N)*lambda - sigma*E
        dI = sigma*E - gamma*I
        dR = gamma*I

        # return
        list(c(dS,dE,dI,dR))
    })
}

