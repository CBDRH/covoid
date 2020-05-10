
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
#' state0 <- seir_state0(S0 = 100,E0 = 1, I0 = 0,R0 = 0)
#' res <- simulate_seir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","I","R"))
#'
#' # if the reproduction number is time varying (e.g. due to control measures)
#' R_t <- function(t) {
#' if (t < 30) 2.5
#' else 1.1
#' }
#' param <- seir_param(R0 = R_t,gamma = 0.1,sigma=0.1)
#' state0 <- seir_state0(S0 = 100,E0 = 1, I0 = 0,R0 = 0)
#' res <- simulate_seir(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","I","R"))
#'
#' @export
simulate_seir <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir_state0")
    stopifnot(class(param) == "seir_param")

    # simulation
    param = EpiModel::param.dcm(gamma=param$gamma,
                                beta=param$beta,
                                sigma=param$sigma,
                                R0=param$R0)
    init = EpiModel::init.dcm(S=state_t0$S,
                              E=state_t0$E,
                              I=state_t0$I,
                              R=state_t0$R)
    control = EpiModel::control.dcm(nsteps=t,
                                    dt=0.1,
                                    new.mod=seir_model)
    mod = EpiModel::dcm(param,init,control)

    # make the output a dataframe
    epi_tmp = lapply(mod$epi, function(x) x$run1)
    mod$epi = data.frame(epi_tmp)
    mod$epi$t = mod$control$timesteps
    # keep the integer time steps
    mod$epi = mod$epi[mod$epi$t == round(mod$epi$t),]
    rownames(mod$epi) <- NULL

    # return
    class(mod) = c("covoid",class(mod))
    mod
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
seir_state0 <- function(S0,E0,I0=0,R0=0) {
    # assertions
    stopifnot(E0 >= 1)

    # output with class seir_state0
    state0 = list(S=S0,E=E0,I=I0,R=R0)
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

