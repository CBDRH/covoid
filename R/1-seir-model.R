
#' Simulate a deterministic SEIR model
#'
#' S = Susceptible
#' E = Exposed (not infectious)
#' I = Infectious
#' R = Removed
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see seir_state0)
#' @param param Model parameters (see seir_param)
#'
#' @return Object of class covoidd and dcm (from the package EpiModels)
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

    # return
    class(mod) = c("covoidd",class(mod))
    mod
}

#' SEIR model parameters
#'
#' Setup function
#'
#' @param R0 Basic reproduction number (S -> E)
#' @param beta Rate of potential new infections per infected (S -> E)
#' @param sigma Inverse of the average length of latent period (E -> I)
#' @param gamma Inverse of the average length of infectious period (I -> R)
#'
#' @return List of SEIR model parameters
#'
#' @export
seir_param <- function(R0,beta,sigma,gamma) {
    # work out missing parameter
    if(missing(R0)) {
        R0 = beta/gamma
    } else if(missing(beta)) {
        beta = gamma*R0
    } else if(!missing(R0) & ! missing(beta)) {
        stopifnot(beta/gamma == R0)
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
#' @param E0 Initial number of exposed
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
        lambda = beta*I

        # differential equations
        dS = -(S/N)*lambda
        dE = (S/N)*lambda - sigma*E
        dI = sigma*E - gamma*I
        dR = gamma*I

        # return
        list(c(dS,dE,dI,dR))
    })
}

