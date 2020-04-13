#' Simulate a deterministic SEIHR-Q model
#'
#' \itemize{
#' \item S = Susceptible
#' \item E = Exposed (Asymptomatic but infectious)
#' \item I1/Iq1 = First infectious period
#' \item I2/Iq2 = Second infection period
#' \item H/Hq = Hospitalised
#' \item R/Rq/Rh/Rqh = Removed
#' \item F = Case fatality (derived number, not a modelled compartment)
#' }
#'
#' Adapted version of the stochastic individual contact seihrq model outlined in Churches & Jorm (2020).
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see seihrq_state0)
#' @param param Model parameters (see seihrq_param)
#'
#' @return Object of class covoidd and dcm (from the package EpiModels)
#'
#' @section References
#'
#' Churches, T. & Jorm, L. (2020). COVOID: A flexible, freely available stochastic individual contact model for exploring COVID-19 intervention and control strategies (Preprint). 10.2196/preprints.18965.
#'
#' @examples
#' param <- seihrq_param(R0=2.5,sigma=0.3,gamma1=0.3,gamma2=0.3,gamma3=0.3,Qeff=0.5,Heff=0.99,rho=0.1,alpha=0.2,eta=0.01)
#' state0 <- seihrq_state0(S0=100,E0=1)
#' res <- simulate_seihrq(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","Is","Hp","Rc","F"))
#'
#' # if the reproduction number is time varying (e.g. due to control measures)
#' R_t <- function(t) {
#' if (t < 30) 2.5
#' else 1.1
#' }
#' param <- seihrq_param(R0=R_t,sigma=0.3,gamma1=0.3,gamma2=0.3,gamma3=0.3,Qeff=0.5,Heff=0.99,rho=0.1,alpha=0.2,eta=0.01)
#' state0 <- seihrq_state0(S0=100,E0=1)
#' res <- simulate_seihrq(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","E","Is","Hp","Rc","F"))
#'
#' @export
simulate_seihrq <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seihrq_state0")
    stopifnot(class(param) == "seihrq_param")

    # simulation
    param = EpiModel::param.dcm(R0=param$R0,
                                beta=param$beta,
                                sigma=param$sigma,
                                gamma1=param$gamma1,
                                gamma2=param$gamma2,
                                gamma3=param$gamma3,
                                Qeff=param$Qeff,
                                Heff=param$Heff,
                                rho=param$rho,
                                alpha=param$alpha,
                                eta=param$eta)
    init = EpiModel::init.dcm(S=state_t0$S,
                              E=state_t0$E,
                              I1=state_t0$I1,
                              I2=state_t0$I2,
                              H=state_t0$H,
                              Rh=state_t0$Rh,
                              R=state_t0$R,
                              Iq1=state_t0$Iq1,
                              Iq2=state_t0$Iq2,
                              Hq=state_t0$Hq,
                              Rqh=state_t0$Rqh,
                              Rq=state_t0$Rq)
    control = EpiModel::control.dcm(nsteps=t,
                                    dt=0.1,
                                    new.mod=seihrq_model)
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

#' SEIHR-Q model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> E), can be a function of t.
#' @param beta Rate of potential new infections per infected (S -> E), can be a function of t.
#' Pass either R0 or beta.
#' @param sigma Inverse of the average length of latent period (E -> I1)
#' @param gamma1 Inverse of the average length of first infectious period (I1 -> I2 or H)
#' @param gamma2 Inverse of the average length of second infectious period (I2 -> R)
#' @param gamma3 Inverse of the average length of second infectious period (for hospitalised) (H -> R or F)
#' @param Qeff Effect of quarantine on infectiousness, should be in (0,1)
#' @param Heff Effect of hospitalisation on infectiousness, should be in (0,1)
#' @param rho Proportion of people who enter quarantine after exposure, should be in (0,1) (E -> Iq1 or I1)
#' @param alpha Proportion of infected requiring hospitalisation, should be in (0,1) (I1 -> I2 or H)
#' @param eta Case fatality rate, should be in (0,1) (H -> F or Rh). Note F is calculated at each step, there is no dF.
#'
#' @return List of SEIHR-Q model parameters
#'
#' @export
seihrq_param <- function(R0,beta,sigma,gamma1,gamma2,gamma3,Qeff,Heff,rho,alpha,eta) {
    # work out missing parameter
    infectious_period = 1/sigma + 1/gamma1 + (1-alpha)*(1/gamma2) + alpha*1/gamma3
    if(missing(R0)) {
        if (is.function(beta)) {
            R0 = function(t) beta(t)*infectious_period
        } else {
            R0 = function(t) beta*infectious_period
        }
    } else if(missing(beta)) {
        if (is.function(R0)) {
            beta = function(t) R0(t)/infectious_period
        } else {
            beta = function(t) R0/infectious_period
        }
    } else if(!missing(R0) & ! missing(beta)) {
        stop("Supply either R0 or beta")
    }

    # output with class seihrq_param
    param = list(R0=R0,beta=beta,sigma=sigma,gamma1=gamma1,gamma2=gamma2,gamma3=gamma3,
                 Qeff=Qeff,Heff=Heff,rho=rho,alpha=alpha,eta=eta)
    class(param) = "seihrq_param"

    # return
    param
}

#' SEIHR-Q model inital state
#'
#' Setup function
#'
#' @param S0 Initial number of susceptibles
#' @param E0 Initial number of exposed
#' @param I10 Initial number of infected (period 1)
#' @param I20 Initial number of infected (period 2)
#' @param H0 Initial number of hospitalised
#' @param Rh0 Initial number of recovered hospitalisations
#' @param R0 Initial number of recovered
#' @param Iq10 Initial number of infected (period 1) (who had self quarantined)
#' @param Iq20 Initial number of infected (period 2) (who had self quarantined)
#' @param Hq0 Initial number of hospitalised (who had self quarantined)
#' @param Rqh0 Initial number of recovered hospitalisations (who had self quarantined)
#' @param Rq0 Initial number of recovered (who had self quarantined)
#'
#' @return List of SEIHR-Q model initial states
#'
#' @export
seihrq_state0 <- function(S0,E0,I10=0,I20=0,H0=0,Rh0=0,R0=0,Iq10=0,Iq20=0,Hq0=0,Rqh0=0,Rq0=0) {
    # assertions
    stopifnot(E0 >= 1)

    # output with class seihrq_state0
    state0 = list(S=S0,E=E0,I1=I10,I2=I20,H=H0,Rh=Rh0,R=R0,Iq1=Iq10,Iq2=Iq20,Hq=Hq0,Rqh=Rqh0,Rq=Rq0)
    class(state0) = "seihrq_state0"

    # return
    state0
}

#' SEIHR-Q differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E,I1,I2,H,Rh,R,Iq1,Iq2,Hq,Rqh,Rq)
#' @param param The model parameters (R0,beta,sigma,gamma1,gamma2,gamma3,Qeff,Heff,rho,alpha,eta)
#'
#' @return derivatives of SEIHR-Q model states with respect to time
#'
seihrq_model <- function(t,state_t0,param) {
    with(as.list(c(state_t0, param)), {

        # population size
        N <- S + E + I1 + I2 + H + Rh + R + Iq1 + Iq2 + Hq + Rqh + Rq

        # derived parameters
        betaq = Qeff*beta(t)
        betah = Heff*beta(t)
        lambda = beta(t)*(E + I1 + I2) + betaq*(Iq1 + Iq2) + betah*(H + Hq)

        # differential equations
        # general population
        dS = -(S/N)*lambda
        dE = (S/N)*lambda - sigma*E
        dI1 = (1-rho)*sigma*E - gamma1*I1
        dI2 = (1-alpha)*gamma1*I1 - gamma2*I2
        dH = alpha*gamma1*I1 - gamma3*H
        dRh = gamma3*H
        dR = gamma2*I2
        # quarantined population
        dIq1 = (rho)*sigma*E - gamma1*Iq1
        dIq2 = (1-alpha)*gamma1*Iq1 - gamma2*Iq2
        dHq = alpha*gamma1*Iq1 - gamma3*Hq
        dRqh = gamma3*Hq
        dRq = gamma2*Iq2

        # return
        list(c(dS,dE,dI1,dI2,dH,dRh,dR,
               dIq1,dIq2,dHq,dRqh,dRq),
             dS=dS,
             Is=I1+I2+Iq1+Iq2,
             Hp=H+Hq,
             Rc=(1-eta)*(R+Rh+Rq+Rqh),
             F=eta*(R+Rh+Rq+Rqh),
             N=S+E+I1+I2+H+Rh+R+Iq1+Iq2+Hq+Rqh+Rq)
    })
}

