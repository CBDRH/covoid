#' Simulate a deterministic SEIR model with quarantine and hospitalisation (SEIR-QH)
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' \itemize{
#' \item S = Susceptible
#' \item Eq1/Eq1 = First latent period (asymptomatic; uninfectious)
#' \item E2/Eq2 = Second latent period (asymptomatic; infectious)
#' \item I2/Iq2 = Second infectious period (symptomatic; infectious)
#' \item I2/Iq2 = Third infection period (symptomatic; infectious)
#' \item H/Hq = Hospitalised
#' \item R/Rq/Rh/Rqh = Removed
#' \item F = Case fatality (derived number, not a modelled compartment)
#' }
#'
#' Adapted version of the stochastic individual contact seir1 model outlined in Churches & Jorm (2020).
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?seir1_state0)
#' @param param Model parameters (see ?seir1_param)
#'
#' @return Object of class covoid
#'
#' @section References
#' Churches, T. & Jorm, L. (2020). COVOID: A flexible, freely available stochastic individual contact model for exploring COVID-19 intervention and control strategies (Preprint). 10.2196/preprints.18965.
#'
#' @examples
#' state0 <- seir1_state0(S = 1e5, E1 = 90, E2 = 40)
#' param <- seir1_param(R0=2.5,sigma1=0.2,sigma2=0.2,gamma1=0.2,gamma2=0.2,gamma3=0.2,
#'                      Qeff=0.5,Heff=0.9,rho=0.1,alpha=0.1,eta=0.02)
#' res <- simulate_seir1(t = 250,state_t0 = state0,param = param)
#' plot(res,y=c("S","E","I","Recov","Fatal"),main="Expanded SEIR model I")
#'
#' @export
simulate_seir1 <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir1_state0")
    stopifnot(class(param) == "seir1_param")

    # simulation
    mod <- deSolve::ode(y=state_t0,
                        times=1:t,
                        func=seir1_model,
                        parms=list(R0=param$R0,
                                   beta=param$beta,
                                   sigma1=param$sigma1,
                                   sigma2=param$sigma2,
                                   gamma1=param$gamma1,
                                   gamma2=param$gamma2,
                                   gamma3=param$gamma3,
                                   Qeff=param$Qeff,
                                   Heff=param$Heff,
                                   rho=param$rho,
                                   alpha=param$alpha,
                                   eta=param$eta))

    # make the output a dataframe
    out <- list(param = param,
                epi = data.frame(mod))
    names(out$epi)[1] <- "t"

    # return
    class(out) <- c("covoid",class(out))
    out
}

#' SEIR model with quarantine and hospitalisation (SEIR-QH) parameters
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> E1), can be a function of t.
#' @param beta Rate of potential new infections per infected (S -> E1), can be a function of t.
#' Pass either R0 or beta.
#' @param sigma1 Inverse of the average length of first latent period (E1 -> E2)
#' @param sigma2 Inverse of the average length of second latent period (E2 -> I1)
#' @param gamma1 Inverse of the average length of first infectious period (I1 -> I2)
#' @param gamma2 Inverse of the average length of second infectious period (I2 -> R)
#' @param gamma3 Inverse of the average length of second infectious period for hospitalised cases (H -> R/F)
#' @param Qeff Effect of quarantine on infectiousness, should be in (0,1)
#' @param Heff Effect of hospitalisation on infectiousness, should be in (0,1)
#' @param rho Proportion of people who enter quarantine after exposure, should be in (0,1) (E -> Iq1 or I1)
#' @param alpha Proportion of infected requiring hospitalisation, should be in (0,1) (I1 -> I2 or H)
#' @param eta Case fatality rate, should be in (0,1) (H -> F or Rh). Note F is calculated at each step, there is no dF.
#'
#' @return List of SEIHR-Q model parameters
#'
#' @export
seir1_param <- function(R0,beta,sigma1,sigma2,gamma1,gamma2,gamma3,Qeff,Heff,rho,alpha,eta) {
    # work out missing parameter
    infectious_period <- 1/sigma2 + 1/gamma1 + (1-alpha)*(1/gamma2) + alpha*1/gamma3
    if(missing(R0)) {
        if (is.function(beta)) {
            R0 <- function(t) beta(t)*infectious_period
        } else {
            R0 <- function(t) beta*infectious_period
        }
    } else if(missing(beta)) {
        if (is.function(R0)) {
            beta <- function(t) R0(t)/infectious_period
        } else {
            beta <- function(t) R0/infectious_period
        }
    } else if(!missing(R0) & ! missing(beta)) {
        stop("Supply either R0 or beta")
    }

    # output with class seir1_param
    param <- list(R0=R0,beta=beta,sigma1=sigma1,sigma2=sigma2,
                  gamma1=gamma1,gamma2=gamma2,gamma3=gamma3,
                 Qeff=Qeff,Heff=Heff,rho=rho,alpha=alpha,eta=eta)
    class(param) <- "seir1_param"

    # return
    param
}

#' SEIR model with quarantine and hospitalisation (SEIR-QH) inital state
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Setup function
#'
#' @param S Initial number of susceptibles
#' @param E1 Initial number of exposed (period 1)
#' @param E2 Initial number of exposed (period 2)
#' @param I1 Initial number of infected (period 1)
#' @param I2 Initial number of infected (period 2)
#' @param H Initial number of hospitalised
#' @param Rh Initial number of recovered hospitalisations
#' @param R Initial number of recovered
#' @param Eq1 Initial number of exposed (period 1)
#' @param Eq2 Initial number of exposed (period 2)
#' @param Iq1 Initial number of infected (period 1) (who were in isolation)
#' @param Iq2 Initial number of infected (period 2) (who were in isolation)
#' @param Hq Initial number of hospitalised (who were in isolation)
#' @param Rqh Initial number of recovered hospitalisations (who were in isolation)
#' @param Rq Initial number of recovered (who were in isolation)
#'
#' @return List of SEIR 1 model initial states
#'
#' @export
seir1_state0 <- function(S,E1,E2,I1=0,I2=0,H=0,Rh=0,R=0,Eq1=0,Eq2=0,Iq1=0,Iq2=0,Hq=0,Rqh=0,Rq=0) {
    # assertions
    stopifnot(any(c(E1 >= 1,E2 >= 1,I1 >= 1,I2 >= 1,Eq1 >= 1,Eq2 >= 1,Iq1 >= 1,Iq2 >= 1)))

    # output with class seir1_state0
    state0 <- c(S=S,E1=E1,E2=E2,I1=I1,I2=I2,H=H,Rh=Rh,R=R,Eq1=Eq1,Eq2=Eq2,Iq1=Iq1,Iq2=Iq2,Hq=Hq,Rqh=Rqh,Rq=Rq)
    class(state0) <- "seir1_state0"

    # return
    state0
}

#' SEIR model with quarantine and hospitalisation (SEIR-QH) differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E1,E2,I1,I2,H,Rh,R,Eq1,Eq2,Iq1,Iq2,Hq,Rqh,Rq)
#' @param param The model parameters (R0,beta,sigma1,sigma2,gamma1,gamma2,gamma3,Qeff,Heff,rho,alpha,eta)
#'
#' @return derivatives of SEIR-QH model states with respect to time
#'
seir1_model <- function(t,state_t0,param) {
    with(as.list(c(state_t0, param)), {

        # population size
        N <- S + E1 + E2 + I1 + I2 + H + Rh + R + Eq1 + Eq2 + Iq1 + Iq2 + Hq + Rqh + Rq

        # derived parameters
        betaq <- Qeff*beta(t)
        betah <- Heff*beta(t)
        lambda <- beta(t)*(E2 + I1 + I2) + betaq*(Eq2 + Iq1 + Iq2) + betah*(H + Hq)

        # differential equations
        # general population
        dS <- -(S/N)*lambda
        dE1 <- (1-rho)*(S/N)*lambda - sigma1*E1
        dE2 <- sigma1*E1 - sigma2*E2
        dI1 <- sigma2*E2 - gamma1*I1
        dI2 <- (1-alpha)*gamma1*I1 - gamma2*I2
        dH <- alpha*gamma1*I1 - gamma3*H
        dRh <- gamma3*H
        dR <- gamma2*I2
        # quarantined population
        dEq1 <- (rho)*(S/N)*lambda - sigma1*Eq1
        dEq2 <- sigma1*Eq1 - sigma2*Eq2
        dIq1 <- sigma2*Eq2 - gamma1*Iq1
        dIq2 <- (1-alpha)*gamma1*Iq1 - gamma2*Iq2
        dHq <- alpha*gamma1*Iq1 - gamma3*Hq
        dRqh <- gamma3*Hq
        dRq <- gamma2*Iq2

        # return
        list(c(dS,dE1,dE2,dI1,dI2,dH,dRh,dR,
               dEq1,dEq2,dIq1,dIq2,dHq,dRqh,dRq),
             E=E1+E2+Eq1+Eq2,
             I=I1+I2+Iq1+Iq2,
             Hp=H+Hq,
             Recov=(1-eta)*(R+Rh+Rq+Rqh),
             Fatal=eta*(R+Rh+Rq+Rqh),
             N=S+E1+E2+I1+I2+H+Rh+R+Eq1+Eq2+Iq1+Iq2+Hq+Rqh+Rq)
    })
}

