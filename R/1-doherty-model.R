#' Simulate a deterministic SEIR model with quarantine and hospitalisation ("Doherty" or "Moss") model
#'
#' \itemize{
#' \item S = Susceptible
#' \item E1/Eq1 = Exposed (asymptomatic; not infectious)
#' \item E2/Eq2 = Exposed (asymptomatic; infectious)
#' \item I1/Iq1 = First infectious period
#' \item I2/Iq2 = Second infection period
#' \item M/Mq = Under medical management (hospitalised; GP)
#' \item R/Rq/Rm/Rqm = Recovered
#' }
#'
#' Partial implementation of the model outlined in Moss et al (2020).
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see seir2_state0)
#' @param param Model parameters (see seir2_param)
#'
#' @return Object of class covoid
#'
#' @section References
#'
#' Moss R., Wood J., Brown D., Shearer F., Black, A.J., Cheng A.C., McCaw J.M., McVernon J. (2020). Modelling the impact of COVID-19 in Australia to inform transmission reducing
#' measures and health system preparedness. The Peter Doherty Institute for Infection and Immunity, VIC, Australia.
#' https://www.doherty.edu.au/uploads/content_doc/McVernon_Modelling_COVID-19_07Apr1_with_appendix.pdf
#'
#' @examples
#' # Non-Indigenous,  aged 50-59, no self-quarantine
#' param <- seir2_param(R0=2.53,lambdaimp=0,sigma1=1/1.6,sigma2=1/1.6,gamma1=1/4.0,gamma2=1/5.68,gammaq1=1/4.0,gammaq2=1/5.68,Qeff=0.5,Meff=0,rho=0,eta=1/sqrt(2),alphamBeta=0.5,probHospGivenInf=0.09895,delta=1/14,kappa=20,pm=1)
#' state0 <- seir2_state0(S=100,E1=1)
#' res <- simulate_seir2(t = 100,state_t0 = state0,param = param)
#'
#' # with self-quarantine
#' param <- seir2_param(R0=2.5,lambdaimp=0,sigma1=0.3,sigma2=0.3,gamma1=0.3,gamma2=0.3,gammaq1=0.3,gammaq2=0.3,Qeff=0.5,Meff=0.99,rho=0.8,eta=1/sqrt(2),alphamBeta=0.5,probHospGivenInf=0.09895,delta=1/14,kappa=20,pm=1)
#' state0 <- seir2_state0(S=100,E1=1)
#' res <- simulate_seir2(t = 100,state_t0 = state0,param = param)
#'
#' @export
simulate_seir2 <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir2_state0")
    stopifnot(class(param) == "seir2_param")

    # simulation
    mod <- deSolve::ode(y=state_t0,
                        times=1:t,
                        func=seir2_model,
                        parms=list(R0=param$R0,
                                   lambdaimp=param$lambdaimp,
                                   sigma1=param$sigma1,
                                   sigma2=param$sigma2,
                                   gamma1=param$gamma1,
                                   gamma2=param$gamma2,
                                   gammaq2=param$gammaq2,
                                   gammaq1=param$gammaq1,
                                   Qeff=param$Qeff,
                                   Meff=param$Meff,
                                   rho=param$rho,
                                   eta=param$eta,
                                   alpha=param$alpha,
                                   alphas=param$alphas,
                                   delta=param$delta,
                                   kappa=param$kappa,
                                   pm=param$pm))

    # make the output a dataframe
    out <- list(param = param,
                epi = data.frame(mod))
    names(out$epi)[1] <- "t"

    # return
    class(out) <- c("covoid",class(out))
    out
}


#' Doherty model parameters
#'
#' Setup function
#'
#' @param R0 Basic reproduction number (S -> E)
#' @param lambdaimp The force of infection from imported cases.
#' @param sigma1 Inverse of the average length of first latent period (E1 -> E2)
#' @param sigma2 Inverse of the average length of second latent period (E2 -> I1)
#' @param gamma1 Inverse of the average length of first infectious period (I1 -> I2 or H)
#' @param gamma2 Inverse of the average length of second infectious period (I2 -> R)
#' @param gammaq1 Inverse of the average length of first infectious period for quarantined cases (Iq1 -> Iq2)
#' @param gammaq2 Inverse of the average length of second infectious period for quarantined cases (Iq2 -> Rq)
#' @param Qeff Effect of quarantine on infectiousness, should be in (0,1)
#' @param Meff Effect of medical management on infectiousness, should be in (0,1)
#' @param rho the proportion of contacts (of ascertained cases) that will self-quarantine (0,1)
#' @param eta Scaling factor for hospitalization ("severe").
#' @param alphamBeta Scaling factor for proportion of non-severe requesting medical assistance.
#' It should be in (0,1) and in Beta draw in Moss et al 2020).
#' @param probHospGivenInf Probability of hospitalization given infection (typically from Table 3 in McVernon et al 2020)
#' @param delta Inverse of the duration of quarantine for contacts of ascertained cases (14 days)
#' @param kappa The per-person contact rate (20 people per day)
#' @param pm The probability of presenting cases being effectively managed
#'
#' @return List of seir2 model parameters
#'
#' @export
seir2_param <- function(R0,lambdaimp,sigma1,sigma2,gamma1,gamma2,gammaq1,gammaq2,Qeff,Meff,rho,eta,alphamBeta,probHospGivenInf,delta,kappa,pm) {

    min_alpham <- 0.05+0.2*(eta-0.01)/0.99
    max_alpham <- 0.15+0.6*(eta-0.01)/0.99
    alpham <- min_alpham+(max_alpham-min_alpham)*alphamBeta
    alphas <- eta * probHospGivenInf
    alpha = alphas + alpham*(1 - alphas)
    # output with class seir2_param
    param = list(R0=R0,lambdaimp=lambdaimp,sigma1=sigma1,sigma2=sigma2,gamma1=gamma1,
                 gamma2=gamma2,gammaq1=gammaq1,gammaq2=gammaq2,Qeff=Qeff,Meff=Meff,rho=rho,eta=eta,alpha=alpha,
                 alphas=alphas,delta=delta,kappa=kappa,pm=pm)
    class(param) = "seir2_param"

    # return
    param
}

#' Doherty model initial state
#'
#' Setup function
#'
#' @param S Initial number of susceptibles
#' @param E1 Initial number of exposed (latent period 1)
#' @param E2 Initial number of exposed (latent period 2)
#' @param I1 Initial number of infected (period 1)
#' @param I2 Initial number of infected (period 2)
#' @param R Initial number of removed
#' @param M Initial number of under medical management
#' @param Rm Initial number of recovered who sought medical treatment
#' @param Eq1 Initial number of exposed (latent period 1) who self quarantined after contact
#' @param Eq2 Initial number of exposed (latent period 2) who self quarantined after contact
#' @param Iq1 Initial number of infected (period 1) who self quarantined after contact
#' @param Iq2 Initial number of infected (period 2) who self quarantined after contact
#' @param Rq Initial number of removed who self quarantined after contact
#' @param Mq Initial number of under medical management who self quarantined after contact
#' @param Rqm Initial number of recovered who sought medical treatment and self quarantined after contact
#'
#' @return List of seir2 model initial states
#'
#' @export
seir2_state0 <- function(S,E1,E2=0,I1=0,I2=0,R=0,M=0,Rm=0,Eq1=0,Eq2=0,Iq1=0,Iq2=0,Rq=0,Mq=0,Rqm=0,
                           CTm=2,CTnm=20){
    # assertions
    stopifnot(any(c(E1 >= 1,E2 >= 1,I1 >= 1,I2 >= 1,Eq1 >= 1,Eq2 >= 1,Iq1 >= 1,Iq2 >= 1)))

    # output with class seir2_state0
    state0 = c(S=S,E1=E1,E2=E2,I1=I1,I2=I2,R=R,M=M,Rm=Rm,Eq1=Eq1,Eq2=Eq2,Iq1=Iq1,Iq2=Iq2,Rq=Rq,Mq=Mq,Rqm=Rqm,
                  CTm=CTm,CTnm=CTnm)
    class(state0) = "seir2_state0"

    # return
    state0
}

#' Doherty differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E1,E2,I1,I2,R,M,Rm,Eq1,Eq2,Iq1,Iq2,Rq,Mq,Rqm)
#' @param param The model parameters (R0,sigma1,sigma2,gamma1,gamma2,gammaq1,gammaq2,Qeff,Heff,rho,alpha)
#'
#' @return derivatives of Doherty model states with respect to time
#'
seir2_model <- function(t,state_t0,param) {
    with(as.list(c(state_t0, param)), {

        # Population size
        N <- S + E1 + E2 + I1 + I2 + R + M + Rm + Eq1 + Eq2 + Iq1 + Iq2 + Rq + Mq + Rqm

        # derived parameters
        beta = R0*(1/((1/sigma2) + (1/gamma1) + (1/gamma2)))
        betamq = beta*(1 - max(Meff,Qeff))
        lambda = lambdaimp + beta*(E2 + I1 + I2) + beta*(1 - Qeff)*(Eq2 + Iq1 + Iq2) +
            beta*(1 - Meff)*M + betamq*Mq

        Thetam =  (S/N)*( CTm/(CTm + CTnm))
        Thetanm = (S/N)*(CTnm/(CTm + CTnm))

        # Differential equations
        # contacts of cases
        dCTm =  kappa*(gamma1*I1 + gammaq1*Iq1)*(alpha*pm) - delta*CTm - lambda*Thetam
        dCTnm = kappa*(gamma1*I1 + gammaq1*Iq1)*(1 - alpha*pm) - delta*CTnm - lambda*Thetanm
        # general population
        dS = -(S/N)*lambda
        dE1 = lambda*(S/N - rho*Thetam) - sigma1*E1
        dE2 = sigma1*E1 - sigma2*E2
        dI1 = sigma2*E2 - gamma1*I1
        dI2 = gamma1*I1*(1 - alpha*pm) - gamma2*I2
        dM = gamma1*I1*alpha*pm - gamma2*M
        dR = gamma2*I2
        dRm = gamma2*M
        # quarantined population
        dEq1 = lambda*rho*Thetam - sigma1*Eq1
        dEq2 = sigma1*Eq1 - sigma2*Eq2
        dIq1 = sigma2*Eq2 - gammaq1*Iq1
        dIq2 = gammaq1*Iq1*(1 - alpha*pm) - gammaq2*Iq2
        dMq = gammaq1*Iq1*alpha*pm - gammaq2*Mq
        dRq = gammaq2*Iq2
        dRqm = gammaq2*Mq

# cat(sprintf("t = %f dM %f gamma1*alpha*pm %f gamma2 %f I1 %f M %f alpha %f Rtotal %f E2+I1+I2 %f Eq2+Iq1+Iq2 %f CTm %f CTnm %f lambda %f\n",t,dM,gamma1*alpha*pm,gamma2,I1,M,alpha,R+Rm+Rq+Rqm,E2+I1+I2,Eq2+Iq1+Iq2,CTm,CTnm,lambda))

        # return
        list(c(dS,dE1,dE2,dI1,dI2,dR,dM,dRm,dEq1,dEq2,dIq1,dIq2,dRq,dMq,dRqm,dCTm,dCTnm),
             E=E1+Eq1+E2+Eq2,
             I=I1+I2+Iq1+Iq2,
             Mtotal=M+Mq,
             Recov=R+Rm+Rq+Rqm,
             H=alphas*(E1+E2)+Eq1+Eq2,
             Fatal=(alphas*(E1+E2)+Eq1+Eq2)*0.29335*0.5, # hospitalized that require ICU and 50% die in ICU
             Q=Eq1+Eq2+Iq1+Iq2+Mq, # Number in quanteen
             N=S + E1 + E2 + I1 + I2 + R + M + Rm + Eq1 + Eq2 + Iq1 + Iq2 + Rq + Mq + Rqm)
    })
}

