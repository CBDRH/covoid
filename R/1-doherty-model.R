#' Simulate a deterministic SEIMR-QC ("Doherty" or "Moss") model
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
#' (In progress) Implementation of the model outlined in Moss et al (2020).
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see seimrqc_state0)
#' @param param Model parameters (see seimrqc_param)
#'
#' @return Object of class covoid and dcm (from the package EpiModels)
#'
#' @section References
#'
#' Moss R., Wood J., Brown D., Shearer F., Black, A.J., Cheng A.C., McCaw J.M., McVernon J. (2020). Modelling the impact of COVID-19 in Australia to inform transmission reducing
#' measures and health system preparedness. The Peter Doherty Institute for Infection and Immunity, VIC, Australia.
#' https://www.doherty.edu.au/uploads/content_doc/McVernon_Modelling_COVID-19_07Apr1_with_appendix.pdf
#'
#' @examples
#' (In progress)
#' # Non-Indigenous,  aged 50-59, no self-quarantine
#' param <- seimrqc_param(R0=2.53,lambdaimp=0,sigma1=1/1.6,sigma2=1/1.6,gamma1=1/4.0,gamma2=1/5.68,gammaq1=1/4.0,gammaq2=1/5.68,Qeff=0.5,Meff=0,rho=0,eta=1/sqrt(2),alphamBeta=0.5,probHospGivenInf=0.09895,delta=1/14,kappa=20,pm=1)
#' state0 <- seimrqc_state0(S0=100,E10=1)
#' res <- simulate_seimrqc(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","Itotal","M","Rtotal"))
#'
#' # with self-quarantine
#' param <- seimrqc_param(R0=2.5,lambdaimp=0,sigma1=0.3,sigma2=0.3,gamma1=0.3,gamma2=0.3,gammaq1=0.3,gammaq2=0.3,Qeff=0.5,Meff=0.99,rho=0.8,eta=1/sqrt(2),alphamBeta=0.5,delta=0.2,kappa=20,pm=1)
#' state0 <- seimrqc_state0(S0=100,E10=1)
#' res <- simulate_seimrqc(t = 100,state_t0 = state0,param = param)
#' plot(res,c("S","Itotal","M","Rtotal"))
#'
#' @export
simulate_seimrqc <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seimrqc_state0")
    stopifnot(class(param) == "seimrqc_param")

    # simulation
    alphas <- param$alphas
    param = EpiModel::param.dcm(R0=param$R0,
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
                                delta=param$delta,
                                kappa=param$kappa,
                                pm=param$pm)
    param$alphas <- alphas
    init = EpiModel::init.dcm(S=state_t0$S,
                              E1=state_t0$E1,
                              E2=state_t0$E2,
                              I1=state_t0$I1,
                              I2=state_t0$I2,
                              R=state_t0$R,
                              M=state_t0$M,
                              Rm=state_t0$Rm,
                              Eq1=state_t0$Eq1,
                              Eq2=state_t0$Eq2,
                              Iq1=state_t0$Iq1,
                              Iq2=state_t0$Iq2,
                              Rq=state_t0$Rq,
                              Mq=state_t0$Mq,
                              Rqm=state_t0$Rqm,
                              CTm=state_t0$CTm,
                              CTnm=state_t0$CTnm)
    control = EpiModel::control.dcm(nsteps=t,
                                    dt=0.1,
                                    new.mod=seimrqc_model)
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


#' SEIMR-QC model parameters
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
#' It should be in (0,1) and in Beta draw in McVernon et al 2020).
#' @param probHospGivenInf Probability of hospitalization given infection (typically from Table 3 in McVernon et al 2020)
#' @param delta Inverse of the duration of quarantine for contacts of ascertained cases (14 days)
#' @param kappa The per-person contact rate (20 people per day)
#' @param pm The probability of presenting cases being effectively managed
#'
#' @return List of seimrqc model parameters
#'
#' @export
seimrqc_param <- function(R0,lambdaimp,sigma1,sigma2,gamma1,gamma2,gammaq1,gammaq2,Qeff,Meff,rho,eta,alphamBeta,probHospGivenInf,delta,kappa,pm) {

    min_alpham <- 0.05+0.2*(eta-0.01)/0.99
    max_alpham <- 0.15+0.6*(eta-0.01)/0.99
    alpham <- min_alpham+(max_alpham-min_alpham)*alphamBeta
    alphas <- eta * probHospGivenInf
    alpha = alphas + alpham*(1 - alphas)
    # output with class seimrqc_param
    param = list(R0=R0,lambdaimp=lambdaimp,sigma1=sigma1,sigma2=sigma2,gamma1=gamma1,
                 gamma2=gamma2,gammaq1=gammaq1,gammaq2=gammaq2,Qeff=Qeff,Meff=Meff,rho=rho,eta=eta,alpha=alpha,
                 alphas=alphas,delta=delta,kappa=kappa,pm=pm)
    class(param) = "seimrqc_param"

    # return
    param
}

#' SEIMR-QC model initial state
#'
#' Setup function
#'
#' @param S0 Initial number of susceptibles
#' @param E10 Initial number of exposed (latent period 1)
#' @param E20 Initial number of exposed (latent period 2)
#' @param I10 Initial number of infected (period 1)
#' @param I20 Initial number of infected (period 2)
#' @param R0 Initial number of removed
#' @param M0 Initial number of under medical management
#' @param Rm0 Initial number of recovered who sought medical treatment
#' @param Eq10 Initial number of exposed (latent period 1) who self quarantined after contact
#' @param Eq20 Initial number of exposed (latent period 2) who self quarantined after contact
#' @param Iq10 Initial number of infected (period 1) who self quarantined after contact
#' @param Iq20 Initial number of infected (period 2) who self quarantined after contact
#' @param Rq0 Initial number of removed who self quarantined after contact
#' @param Mq0 Initial number of under medical management who self quarantined after contact
#' @param Rqm0 Initial number of recovered who sought medical treatment and self quarantined after contact
#'
#' @return List of seimrqc model initial states
#'
#' @export
seimrqc_state0 <- function(S0,E10,E20=0,I10=0,I20=0,R0=0,M0=0,Rm0=0,Eq10=0,Eq20=0,Iq10=0,Iq20=0,Rq0=0,Mq0=0,Rqm0=0,
                           CTm0=2,CTnm0=20){
    # assertions
    stopifnot(E10 >= 1)

    # output with class seimrqc_state0
    state0 = list(S=S0,E1=E10,E2=E20,I1=I10,I2=I20,R=R0,M=M0,Rm=Rm0,Eq1=Eq10,Eq2=Eq20,Iq1=Iq10,Iq2=Iq20,Rq=Rq0,Mq=Mq0,Rqm=Rqm0,
                  CTm=CTm0,CTnm=CTnm0,
                  N=S0 + E10 + E20 + I10 + I20 + R0 + M0 + Rm0 + Eq10 + Eq20 + Iq10 + Iq20 + Rq0 + Mq0 + Rqm0)
    class(state0) = "seimrqc_state0"

    # return
    state0
}

#' SEIMR-QC differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E1,E2,I1,I2,R,M,Rm,Eq1,Eq2,Iq1,Iq2,Rq,Mq,Rqm)
#' @param param The model parameters (R0,sigma1,sigma2,gamma1,gamma2,gammaq1,gammaq2,Qeff,Heff,rho,alpha)
#'
#' @return derivatives of SEIMR-QC model states with respect to time
#'
seimrqc_model <- function(t,state_t0,param) {
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
             Itotal=E1+Eq1+E2+Eq2+I1+Iq1+I2+Iq2,
             I=E1+E2+I1+I2,
             Ictotal=E1+E2+I1+I2,
             Mtotal=M+Mq,
             Rtotal=R+Rm+Rq+Rqm,
             H=alphas*(E1+E2)+Eq1+Eq2,
             D=(alphas*(E1+E2)+Eq1+Eq2)*0.29335*0.5, # hospitalized that require ICU and 50% die in ICU
             Q=Eq1+Eq2+Iq1+Iq2+Mq, # Number in quanteen
             N=S + E1 + E2 + I1 + I2 + R + M + Rm + Eq1 + Eq2 + Iq1 + Iq2 + Rq + Mq + Rqm)
    })
}

