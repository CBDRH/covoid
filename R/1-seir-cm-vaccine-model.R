
#' Euler summation solving with intervention update
#'
#' @param y initial state
#' @param times time steps over which to sum
#' @param func difference equation, see `seir_cv_model`
#' @param parms parameters to pass to func, needs to be a list
#'
euler1 <- function(y,times,func,parms) {
    # object to store results from y = y0 + sum(dy/dt,time)
    main_res <- matrix(nrow=length(times),ncol=length(y))
    colnames(main_res) <- names(y)
    main_res[1,] <- y
    intervention_res <- matrix(nrow=length(times),ncol=2)
    colnames(intervention_res) <- c("contact_intervention","transmission_intervention")
    if(!is.null(parms$contact_intervention)) {
        intervention_res[1,1] <- parms$contact_intervention$state$inplace
    } else {
        intervention_res[1,1] <- FALSE
    }
    if(!is.null(parms$transmission_intervention)) {
        intervention_res[1,2] <- parms$transmission_intervention$state$inplace
    } else {
        intervention_res[1,1] <- FALSE
    }

    # loop over time steps and update
    # 1) y
    # 2) reactive intervention
    for (t in 2:max(times)) {
        update_t <- func(t,y,parms)
        data_t <- update_t$data
        dy <- data_t[[1]]
        other_data_t <- unlist(data_t[2:length(data_t)])

        if (t == 2) {
            # matrix to store other non-difference equation results
            other_res <- matrix(nrow=length(times),ncol=length(other_data_t))
            colnames(other_res) <- names(other_data_t)
        }

        # update and store results
        y <- y + dy
        main_res[t,] <- y
        other_res[t,] <- other_data_t

        # update interventions
        if(!is.null(update_t$interventions$transmission_intervention)) {
            parms$transmission_intervention <- update_t$interventions$transmission_intervention
        }
        if(!is.null(update_t$interventions$contact_intervention)) {
            parms$contact_intervention <- update_t$interventions$contact_intervention
        }
        if(!is.null(parms$contact_intervention)) {
            intervention_res[t,1] <- parms$contact_intervention$state$inplace
        } else {
            intervention_res[t,1] <- FALSE
        }
        if(!is.null(parms$transmission_intervention)) {
            intervention_res[t,2] <- parms$transmission_intervention$state$inplace
        } else {
            intervention_res[t,1] <- FALSE
        }
    }
    other_res[is.na(other_res)] <- 0
    cbind(times,main_res,other_res,intervention_res)
}



#' Simulate a deterministic age structured SEIR model with vaccinations and
#' reactive interventions
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' S = Susceptible
#' E = Exposed (latent period)
#' I = Infectious
#' R = Removed (recovered or died)
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?seir_state0)
#' @param param Model parameters (see ?seir_param)
#'
#' @return Object of class covoid
#'
#' @examples
#' (this is a long one!)
#'
#' The two functions (covoid::mm and covoid::vaccination_allocation_mm)
#' will be used in vaccine allocation
#' # import and setup baseline states
#' cm_oz <- import_contact_matrix("Australia","general")
#' nJ <- ncol(cm_oz)
#' dist_oz <- import_age_distribution("Australia")
#' SSv <- dist_oz*5e6
#' baseline <- 0.0  # vaccinations at t=0
#' S <- SSv*(1-baseline)
#' Sv <- SSv*baseline
#' E <- rep(0,nJ)
#' Ev <- rep(0,nJ)
#' I <- Iv <- rep(0,nJ)
#' H <- rep(0,nJ)
#' Hv <- rep(0,nJ)
#' R <- Rv <- rep(0,nJ)
#' state0 <- seir_cv_state0(S,E,I,H,R,Sv,Ev,Iv,Hv,Rv)
#'
#' ## parameters
#' # vaccine effectiveness
#' vaceff1 <- rep(0.99,nJ)
#' vaceff2 <- rep(0.90,nJ)
#' vaceff3 <- rep(0.90,nJ)
#'
#' # imported cases
#' n_imp_cases <- function(t) {
#'     0*(t <= 365) + (t > 365)*rpois(n = 1,lambda = 200)
#' }
#' # number of available vaccines
#' nvac <- function(t) {
#'     # number of available vaccinations (total)
#'     1000*(t < 30) + 20000*(t >= 30)
#' }
#' # wrap vaccine allocation function
#' random_vac_alloc <- function(t,n,s) {
#'     vaccination_allocation_mm(t,n,s,list(p=dist_oz,s0=S,half=0.05))
#' }
#' # reactive transmission interventions
#' int1 <- reactive_intervention(threshold=40,reduce=0.5,state=reactive_state())
#' # reactive contact rate interventions
#' int2 <- reactive_intervention(threshold=40,reduce=0.5,state=reactive_state())
#' param1 <- seir_cv_param(R0 = 2.5,
#'                         sigma=0.1,
#'                         gamma = 0.1,
#'                         phosp = rep(0.1,nJ),
#'                         phospv= rep(0.01,nJ),
#'                         thosp = 1/7,
#'                         cm=cm_oz,
#'                         dist=dist_oz,
#'                         vaceff1=vaceff1,
#'                         vaceff2=vaceff2,
#'                         vaceff3=vaceff3,
#'                         nvac=nvac,
#'                         contact_intervention = int1,
#'                         transmission_intervention=int2,
#'                         vac_alloc=random_vac_alloc,
#'                         n_imp=n_imp_cases)
#'
#' ## simulation
#' res1 <- simulate_seir_cv(t = 365*2,state_t0 = state0,param = param1)
#'
#' ## plot the results
#' library(ggplot2)
#' p1 <- plot(res1,y = c("Sv","S")) +
#'     theme_bw(base_size = 16) +
#'     labs(title="Susceptible\nvaccinated vs. unvaccinated")
#' p2 <- plot(res1,y="incidence") +
#'     theme_bw(base_size = 16) +
#'     labs(title="Incidence")
#' gridExtra::grid.arrange(p1,p2,ncol=1)
#'
#'
#' @export
simulate_seir_cv <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir_cv_state0")
    stopifnot(class(param) == "seir_cv_param")

    # calculate prob transmission
    if (is.list(param$cm)) {
        cm_gen <- Reduce('+', param$cm)
    } else {
        cm_gen <- param$cm
    }
    if (is.null(param$im)) {
        param$im <- matrix(1,nrow = nrow(cm_gen),ncol = ncol(cm_gen))
    }
    J <- ncol(cm_gen)
    V <- diag(rep(param$gamma,J))
    F1 <- cm_gen
    for (i in 1:J) {
        for (j in 1:J) {
            F1[i,j] <- param$dist[i]/param$dist[j]*F1[i,j]*param$im[i,j]
        }
    }
    K1 <- F1 %*% solve(V)
    l1 <- max(Re(eigen(K1)$values))
    pt <- param$R0/l1


    # simulation
    mod <- euler1(y=state_t0,
                        times=1:t,
                        func=seir_cv_model,
                        parms=list(pt=pt,
                                   sigma=param$sigma,
                                   gamma=param$gamma,
                                   phosp=param$phosp,
                                   phospv=param$phospv,
                                   thosp=param$thosp,
                                   cm=param$cm,
                                   pt=param$pt,
                                   dist=param$dist,
                                   vaceff1=param$vaceff1,
                                   vaceff2=param$vaceff2,
                                   vaceff3=param$vaceff3,
                                   nvac=param$nvac,
                                   vac_alloc=param$vac_alloc,
                                   n_imp=param$n_imp,
                                   contact_intervention=param$contact_intervention,
                                   transmission_intervention=param$transmission_intervention,
                                   im=param$im))

    # make the output a dataframe
    out <- list(param = param,
                epi = data.frame(mod))
    names(out$epi)[1] <- "t"
    nJ <- length(param$dist)
    out$epi <- cbind(out$epi)
    out$epi$incidence <- rowSums(out$epi[paste0("incidence",1:nJ)])
    out$epi$incidencev <- rowSums(out$epi[paste0("incidencev",1:nJ)])
    out$epi$incidenceE <- rowSums(out$epi[paste0("incidenceE",1:nJ)])
    out$epi$incidenceEv <- rowSums(out$epi[paste0("incidenceEv",1:nJ)])

    # return
    class(out) <- c("covoid",class(out))
    out
}


#' Age structured SEIR model parameters
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' Define parameter values for age structured SEIR models
#'
#' @param R0 Basic/empirical reproduction number (S -> E).
#' @param sigma Inverse of the average length of the latent period (E -> I)
#' @param gamma Inverse of the average length of infectious period (I -> R)
#' @param phosp Hospitalisation rate for the unvaccinated
#' @param phospv Hospitalisation rate for the vaccinated
#' @param thosp Inverse of the average time in hospital
#' @param cm Contact matrix or a named list of contact matrices where the sum is taken to
#' be all contacts in the population.
#' @param vaceff1 Vaccine effectiveness: P(infection) of vaccinated (a vector of length J)
#' @param vaceff2 Vaccine effectiveness: S -> Sv (a vector of length J)
#' @param vaceff3 Vaccine effectiveness: Iv -> S/Sv (a vector of length J)
#' @param nvac Number vaccinated.
#' @param n_imp Number of infection imported cases (a function of time).
#' @param dist Proportion of the population in each age category. Should sum to 1.
#' @param contact_intervention An intervention created using `contact_intervention` or named list
#' of interventions with the names matching the list of contact matrices.
#' @param transmission_intervention An intervention created using `create_intervention` or named list
#' of interventions with the names matching the list of contact matrices.
#' @param im Relative infectiousness matrix. Default is 1.
#'
#' @return List of SEIR model parameters
#'
#' @export
seir_cv_param <- function(R0,sigma,gamma,phosp,phospv,thosp,cm,dist,vaceff1,vaceff2,vaceff3,nvac,vac_alloc,n_imp,contact_intervention=NULL,transmission_intervention=NULL,im=NULL) {
    # assertions
    if (is.list(cm)) {
        stopifnot(!is.null(names(cm)))
        stopifnot(all(sapply(cm,function(x) nrow(x) == ncol(x))))
        stopifnot(all(sapply(cm,function(x) nrow(x) == length(dist))))
        if (!is.null(contact_intervention)) {
            stopifnot(is.list(contact_intervention))
            stopifnot(all(names(contact_intervention) %in% names(cm)))
        }
        if (!is.null(im)) {
            stopifnot(ncol(cm[[1]]) == ncol(im))
            stopifnot(ncol(im) == nrow(im))
        }
    } else {
        stopifnot(ncol(cm) == nrow(cm))
        stopifnot(length(dist) == ncol(cm))
        if (!is.null(contact_intervention)) {
            stopifnot("intervention" %in% class(contact_intervention))
        }
        if (!is.null(im)) {
            stopifnot(ncol(cm) == ncol(im))
            stopifnot(ncol(im) == nrow(im))
        }
    }
    stopifnot(all.equal(sum(dist),1.0))

    # # calculate prob transmission
    # if (is.list(cm)) {
    #     cm_gen <- Reduce('+', cm)
    # } else {
    #     cm_gen <- cm
    # }
    # if (is.null(im)) {
    #     im <- matrix(1,nrow = nrow(cm_gen),ncol = ncol(cm_gen))
    # }
    # J <- ncol(cm_gen)
    # V <- diag(rep(gamma,J))
    # F1 <- cm_gen
    # for (i in 1:J) {
    #     for (j in 1:J) {
    #         F1[i,j] <- dist[i]/dist[j]*F1[i,j]*im[i,j]
    #     }
    # }
    # K1 <- F1 %*% solve(V)
    # l1 <- max(Re(eigen(K1)$values))
    # pt <- R0/l1

    # output with class seir_param
    param <- list(R0=R0,
                  sigma=sigma,
                  gamma=gamma,
                  phosp=phosp,
                  phospv=phospv,
                  thosp=thosp,
                  cm=cm,
                  #pt=pt,
                  dist=dist,
                  vaceff1=vaceff1,
                  vaceff2=vaceff2,
                  vaceff3=vaceff3,
                  nvac=nvac,
                  vac_alloc=vac_alloc,
                  n_imp=n_imp,
                  contact_intervention=contact_intervention,
                  transmission_intervention=transmission_intervention,
                  im=im)
    class(param) <- "seir_cv_param"

    # return
    param
}

#' Age structured SEIR model inital state
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' Define intial state values for age structured SEIR models
#'
#' @param S Initial number of susceptibles
#' @param E Initial number of exposed (latent period)
#' @param I Initial number of infectious
#' @param H Initial number of hospitalisation
#' @param R Initial number of removed
#'
#' @return List of SEIR model initial states
#'
#' @export
seir_cv_state0 <- function(S,E,I,H,R,Sv,Ev,Iv,Hv,Rv) {
    # assertions
    stopifnot(length(S) == length(E))
    stopifnot(length(E) == length(I))
    stopifnot(length(I) == length(H))
    stopifnot(length(I) == length(R))
    stopifnot(any(I >= 0))

    # output with class seir_state0
    state0 <- c(S=S,E=E,I=I,H=H,R=R,Sv=Sv,Ev=Ev,Iv=Iv,Hv=Hv,Rv=Rv)
    class(state0) <- "seir_cv_state0"

    # return
    state0
}

#' SEIR with heterogeneous contact differential equations and vaccination
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E,I,R,Sv,Ev,Iv,Rv)
#' @param param The model parameters (R0,sigma,gamma,cm,dist,intervention)
#'
#' @return derivatives of SEIR model states with respect to time
#'
seir_cv_model <- function(t,y,parms) {
    with(as.list(c(y, parms)), {

        # population size
        J <- ncol(cm)

        # un-vaccinated
        S <- y[1:J]
        E <- y[(J+1):(2*J)]
        I <- y[(2*J+1):(3*J)]
        H <- y[(3*J+1):(4*J)]
        R <- y[(4*J+1):(5*J)]
        # vaccinated
        Sv <- y[(5*J+1):(6*J)]
        Ev <- y[(6*J+1):(7*J)]
        Iv <- y[(7*J+1):(8*J)]
        Hv <- y[(8*J+1):(9*J)]
        Rv <- y[(9*J+1):(10*J)]
        # total
        N <- S + E + I + H + R + Sv + Ev + Iv + Hv + Rv

        # account for interventions
        # contact matrix
        cm_int_update <- calculate_reactive(cm, contact_intervention, I + Iv,dist)
        contact_intervention <- cm_int_update$intervention
        cm_cur <- cm_int_update$param
        # probability of transmission
        pt_int_update <- calculate_reactive(pt, transmission_intervention, I + Iv)
        transmission_intervention <- pt_int_update$intervention
        pt_cur <- pt_int_update$param
        #pt_cur <- calculate_current_pt(pt,transmission_intervention,t)
        #cm_cur <- calculate_current_cm(cm,contact_intervention,t,dist)

        # vaccination rate
        nvac_t <- vac_alloc(t, nvac(t),S)

        # prop vaccinated
        pvac <- sum(Sv+Ev+Iv+Hv+Rv)/(sum(Sv+Ev+Iv+Hv+Rv) + sum(S+E+I+H+R))

        # derived parameters

        # differential equations
        dS <- numeric(length=J)
        dE <- numeric(length=J)
        dI <- numeric(length=J)
        dH <- numeric(length=J)
        dR <- numeric(length=J)
        dSv <- numeric(length=J)
        dEv <- numeric(length=J)
        dIv <- numeric(length=J)
        dHv <- numeric(length=J)
        dRv <- numeric(length=J)

        for (i in 1:J) {
            # derived parameters
            lambda_imp <- pt_cur*sum(cm_cur[i,]*n_imp(t)/N)
            lambda_i <- sum(pt_cur*im[,i]*cm_cur[i,]*((I + vaceff3[i]*Iv)/N)) # force infect

            # vaccination process
            nvac_i <- vaceff2[i]*nvac_t[i]

            # un-vaccinated flow
            dS[i] <- -(S[i])*(lambda_i + lambda_imp) - nvac_i
            dE[i] <- (S[i])*(lambda_i + lambda_imp) - sigma*E[i]
            dI[i] <- sigma*E[i] - (1-phosp[i])*gamma*I[i] - phosp[i]*gamma*I[i]
            dH[i] <- phosp[i]*gamma*I[i] - thosp*H[i]
            dR[i] <- (1-phosp[i])*gamma*I[i] + thosp*H[i]

            # un-vaccinated flow
            dSv[i] <- -(1-vaceff1[i])*Sv[i]*(lambda_i + lambda_imp) + nvac_i
            dEv[i] <- (1-vaceff1[i])*Sv[i]*(lambda_i + lambda_imp) - sigma*Ev[i]
            dIv[i] <- sigma*Ev[i] - (1-phospv[i])*gamma*Iv[i] - phospv[i]*gamma*Iv[i]
            dHv[i] <- phospv[i]*gamma*Iv[i] - thosp*Hv[i]
            dRv[i] <- (1-phospv[i])*gamma*Iv[i] + thosp*Hv[i]
        }

        # return
        list(data=list(c(dS,dE,dI,dH,dR,dSv,dEv,dIv,dHv,dRv),
             S=sum(S),
             E=sum(E),
             I=sum(I),
             H=sum(H),
             R=sum(R),
             Sv=sum(Sv),
             Ev=sum(Ev),
             Iv=sum(Iv),
             Hv=sum(Hv),
             Rv=sum(Rv),
             Ntotal=sum(S) + sum(E) + sum(I) + sum(H) + sum(R) +
                 sum(Sv) + sum(Ev) + sum(Iv) + sum(Hv) + sum(Rv),
             incidence=dE+dI+dR,
             incidencev=dEv+dIv+dRv,
             incidenceE=dI+dR,
             incidenceEv=dIv+dRv),
             interventions=list(transmission_intervention=transmission_intervention,
                                contact_intervention=contact_intervention))
    })
}
