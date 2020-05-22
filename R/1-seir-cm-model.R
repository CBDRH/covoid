
#' Simulate a deterministic age structured SEIR model
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
#' @return Object of class covoidd and dcm (from the package EpiModels)
#'
#' @examples
#' # Example 1: no interventions
#' cm_oz <- import_contact_matrix("Australia","general")
#' nJ <- ncol(cm_oz)
#' dist_oz <- import_age_distribution("Australia")
#' S <- rep(1000,nJ)
#' E <- rep(1,nJ)
#' I <- rep(0,nJ)
#' R <- rep(0,nJ)
#' state0 <- seir_c_state0(S = S,E = E,I = I,R = R)
#' param1 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,cm=cm_oz,dist=dist_oz)
#' res1 <- simulate_seir_c(t = 150,state_t0 = state0,param = param1)
#' plot(res1,y=c("S","E","I","R"))
#'
#' # Example 2: general physical distancing, school closures and masks/handwashing
#' cm_oz_all <- import_contact_matrix("Australia","general")
#' cm_oz_sch <- import_contact_matrix("Australia","school")
#' cm <- list(all = cm_oz_all, sch = cm_oz_sch)
#' # separate out school and general population contact rates
#' cm_oz_all <- cm_oz_all - cm_oz_sch
#' int <- list(sch=contact_intervention(start = 10,stop = 150,reduce = 0.2,start_delay = 5,stop_delay = 5),
#'           all=contact_intervention(start = 10,stop = 150,reduce = 0.8,start_delay = 5,stop_delay = 5))
#' int_t <- transmission_intervention(start = 10,stop = 200,reduce = 0.9,start_delay = 5,stop_delay = 5)
#' param2 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,
#'                      cm=list(sch=cm_oz_sch,
#'                              all=cm_oz_all),
#'                      dist=dist_oz,
#'                      contact_intervention=int,
#'                      transmission_intervention=int_t)
#' res2 <- simulate_seir_c(t = 150,state_t0 = state0,param = param2)
#' plot(res2,y=c("S","E","I","R"))
#'
#' # Example 3: as #2 with children (<15) less infectious/susceptible
#' im <- matrix(1,ncol=16,nrow=16)
#' im[,1:3] <- 0.8
#' param3 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,
#'                      cm=list(sch=cm_oz_sch,
#'                              all=cm_oz_all),
#'                      dist=dist_oz,
#'                      contact_intervention=int,
#'                      transmission_intervention=int_t,
#'                      im=im)
#' res3 <- simulate_seir_c(t = 200,state_t0 = state0,param = param3)
#' plot(res3,y=c("S","E","I","R"))
#'
#' @export
simulate_seir_c <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seir_c_state0")
    stopifnot(class(param) == "seir_c_param")

    # simulation
    mod <- deSolve::ode(y=state_t0,
                       times=1:t,
                       func=seir_c_model,
                       parms=list(pt=param$pt,
                                  sigma=param$sigma,
                                  gamma=param$gamma,
                                  cm=param$cm,
                                  pt=param$pt,
                                  dist=param$dist,
                                  contact_intervention=param$contact_intervention,
                                  transmission_intervention=param$transmission_intervention,
                                  im=param$im))

    # make the output a dataframe
    out <- list(param = param,
               epi = data.frame(mod))
    names(out$epi)[1] <- "t"
    nJ <- length(param$dist)
    incidences <- out$epi[c("I",paste0("I",1:nJ))]*param$sigma
    names(incidences) <- c("incidence",paste0("incidence",1:nJ))
    out$epi <- cbind(out$epi,incidences)

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
#' @param cm Contact matrix or a named list of contact matrices where the sum is taken to
#' be all contacts in the population.
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
seir_c_param <- function(R0,sigma,gamma,cm,dist,contact_intervention=NULL,transmission_intervention=NULL,im=NULL) {
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

    # calculate prob transmission
    if (is.list(cm)) {
        cm_gen <- Reduce('+', cm)
    } else {
        cm_gen <- cm
    }
    if (is.null(im)) {
        im <- matrix(1,nrow = nrow(cm_gen),ncol = ncol(cm_gen))
    }
    J <- ncol(cm_gen)
    V <- diag(rep(gamma,J))
    F1 <- cm_gen
    for (i in 1:J) {
        for (j in 1:J) {
            F1[i,j] <- dist[i]/dist[j]*F1[i,j]*im[i,j]
        }
    }
    K1 <- F1 %*% solve(V)
    l1 <- max(Re(eigen(K1)$values))
    pt <- R0/l1

    # output with class seir_param
    param <- list(R0=R0,
                 sigma=sigma,
                 gamma=gamma,
                 cm=cm,
                 pt=pt,
                 dist=dist,
                 contact_intervention=contact_intervention,
                 transmission_intervention=transmission_intervention,
                 im=im)
    class(param) <- "seir_c_param"

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
#' @param R Initial number of removed
#'
#' @return List of SEIR model initial states
#'
#' @export
seir_c_state0 <- function(S,E,I,R) {
    # assertions
    stopifnot(length(S) == length(E))
    stopifnot(length(E) == length(I))
    stopifnot(length(I) == length(R))
    stopifnot(any(I >= 0))

    # output with class seir_state0
    state0 <- c(S=S,E=E,I=I,R=R)
    class(state0) <- "seir_c_state0"

    # return
    state0
}

#' SEIR with heterogeneous contact differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,E,I,R)
#' @param param The model parameters (R0,sigma,gamma,cm,dist,intervention)
#'
#' @return derivatives of SEIR model states with respect to time
#'
seir_c_model <- function(t,y,parms) {
    with(as.list(c(y, parms)), {

        # account for interventions
        cm_cur <- calculate_current_cm(cm,contact_intervention,t,dist)
        pt_cur <- calculate_current_pt(pt,transmission_intervention,t)

        # population size
        J <- ncol(cm_cur)
        S <- y[1:J]
        E <- y[(J+1):(2*J)]
        I <- y[(2*J+1):(3*J)]
        R <- y[(3*J+1):(4*J)]
        N <- S + E + I + R

        # derived parameters
        # none

        # differential equations
        dS <- numeric(length=J)
        dE <- numeric(length=J)
        dI <- numeric(length=J)
        dR <- numeric(length=J)
        for (i in 1:J) {
            dS[i] <- -(S[i])*sum(pt_cur*im[,i]*cm_cur[i,]*(I/N))
            dE[i] <- (S[i])*sum(pt_cur*im[,i]*cm_cur[i,]*(I/N)) - sigma*E[i]
            dI[i] <- sigma*E[i] - gamma*I[i]
            dR[i] <- gamma*I[i]
        }

        # return
        list(c(dS,dE,dI,dR),
             S=sum(S),
             E=sum(E),
             I=sum(I),
             R=sum(R),
             Ntotal=sum(S) + sum(E) + sum(I) + sum(R))
    })
}
