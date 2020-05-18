
#' Simulate a deterministic age structured SIR model
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' S = Susceptible
#' I = Infectious
#' R = Removed
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?sir_state0)
#' @param param Model parameters (see ?sir_param)
#'
#' @return Object of class covoid and dcm (from the package EpiModels)
#'
#' @examples
#'
#' cm_oz <- import_contact_matrix("Australia","general")
#' cm_oz <- 0.5*(cm_oz +t(cm_oz))  # while no age groups available
#' param <- sir_c_param(R0 = 2.5,gamma = 0.1,cm=cm_oz)
#' nJ = ncol(cm_oz)
#' S = rep(100,nJ)
#' I = rep(1,nJ)
#' R = rep(0,nJ)
#' state0 <- sir_c_state0(S = S,I = I,R = R)
#' res <- simulate_sir_c(t = 150,state_t0 = state0,param = param)
#' plot(res,y=c("S","I","R"),main="Heterogeneous mixing")
#'
#' # compare with homogeneous
#' param <- sir_param(R0 = 2.5,gamma = 0.1)
#' state0 <- sir_state0(S = 1600,I = 16,R = 0)
#' res <- simulate_sir(t = 150,state_t0 = state0,param = param)
#' plot(res,c("S","I","R"),main="Homogeneous mixing")
#'
#' @export
simulate_sir_c <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "sir_c_state0")
    stopifnot(class(param) == "sir_c_param")

    # simulation
    mod <- deSolve::ode(y=state_t0,
                       times=1:t,
                       func=sir_c_model,
                       parms=list(pt=param$pt,
                                  gamma=param$gamma,
                                  cm=param$cm,
                                  pt=param$pt,
                                  dist=param$dist,
                                  im=param$im,
                                  contact_intervention=param$contact_intervention,
                                  transmission_intervention=param$transmission_intervention))

    # make the output a dataframe
    out = list(param = param,
               epi = data.frame(mod))
    names(out$epi)[1] = "t"

    # return
    class(out) = c("covoid",class(out))
    out
}


#' Age structured SIR model parameters
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' Define parameter values for age structured SIR models
#'
#' @param R0 Basic/empirical reproduction number (S -> I), can be a function of t.
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
#' @return List of SIR model parameters
#'
#' @export
sir_c_param <- function(R0,gamma,cm,dist,contact_intervention=NULL,transmission_intervention=NULL,im=NULL) {
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

    # output with class sir_param
    param <- list(R0=R0,
                 gamma=gamma,
                 cm=cm,
                 pt=pt,
                 dist=dist,
                 contact_intervention=contact_intervention,
                 transmission_intervention=transmission_intervention,
                 im=im)
    class(param) <- "sir_c_param"

    # return
    param
}

#' Age structured SIR model inital state
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' Define intial state values for age structured SIR models
#'
#' @param S Initial number of susceptibles
#' @param I Initial number of infected
#' @param R Initial number of removed
#'
#' @return List of SIR model initial states
#'
#' @export
sir_c_state0 <- function(S,I,R) {
    # assertions
    stopifnot(length(S) == length(I))
    stopifnot(length(I) == length(R))
    stopifnot(any(I >= 0))

    # output with class sir_state0
    state0 <- c(S=S,I=I,R=R)
    class(state0) <- "sir_c_state0"

    # return
    state0
}


#' SIR with heterogeneous contact differential equations
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state (S,I,R)
#' @param param The model parameters (beta,R0,gamma,cm)
#'
#' @return derivatives of SIR model states with respect to time
#'
sir_c_model <- function(t,y,parms) {
    with(as.list(c(y, parms)), {

        # account for interventions
        cm_cur <- calculate_current_cm(cm,contact_intervention,t,dist)
        pt_cur <- calculate_current_pt(pt,transmission_intervention,t)

        # population size
        J <- ncol(cm_cur)
        S <- y[1:J]
        I <- y[(J+1):(2*J)]
        R <- y[(2*J+1):(3*J)]
        N <- S + I + R

        # derived parameters
        # none

        # differential equations
        dS <- numeric(length=J)
        dI <- numeric(length=J)
        dR <- numeric(length=J)
        for (i in 1:J) {
            dS[i] <- -(S[i])*sum(pt_cur*im[,i]*cm_cur[i,]*(I/N))
            dI[i] <- (S[i])*sum(pt_cur*im[,i]*cm_cur[i,]*(I/N)) - gamma*I[i]
            dR[i] <- gamma*I[i]
        }

        # return
        list(c(dS,dI,dR),
             S=sum(S),
             I=sum(I),
             R=sum(R),
             Ntotal=sum(S) + sum(I) + sum(R))
    })
}
