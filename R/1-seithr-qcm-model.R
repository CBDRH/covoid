
#' Simulate a deterministic SEIR-QHT model
#'
#' S = Susceptible
#' Sc = Susceptible and in self isolation
#'
#'
#'
#' Rh = Removed after hospitalisation
#' R = Removed
#'
#' @param t Number of time steps over which to sample from the model
#' @param state_t0 Initial state of the model (see ?sir_state0)
#' @param param Model parameters (see ?sir_param)
#'
#' @return Object of class covoid
#'
#' @examples
#'
#' @export
simulate_seithr_qc <- function(t,state_t0,param) {
    # assertions
    stopifnot(class(state_t0) == "seithr_qc_state0")
    stopifnot(class(param) == "seithr_qc_param")

    # simulation
    mod = deSolve::ode(y=state_t0,
                       times=1:t,
                       func=seithr_qc_model,
                       parms=list(pt=param$pt,
                                  Qeff=param$Qeff,
                                  Ieff=param$Ieff,
                                  Heff=param$Heff,
                                  delta1=param$delta1,
                                  delta2=param$delta2,
                                  delta3=param$delta3,
                                  delta4=param$delta4,
                                  sigma1=param$sigma1,
                                  sigma2=param$sigma2,
                                  p_severe=param$p_severe,
                                  p_isolate=param$p_isolate,
                                  p_tested=param$p_tested,
                                  gamma1=param$gamma1,
                                  gamma2=param$gamma2,
                                  cm=param$cm,
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


#' SEIR-QHT model parameters
#'
#' Setup function
#'
#' @param R0 Basic/empirical reproduction number (S -> I), can be a function of t.
#' @param Qeff ...
#' @param Teff ...
#' @param Heff ...
#' @param delta1 ...
#' @param delta2 ...
#' @param delta3 ...
#' @param delta4 ...
#' @param sigma1 ...
#' @param sigma2 ...
#' @param p_severe ...
#' @param p_isolate ...
#' @param gamma1 ...
#' @param gamma2 ...
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
seithr_qc_param <- function(R0,Qeff,Ieff,Heff,delta1,delta2,delta3,delta4,
                            sigma1,sigma2,p_severe,p_isolate,p_tested,gamma1,gamma2,cm,dist,
                            contact_intervention=NULL,transmission_intervention=NULL,im=NULL) {
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
        cm_gen = Reduce('+', cm)
    } else {
        cm_gen = cm
    }
    if (is.null(im)) {
        im = matrix(1,nrow = nrow(cm_gen),ncol = ncol(cm_gen))
    }
    J = ncol(cm_gen)
    V = diag(rep(1/(1/sigma2+1/gamma1+1/gamma2),J))
    F1 = cm_gen
    for (i in 1:J) {
        for (j in 1:J) {
            F1[i,j] = dist[i]/dist[j]*F1[i,j]*im[i,j]
        }
    }
    K1 = F1 %*% solve(V)
    l1 = max(Re(eigen(K1)$values))
    pt = R0/l1

    # output with class sir_param
    param = list(R0=R0,
                 Qeff=Qeff,
                 Ieff=Ieff,
                 Heff=Heff,
                 delta1=delta1,
                 delta2=delta2,
                 delta3=delta3,
                 delta4=delta4,
                 sigma1=sigma1,
                 sigma2=sigma2,
                 p_severe=p_severe,
                 p_isolate=p_isolate,
                 p_tested=p_tested,
                 gamma1=gamma1,
                 gamma2=gamma2,
                 cm=cm,
                 pt=pt,
                 dist=dist,
                 contact_intervention=contact_intervention,
                 transmission_intervention=transmission_intervention,
                 im=im)
    class(param) = "seithr_qc_param"

    # return
    param
}

#' SEIR-QHT model inital state
#'
#' Setup function
#' @param S
#' @param E1
#' @param E2
#' @param I1m
#' @param I1s
#' @param I2
#' @param T
#' @param H
#' @param Sc
#' @param Ec1
#' @param Ec2
#' @param Ic1m
#' @param Ic1s
#' @param Ic2
#' @param Hc
#' @param R
#' @param Rh
#'
#' @return List of SEIR-QHT model initial states
#'
#' @export
seithr_qc_state0 <- function(S,E1,E2,I1m,I1s,I2,I2i,H,Sc,Ec1,Ec2,Ic1m,Ic1s,Ic2,Hc,R,Rh,Tested,Detect,NotDetect) {
    # assertions
    # add a check

    # output with class sir_state0
    state0 = c(S=S,E1=E1,E2=E2,I1m=I1m,I1s=I1s,I2=I2,I2i=I2i,H=H,
               Sc=Sc,Ec1=Ec1,Ec2=Ec2,Ic1m=Ic1m,Ic1s=Ic1s,Ic2=Ic2,
               Hc=Hc,R=R,Rh=Rh,Tested=Tested,Detect=Detect,NotDetect=NotDetect)
    class(state0) = "seithr_qc_state0"

    # return
    state0
}


#' SEITHR-QC with heterogeneous contact differential equations
#'
#' @param t current time (dt scale)
#' @param state_t0 Initial state (S,I,R)
#' @param param The model parameters (beta,R0,gamma,cm)
#'
#' @return derivatives of SEIR model states with respect to time
#'
seithr_qc_model <- function(t,y,parms) {
    with(as.list(c(y, parms)), {

        # account for interventions
        cm_cur = calculate_current_cm(cm,contact_intervention,t,dist)
        pt_cur = calculate_current_pt(pt,transmission_intervention,t)

        # population size
        J = ncol(cm_cur)
        S = y[1:(1*J)]
        E1 = y[(J+1):(2*J)]
        E2 = y[(2*J+1):(3*J)]
        I1m = y[(3*J+1):(4*J)]
        I1s = y[(4*J+1):(5*J)]
        I2 = y[(5*J+1):(6*J)]
        I2i = y[(6*J+1):(7*J)]
        H = y[(7*J+1):(8*J)]
        Sc = y[(8*J+1):(9*J)]
        Ec1 = y[(9*J+1):(10*J)]
        Ec2 = y[(10*J+1):(11*J)]
        Ic1m = y[(11*J+1):(12*J)]
        Ic1s = y[(12*J+1):(13*J)]
        Ic2 = y[(13*J+1):(14*J)]
        Hc = y[(14*J+1):(15*J)]
        R = y[(15*J+1):(16*J)]
        Rh = y[(16*J+1):(17*J)]
        Tested = y[(17*J+1):(18*J)]
        Detect = y[(18*J+1):(19*J)]
        NotDetect = y[(19*J+1):(20*J)]

        N = S+E1+E2+I1m+I1s+I2+I2i+H+Sc+Ec1+Ec2+Ic1m+Ic1s+Ic2+Hc+R+Rh

        dS = numeric(length=J)
        dE1 = numeric(length=J)
        dE2 = numeric(length=J)
        dI1m = numeric(length=J)
        dI1s = numeric(length=J)
        dI2 = numeric(length=J)
        dI2i = numeric(length=J)
        dH = numeric(length=J)
        dSc = numeric(length=J)
        dEc1 = numeric(length=J)
        dEc2 = numeric(length=J)
        dIc1m = numeric(length=J)
        dIc1s = numeric(length=J)
        dIc2 = numeric(length=J)
        dHc = numeric(length=J)
        dR = numeric(length=J)
        dRh = numeric(length=J)
        dTested = numeric(length=J)
        dDetect = numeric(length=J)
        dNotDetect = numeric(length=J)

        for (i in 1:J) {
            # derived parameters
            lambda = sum(pt_cur*im[,i]*cm_cur[i,]*((E2+I1m+I1s+I2)/N)) +
                (1-Qeff)*sum(pt_cur*im[,i]*cm_cur[i,]*((Ec2+Ic1m+Ic1s+Ic2)/N)) +
                (1-Ieff)*sum(pt_cur*im[,i]*cm_cur[i,]*((I2i)/N)) +
                (1-Heff)*sum(pt_cur*im[,i]*cm_cur[i,]*((H+Hc)/N))

            # case discovery
            dTested[i] = p_tested*(sigma2*E2[i] + sigma2*Ec2[i])
            dDetect[i] = gamma1*Ic1s[i] + gamma1*I1s[i] + dTested[i]
            dNotDetect[i] = (1-p_tested)*sigma2*E2[i]
            DetectRate = dDetect[i]/(dNotDetect[i] + dDetect[i])
            DetectRate = replace(DetectRate,is.na(DetectRate),0)
            delta1_ = delta1*DetectRate
            delta2_ = delta2*DetectRate
            delta3_ = delta3*DetectRate
            delta4_ = delta4*DetectRate

            # differential equations
            dS[i]   = -(S[i])*lambda
            dE1[i]  = (1-delta1_)*(S[i])*lambda - sigma1*E1[i]
            dE2[i]  = (1-delta2_)*sigma1*E1[i] - sigma2*E2[i]
            dI1m[i] = (1-delta3_)*(1-p_severe)*sigma2*E2[i] - gamma1*I1m[i]
            dI1s[i] = (1-delta3_)*(p_severe)*sigma2*E2[i] - gamma1*I1s[i]
            dI2[i]  = (1-delta4_)*(1-p_isolate)*gamma1*I1m[i] - gamma2*I2[i]
            dI2i[i] = (1-delta4_)*(p_isolate)*gamma1*I1m[i] - gamma2*I2i[i]
            dH[i]   = gamma1*I1s[i] - gamma2*H[i]
            dSc[i]  = 0
            dEc1[i] = (delta1_)*(S[i])*lambda - sigma1*Ec1[i]
            dEc2[i] = (delta2_)*sigma1*E1[i] + sigma1*Ec1[i] - sigma2*Ec2[i]
            dIc1m[i] = (delta3_)*(1-p_severe)*sigma2*E2[i] + (1-p_severe)*sigma2*Ec2[i] - gamma1*Ic1m[i]
            dIc1s[i] = (delta3_)*(p_severe)*sigma2*E2[i] + (p_severe)*sigma2*Ec2[i] - gamma1*Ic1s[i]
            dIc2[i] = (delta4_)*gamma1*I1m[i] + gamma1*Ic1m[i] - gamma2*Ic2[i]
            dHc[i]  = gamma1*Ic1s[i] - gamma2*Hc[i]
            dR[i]   = gamma2*Ic2[i] + gamma2*I2i[i] + gamma2*I2[i]
            dRh[i]  = gamma2*Hc[i] + gamma2*H[i]
        }

        # return
        list(c(dS,dE1,dE2,dI1m,dI1s,dI2,dI2i,dH,dSc,dEc1,dEc2,dIc1m,dIc1s,dIc2,dHc,dR,dRh,
               dTested,dDetect,dNotDetect),
             S=sum(S,Sc),
             E=sum(E1,Ec1),
             I=sum(E2,Ec2,I1m,I1s,I2,I2i,H,Ic1m,Ic1s,Ic2,Hc),
             R=sum(R,Rh),
             Detect=sum(Detect),
             Ntotal=sum(S,E1,E2,I1m,I1s,I2,I2i,H,Sc,Ec1,Ec2,Ic1m,Ic1s,Ic2,Hc,R,Rh))
    })
}
