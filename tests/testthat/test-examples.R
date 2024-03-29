test_that("seir_cm_* examples work",{
    # Shared state for examples
    cm_oz <- import_contact_matrix("Australia","general")
    dist_oz <- import_age_distribution("Australia")
    nJ <- ncol(cm_oz)
    S <- rep(1000,nJ)
    E <- rep(1,nJ)
    I <- rep(0,nJ)
    R <- rep(0,nJ)
    state0 <- seir_c_state0(S = S,E = E,I = I,R = R)

    # Example 1: no interventions
    param1 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,cm=cm_oz,dist=dist_oz)
    res1 <- simulate_seir_c(t = 150,state_t0 = state0,param = param1)
    expect_is(res1,"covoid")
    expect_is(plot(res1,y=c("S","E","I","R")),"ggplot")

    # Example 2: general physical distancing, school closures and masks/handwashing
    cm_oz_all <- import_contact_matrix("Australia","general")
    cm_oz_sch <- import_contact_matrix("Australia","school")
    cm <- list(all = cm_oz_all, sch = cm_oz_sch)
    # separate out school and general population contact rates
    cm_oz_all <- cm_oz_all - cm_oz_sch
    int <- list(sch=contact_intervention(start = 10,stop = 150,reduce = 0.2,start_delay = 5,stop_delay = 5),
                all=contact_intervention(start = 10,stop = 150,reduce = 0.8,start_delay = 5,stop_delay = 5))
    int_t <- transmission_intervention(start = 10,stop = 200,reduce = 0.9,start_delay = 5,stop_delay = 5)
    param2 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,
                           cm=list(sch=cm_oz_sch,all=cm_oz_all),
                           dist=dist_oz,
                           contact_intervention=int,
                           transmission_intervention=int_t)
    res2 <- simulate_seir_c(t = 150,state_t0 = state0,param = param2)
    expect_is(res2,"covoid")
    expect_is(plot(res2,y=c("S","E","I","R")),"ggplot")

    # Example 3: as #2 with under 15s less infectious
    im <- matrix(1,ncol=16,nrow=16)
    im[,1:3] <- 0.8
    param3 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,
                           cm=list(sch=cm_oz_sch,
                                   all=cm_oz_all),
                           dist=dist_oz,
                           contact_intervention=int,
                           transmission_intervention=int_t,
                           im=im)
    res3 <- simulate_seir_c(t = 200,state_t0 = state0,param = param3)
    expect_is(res3,"covoid")
    expect_is(plot(res3,y=c("S","E","I","R")),"ggplot")

    # Example from seir-cm-vaccine.R
    # The two functions (mm and vaccination_allocation_mm) will be used in vaccine allocation
    # import and setup baseline states
    cm_oz <- import_contact_matrix("Australia","general")
    nJ <- ncol(cm_oz)
    dist_oz <- import_age_distribution("Australia")
    SSv <- dist_oz*5e6
    baseline <- 0.0  # vaccinations at t=0
    S <- SSv*(1-baseline)
    Sv <- SSv*baseline
    E <- rep(0,nJ)
    Ev <- rep(0,nJ)
    H <- rep(0,nJ)
    Hv <- rep(0,nJ)
    I <- Iv <- rep(0,nJ)
    R <- Rv <- rep(0,nJ)
    state0 <- seir_cv_state0(S,E,I,H,R,
                             Sv,Ev,Iv,Hv,Rv)
    ## parameters
    # vaccine effectiveness
    vaceff1 <- rep(0.99,nJ)
    vaceff2 <- rep(0.90,nJ)
    vaceff3 <- rep(0.90,nJ)
    # hospitalisation by age
    phosp = rep(0.1,nJ)
    phospv = rep(0.01,nJ)
    # imported cases
    n_imp_cases <- function(t) {
        0*(t <= 365) + (t > 365)*rpois(n = 1,lambda = 200)
    }
    # number of available vaccines
    nvac <- function(t) {
        # number of available vaccinations (total)
        1000*(t < 30) + 20000*(t >= 30)
    }
    # wrap vaccine allocation function
    random_vac_alloc <- function(t,n,s) {
        vaccination_allocation_mm(t,n,s,list(p=dist_oz,s0=S,half=0.05))
    }
    # reactive transmission interventions
    int1 <- reactive_intervention(threshold=40,reduce=0.5,state=reactive_state(inplace=FALSE))
    # reactive contact rate interventions
    int2 <- reactive_intervention(threshold=40,reduce=0.5,state=reactive_state(inplace=FALSE))
    param1 <- seir_cv_param(R0 = 2.5,
                            sigma=0.1,
                            gamma = 0.1,
                            phosp = phosp,
                            phospv = phospv,
                            thosp = 1/7,
                            cm=cm_oz,
                            dist=dist_oz,
                            vaceff1=vaceff1,
                            vaceff2=vaceff2,
                            vaceff3=vaceff3,
                            nvac=nvac,
                            contact_intervention = int1,
                            transmission_intervention=int2,
                            vac_alloc=random_vac_alloc,
                            n_imp=n_imp_cases)
    ## simulation
    res4 <- simulate_seir_cv(t = 365*2,state_t0 = state0,param = param1)
    expect_is(res4,"covoid")
    expect_is(plot(res4,y = c("Sv","S")),"ggplot")
    expect_is(plot(res4,y = c("H","Hv")),"ggplot")
    expect_is(plot(res4,y = c("contact_intervention")),"ggplot")
})

