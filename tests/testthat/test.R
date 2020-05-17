
test_that("models give same results for S_inf with equal R0", {
    TT <- 1000
    res_sir <- simulate_sir(t = TT,
                         state_t0 = sir_state0(S0 = 1000,I0 = 1,R0 = 0),
                         param = sir_param(R0 = 2.5,gamma = 0.1))
    res_seir <- simulate_seir(t = TT,
                          state_t0 = seir_state0(S0 = 1000,E0 = 1, I0 = 0,R0 = 0),
                          param = seir_param(R0 = 2.5,gamma = 0.1,sigma=0.1))
    res_seihrq <- simulate_seihrq(t = TT,
                         state_t0 = seihrq_state0(S0=1000,E0=1,I20=0),
                         param = seihrq_param(R0=2.5,sigma=0.3,gamma1=0.3,gamma2=0.3,gamma3=0.3,
                                              Qeff=0.0,Heff=0.0,rho=0.0,alpha=0.0,eta=0.00))
    expect_equal(res_sir$epi$S[TT], res_seir$epi$S[TT])
    expect_equal(res_sir$epi$S[TT], res_seihrq$epi$S[TT])
})

test_that("seir_cm_* examples work",{
    # Shared state for examples
    cm_oz <- import_contact_matrix("Australia","general")
    dist_oz <- import_age_distribution("Australia")
    S <- rep(1000,nJ)
    E <- rep(1,nJ)
    I <- rep(0,nJ)
    R <- rep(0,nJ)
    state0 <- seir_c_state0(S = S,E = E,I = I,R = R)

    # Example 1: no interventions
    param1 <- seir_c_param(R0 = 2.5,sigma=0.1,gamma = 0.1,cm=cm_oz,dist=dist_oz)
    nJ <- ncol(cm_oz)
    res1 <- simulate_seir_c(t = 150,state_t0 = state0,param = param1)
    expect_is(res1,"covoid")
    expect_is(plot(res1,y=c("S","E","I","R")),"ggplot")

    # Example 2: general physical distancing, school closures and masks/handwashing
    cm <- list(all = cm_oz_all, sch = cm_oz_sch)
    cm_oz_all <- import_contact_matrix("Australia","general")
    cm_oz_sch <- import_contact_matrix("Australia","school")
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
    expect_is(plot(res,y=c("S","E","I","R")),"ggplot")

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
    plot(res3,y=c("S","E","I","R"))
    expect_is(res3,"covoid")
    expect_is(plot(res3,y=c("S","E","I","R")),"ggplot")
})
