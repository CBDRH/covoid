
test_that("models give same results for S_inf with equal R0", {
    TT <- 1000
    # homogeneous models
    res_sir <- simulate_sir(t = TT,
                         state_t0 = sir_state0(S = 1000,I = 2,R = 0),
                         param = sir_param(R0 = 2.5,gamma = 0.1))
    res_seir <- simulate_seir(t = TT,
                          state_t0 = seir_state0(S = 1000,E = 2, I = 0,R = 0),
                          param = seir_param(R0 = 2.5,gamma = 0.1,sigma=0.1))
    res_seir1 <- simulate_seir1(t = TT,
                                state_t0 = seir1_state0(S = 1000, E1 = 2, E2 = 0),
                                param = seir1_param(R0=2.5,sigma1=0.1,sigma2=0.1,gamma1=0.1,gamma2=0.1,gamma3=0.1,
                                                    Qeff=0,Heff=0,rho=0,alpha=0,eta=0))
    res_seir2 <- simulate_seir2(t = TT,
                                state_t0 = seir2_state0(S = 1000, E1 = 2, E2 = 0),
                                param = seir2_param(R0=2.5,lambdaimp=0,sigma1=0.1,sigma2=0.1,gamma1=0.1,
                                                    gamma2=0.1,gammaq1=0.1,gammaq2=0.1,Qeff=0,Meff=0,rho=0,eta=0,
                                                    alphamBeta=0,probHospGivenInf=0,delta=0,kappa=0,pm=0))
    # age structured models
    cm <- matrix(c(1,1,1,1),ncol=2,nrow=2)
    dist <- c(0.5,0.5)
    res_sir_c <- simulate_sir_c(t = TT,
                                state_t0 = sir_c_state0(S = c(500,500),I=c(1,1),R=c(0,0)),
                                param = sir_c_param(R0=2.5,gamma=0.2,cm=cm,dist=dist))
    res_seir_c <- simulate_seir_c(t = TT,
                                state_t0 = seir_c_state0(S = c(500,500),E=c(1,1),I=c(0,0),R=c(0,0)),
                                param = seir_c_param(R0=2.5,sigma=0.1,gamma=0.1,cm=cm,dist=dist))
    # compare to base SIR model
    expect_equal(res_sir$epi$S[TT], res_seir$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir1$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir2$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_sir_c$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir_c$epi$S[TT],tolerance=0.1)
    #
    expect_equal(res_sir$epi$S[TT], res_seir$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir1$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir2$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_sir_c$epi$S[TT],tolerance=0.1)
    expect_equal(res_sir$epi$S[TT], res_seir_c$epi$S[TT],tolerance=0.1)


})


