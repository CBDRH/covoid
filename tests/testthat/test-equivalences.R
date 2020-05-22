

test_that("seir1 and seir2 give equal results with equal parameters", {
    TT <- 1000
    # homogeneous models
    res_seir1 <- simulate_seir1(t = TT,
                                state_t0 = seir1_state0(S = 1000, E1 = 2, E2 = 0),
                                param = seir1_param(R0=2.5,sigma1=0.1,sigma2=0.1,gamma1=0.1,gamma2=0.1,gamma3=0.1,
                                                    Qeff=0,Heff=0,rho=0,alpha=0,eta=0))
    res_seir2 <- simulate_seir2(t = TT,
                                state_t0 = seir2_state0(S = 1000, E1 = 2, E2 = 0),
                                param = seir2_param(R0=2.5,lambdaimp=0,sigma1=0.1,sigma2=0.1,gamma1=0.1,
                                                    gamma2=0.1,gammaq1=0.1,gammaq2=0.1,Qeff=0,Meff=0,rho=0,eta=0,
                                                    alphamBeta=0,probHospGivenInf=0,delta=0,kappa=0,pm=0))
    expect_equal(res_seir1$epi$S,res_seir2$epi$S,tolerance = 0.1)
    expect_equal(res_seir1$epi$E1,res_seir2$epi$E1,tolerance = 0.1)
    expect_equal(res_seir1$epi$E2,res_seir2$epi$E2,tolerance = 0.1)
})



test_that("sir_c and seir_c give near (~5) equal results with equal parameters", {
    TT <- 1000
    # age structured models
    cm <- matrix(c(1,1,1,1),ncol=2,nrow=2)
    dist <- c(0.5,0.5)
    res_sir_c <- simulate_sir_c(t = TT,
                                state_t0 = sir_c_state0(S = c(500,500),I=c(1,1),R=c(0,0)),
                                param = sir_c_param(R0=2.5,gamma=0.2,cm=cm,dist=dist))
    res_seir_c <- simulate_seir_c(t = TT,
                                  state_t0 = seir_c_state0(S = c(500,500),E=c(1,1),I=c(0,0),R=c(0,0)),
                                  param = seir_c_param(R0=2.5,sigma=8,gamma=0.2,cm=cm,dist=dist))
    # compare to base SIR model
    expect_equal(res_sir_c$epi$S,res_seir_c$epi$S,tolerance = 5)
    expect_equal(res_sir_c$epi$I,res_seir_c$epi$I,tolerance = 5)
})
