
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
