
test_that("models give same results for S_inf with equal R0", {
    TT <- 1000
    res_sir <- simulate_sir(t = TT,
                         state_t0 = sir_state0(S = 1000,I = 1,R = 0),
                         param = sir_param(R0 = 2.5,gamma = 0.1))
    res_seir <- simulate_seir(t = TT,
                          state_t0 = seir_state0(S = 1000,E = 1, I = 0,R = 0),
                          param = seir_param(R0 = 2.5,gamma = 0.1,sigma=0.1))
    expect_equal(res_sir$epi$S[TT], res_seir$epi$S[TT],tolerance=0.1)
})

