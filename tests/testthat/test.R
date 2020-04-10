
library(dplyr)

param <- sir_param(R0 = 2.5,gamma = 0.1)
state0 <- sir_state0(S0 = 100,I0 = 1,R0 = 0)
res <- simulate_sir(t = 100,state_t0 = state0,param = param)

res_df = tidyr::pivot_longer(res$epi,-t,names_to = "compartment",values_to = "number")
res_df$compartment = factor(res_df$compartment,levels=unique(res_df$compartment),ordered=TRUE)
ggplot(res_df) +
    geom_line(aes(x = t, y = number, col = compartment)) +
    theme_bw()


param <- seir_param(R0 = 2.5,gamma = 0.1,sigma=10)
state0 <- seir_state0(S0 = 100,E0 = 1, I0 = 0,R0 = 0)
res <- simulate_seir(t = 100,state_t0 = state0,param = param)

res_df = tidyr::pivot_longer(res$epi,-t,names_to = "compartment",values_to = "number")
res_df$compartment = factor(res_df$compartment,levels=unique(res_df$compartment),ordered=TRUE)
ggplot(res_df) +
    geom_line(aes(x = t, y = number, col = compartment)) +
    theme_bw()

param <- seiqhrf_param(R0=2.5,sigma=0.3,gamma1=0.3,gamma2=0.3,gamma3=0.3,Qeff=0.5,Heff=0.99,rho=0.1,alpha=0.2,eta=0.01)
state0 <- seiqhrf_state0(S0=100,E0=1)
res <- simulate_seiqhrf(t = 100,state_t0 = state0,param = param)
COVOIDd::qplot(res,c("S","E","Is","Hp","Rc","F"))

res$epi$N
res$epi$Fatalities + res$epi$Recovered

