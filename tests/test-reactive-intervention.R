library(tidyverse)
library(RColorBrewer)



cm_oz <- import_contact_matrix("Australia","general")
p1bp1b <- round(mean(colSums(cm_oz[14:16, 14:16])), digits = 1)
p1bp2a <- round(mean(colSums(cm_oz[10:13, 14:16])), digits = 1)
p1bp2b <- round(mean(colSums(cm_oz[5:9, 14:16])), digits = 1)
p1bp3 <- round(mean(colSums(cm_oz[1:4, 14:16])), digits = 1)
p2ap2a <- round(mean(colSums(cm_oz[10:13, 10:13])), digits = 1)
p2ap2b <- round(mean(colSums(cm_oz[5:9, 10:13])), digits = 1)
p2ap3 <- round(mean(colSums(cm_oz[1:4, 10:13])), digits = 1)
p2bp2b <- round(mean(colSums(cm_oz[5:9, 5:9])), digits = 1)
p2bp3 <- round(mean(colSums(cm_oz[1:4, 5:9])), digits = 1)
p3p3 <- round(mean(colSums(cm_oz[1:4, 1:4])), digits = 1)

(cm <- matrix(c(p1bp1b, p1bp2a, p1bp2b, p1bp3,
                p1bp2a, p2ap2a, p2ap2b, p2ap3,
                p1bp2b, p2ap2b, p2bp2b, p2bp3,
                p1bp3, p2ap3, p2bp3, p3p3),
              nrow = 4, ncol=4,
              dimnames =list(c('1b', '2a', '2b', '3'), c('1b', '2a', '2b', '3'))))

nJ <- 4

hesitancyDF <- data.frame(
    scenario = rep(1:2, each = 4),
    group = rep(1:4, 2),
    prob = c(0.60, 0.28, 0.07, 0.05, 0.43, 0.36, 0.13, 0.08)
) %>%
    mutate(
        group = factor(group, labels = c('Definitely will', 'Probably will', 'Probably not', 'Definitely not')),
        scenario = factor(scenario, labels = c('August 2020', 'January 2021'))
    )

## The breakdown across 4 levels of willingness (Definitely, Probably, Maybe, No). Sums to 1.
hesitancyPct1 <- hesitancyDF$prob[1:4]
hesitancyPct2 <- hesitancyDF$prob[5:8]

## The probability of accepting the vaccine in each willingness group
propensity <- c(0.03, 0.015, 0.0025, 0.0005)

## Population at time zero
phasePop <- c(3E6, 6E6, 11E6, 6E6)

# Functions to define the daily available doses

## Pessimistic scenario: grows from 60,000 to 100,000 in 90 days; steady thereafter
nvacLo <- function(t) { (t < 90)*(60000 + (100000-60000)*(t/90)) + (t >=90)*100000 }

# Optimistic scenario: grows from 60,000 to 150,000 in 60 days; steady thereafter
nvacHi <- function(t) { (t < 60)*(60000 + (150000-60000)*(t/60)) + (t >=60)*150000 }



SSv <- rep(phasePop, each = 4) * c(0.43, 0.36, 0.13, 0.08)
dist_oz <- SSv/sum(SSv)

## initial states
baseline <- 0
vaceff1 <- rep(0.99, nJ*4)
vaceff2 <- rep(0.90, nJ*4)
vaceff3 <- rep(0.9, nJ*4)
S <- SSv
Sv <- rep(0, nJ*4)
E <- rep(0, nJ*4)
Ev <- rep(0, nJ*4)
I <- Iv <- rep(0, nJ*4)
R <- Rv <- rep(0, nJ*4)
state0 <- seir_cv_state0(S = S,E = E,I = I,R = R,Sv = Sv,Ev = Ev,Iv = Iv,Rv = Rv)

## parameters
# imported case
n_imp_cases <- function(t) {
    0*(t <= 365) + (t > 365)*rpois(n = 1,lambda = 200)*0.05
}
# number of available vaccines
nvac <- function(t) {
    # number of available vaccinations (total)
    1000*(t < 30) + 20000*(t >= 30)
}
# vaccine allocation function
# prob_vaccine <- rep(dist_oz,each=2)
# prob_vaccine[seq(2,length(dist_oz2),by=2)] <- 0.0
# random_vac_alloc <- function(n,s) {
#     vaccination_allocation_mm(n,s,list(p=prob_vaccine,s0=S,half=0.05))
# }

# The day that each group becomes eligible to receive the vaccine
openDay <- c(1, 30, 90, 120)

# contact matrix
cm_oz2 = matrix(ncol=ncol(cm)*4,nrow=nrow(cm)*4)
for (i in 1:(ncol(cm)*4)) {
    cm_oz2[i,] <- rep(cm[ceiling(i/4),],each=4)
}

failure <- function(t, arrival_params){

    with (arrival_params, {
        incoming <- tibble(
            date = seq(1, days, by = 1),
            land = arrivals,
            inQH = runner::sum_run(x = land, k = 14, idx = date),
            failure = rbinom(days, inQH, p)
        )

        return(incoming$failure[t])
    })
}

n_imp_cases <- function(t){

    failure(t,
            arrival_params = list(
                arrivals = round(c(rnorm(40, 1000, 50), rnorm(30, 5000, 200), rnorm(180, 1000, 50), rnorm(30, 5000, 200), rnorm(365-280, 1000, 50))),
                p = (3.1/1E5)/14, # Failure risk based on https://www.medrxiv.org/content/10.1101/2021.02.17.21251946v1.full.pdf
                days = 365
            )
    )

}

mm <- function(p,half) {
    # Michaelis–Menten like dynamics
    # p: proportion vaccinated
    # half: point at which vaccination rate halfs
    (p/(p+half))*(1.0+half)
}

vaccination_allocation_mm <- function(n,s,vac_params) {
    # n: number of available vaccines
    # s: number of people in group j
    # in vac_params:
    # p: probability of group j getting vaccinates
    # s0: group size at time 0
    # half: percentage of population at which slow down begins

    with (vac_params, {
        # number vaccinated
        nvac0 <- pmin(p*n,s*ceiling(p))
        remaining_vac <- n - sum(nvac0)

        # allocate remaining vaccines
        while(remaining_vac > 1 & any((nvac0 != s)[as.logical(ceiling(p))])) {
            fully_allocated <- nvac0 == s
            p1 <- (p*!fully_allocated)/sum(p*!fully_allocated)
            nvac0 <- pmin(nvac0 + p1*remaining_vac,s*ceiling(p))
            remaining_vac <- n - sum(nvac0)
        }
        return(nvac0*mm(s/s0,half))
    })
}

random_vac_alloc <- function(n,s) {
    vaccination_allocation_mm(n,s,list(p=dist_oz,s0=S,half=0.05))
}

# Interventions
int1 <- data.frame(
    threshold = 40,
    reduce = .8
)


int2 <- data.frame(
    threshold = 40,
    reduce = .6
)


int3 <- data.frame(
    threshold = 40,
    reduce = .4
)


int4 <- data.frame(
    threshold = 40,
    reduce = .2
)

int5 <- data.frame(
    threshold = 10,
    reduce = .8
)


int6 <- data.frame(
    threshold = 10,
    reduce = .6
)


int7 <- data.frame(
    threshold = 10,
    reduce = .4
)


int8 <- data.frame(
    threshold = 10,
    reduce = .2
)

class(int1) <- c('data.frame', 'intervention')
class(int2) <- c('data.frame', 'intervention')
class(int3) <- c('data.frame', 'intervention')
class(int4) <- c('data.frame', 'intervention')

param0 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases)

param1 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int1)

param2 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int2)

param3 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int3)

param4 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int4)


param5 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int5)
param6 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int6)

param7 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int7)

param8 <- seir_cv_param(R0 = 2.5,
                        sigma=0.1,
                        gamma = 0.1,
                        cm=cm_oz2,
                        dist=dist_oz,
                        vaceff1=vaceff1,
                        vaceff2=vaceff2,
                        vaceff3=vaceff3,
                        nvac=nvacHi,
                        vac_alloc=random_vac_alloc,
                        n_imp=n_imp_cases,
                        transmission_intervention = int8)

## simulation
res0 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param0)
res1 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param1)
res2 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param2)
res3 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param3)
res4 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param4)
res5 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param5)
res6 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param6)
res7 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param7)
res8 <- simulate_seir_cv(t = 365, state_t0 = state0, param = param8)

dfTest <- data.frame(
    res1 = res1$epi$I,
    res2 = res2$epi$I,
    res3 = res3$epi$I,
    res4 = res4$epi$I,
    res5 = res5$epi$I,
    res6 = res6$epi$I,
    res7 = res7$epi$I,
    res8 = res8$epi$I
) %>%
    mutate(day = row_number()) %>%
    pivot_longer(cols = starts_with('res'),
                 values_to = 'I',
                 names_to = 'sim',
                 names_prefix = 'res') %>%
    mutate(threshold = ifelse(as.numeric(sim) > 4, 1, 0),
           reduction = (as.numeric(sim)-1) %% 4 + 1,
           threshold = factor(threshold, level = c(0, 1), labels = c('40 daily cases', '10 daily cases')),
           reduction = factor(reduction, levels = c(1,2,3,4),
                        labels = c('20% reduction',
                                     '40% reduction',
                                     '60% reduction',
                                     '80% reduction'))
           )


ggplot(dfTest,
       aes(x = day, y = I)) +
    geom_line(color = 'blue') +
    facet_wrap(threshold~reduction, nrow = 2) +
    scale_y_continuous('Daily incidence') +
    scale_x_continuous('Day')

