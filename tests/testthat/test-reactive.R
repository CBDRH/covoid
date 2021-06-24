test_that("reactive updates work (calculate_reactive)",{

    int <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    cm <- import_contact_matrix("Australia","general")
    dist <- import_age_distribution("Australia")
    cm_update <- calculate_reactive(cm,int,c(100),dist=dist)$param
    expect_true(all(cm_update/cm == 0.5))

    cm_update <- calculate_reactive(cm,int,c(10),dist=dist)$param
    expect_true(all(cm_update/cm == 1.0))

    pt <- 0.1
    pt_update <- calculate_reactive(pt,int,c(100),dist=NULL)$param
    expect_true(all(pt_update/pt == 0.5))

    pt <- 0.1
    pt_update <- calculate_reactive(pt,int,c(10),dist=NULL)$param
    expect_true(all(pt_update/pt == 1.0))
})


test_that("reactive logic is correct (pt)",{
    # setup
    pt <- 0.1  # probability of transmission

    # basic tests
    y <- c(100,100)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(pt,int1,y)$param,pt*0.5)

    y <- c(0,0)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(pt,int1,y)$param,pt)

    y <- c(20,20.001)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(pt,int1,y)$param,pt*0.5)

    # over time test
    pt <- 0.1
    ys <- c(40,41,rep(1,10))
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    for (t in 1:length(ys)) {
        reactive_update <- calculate_reactive(pt,int1,ys[t])
        pt_cur <- reactive_update$param
        int1 <- reactive_update$intervention
        expect_equal(pt_cur,pt*ifelse(t %in% 2:6,0.5,1.0))
    }

    # # over time test
    # ys <- c(40,41,rep(6,9),rep(1,5))
    # int1 <- reactive_intervention(threshold=40,reduce=0.5,
    #                               state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    # for (t in 1:length(ys)) {
    #     print(calculate_reactive_pt(pt,int1,ys[t]))
    # }

})

test_that("reactive logic is correct (cm)",{
    # setup
    cm <- matrix(2) # contact rates
    dist <- 1.0

    # basic tests
    y <- c(100,100)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(cm,int2,y,dist)$param,cm*0.5)

    y <- c(0,0)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(cm,int2,y,dist)$param,cm)

    y <- c(20,20.001)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive(cm,int2,y,dist)$param,cm*0.5)

    # over time test
    ys <- c(40,41,rep(1,10))
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    for (t in 1:length(ys)) {
        reactive_update <- calculate_reactive(cm,int2,ys[t])
        cm_cur <- reactive_update$param
        int2 <- reactive_update$intervention
        expect_equal(cm_cur,cm*ifelse(t %in% 2:6,0.5,1.0))
    }
})

test_that("reactive sims work",{
    cm_oz <- import_contact_matrix("Australia","general")
    nJ <- ncol(cm_oz)
    dist_oz <- import_age_distribution("Australia")
    SSv <- dist_oz*5e6
    baseline <- 0.0  # vaccinations at t=0
    S <- SSv*(1-baseline)
    Sv <- SSv*baseline
    E <- rep(0,nJ)
    Ev <- rep(0,nJ)
    I <- Iv <- rep(0,nJ)
    R <- Rv <- rep(0,nJ)
    state0 <- seir_cv_state0(S = S,E = E,I = I,R = R,Sv = Sv,Ev = Ev,Iv = Iv,Rv = Rv)
    ## parameters
    # vaccine effectiveness
    vaceff1 <- rep(0.99,nJ)
    vaceff2 <- rep(0.90,nJ)
    vaceff3 <- rep(0.90,nJ)
    # imported cases
    n_imp_cases <- function(t) {
        10*(t == 10) + 0*(t != 10)
    }
    # no vaccination occurs
    nvac <- function(t) 0
    random_vac_alloc <- function(t,n,s) rep(0,nJ)
    # reactive transmission interventions
    int1 <- reactive_intervention(threshold=40,reduce=0.01,state=reactive_state(length=100))
    # reactive contact rate interventions
    int2 <- reactive_intervention(threshold=40,reduce=0.01,state=reactive_state(length=100))
    param1 <- seir_cv_param(R0 = 2.5,
                            sigma=0.1,
                            gamma = 0.1,
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
    res4 <- simulate_seir_cv(t = 100,state_t0 = state0,param = param1)
    expect_is(res4,"covoid")
    expect_is(plot(res4,y="incidence"),"ggplot")
})





