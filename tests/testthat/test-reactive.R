test_that("reactive logic is correct (pt)",{
    # setup
    pt <- 0.1  # probability of transmission

    # basic tests
    y <- c(100,100)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_pt(pt,int1,y),pt*0.5)

    y <- c(0,0)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_pt(pt,int1,y),pt)

    y <- c(20,20.001)
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_pt(pt,int1,y),pt*0.5)

    # over time test
    ys <- c(40,41,rep(1,10))
    int1 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    for (t in 1:length(ys)) {
        expect_equal(calculate_reactive_pt(pt,int1,ys[t]),pt*ifelse(t %in% 2:6,0.5,1.0))
    }
})

test_that("reactive logic is correct (cm)",{
    # setup
    cm <- matrix(2) # contact rates
    dist <- 1.0

    # basic tests
    y <- c(100,100)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_cm(cm,int2,y,dist),cm*0.5)

    y <- c(0,0)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_cm(cm,int2,y,dist),cm)

    y <- c(20,20.001)
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    expect_equal(calculate_reactive_cm(cm,int2,y,dist),cm*0.5)

    # over time test
    ys <- c(40,41,rep(1,10))
    int2 <- reactive_intervention(threshold=40,reduce=0.5,
                                  state=reactive_state(inplace=FALSE,length=5,lowerbound=5))
    for (t in 1:length(ys)) {
        expect_equal(calculate_reactive_cm(cm,int2,ys[t],dist),cm*ifelse(t %in% 2:6,0.5,1.0))
    }
})
