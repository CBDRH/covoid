fillt <- function(timevar, value, startDate, endDate, d=0) {
    timeframe <- data.frame(t = seq(1, as.numeric(endDate - startDate), 1))

    tidydata <- data.frame(
        t = floor(timevar - as.numeric(endDate) + 1),
        value = round(value, digits=d)
        )

    # fx <- left_join(timeframe, tidydata, by='t') %>%
    #     stats::as.ts() %>%
    #     imputeTS::na_interpolation(option='linear') %>%
    #     as.data.frame()

    return(timeframe)

}
