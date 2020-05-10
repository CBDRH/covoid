
# Function to launch a clickr tool
clickrPlot <- function(df, startDate, endDate) {
    ggplot(rbind(df, data.frame(x=startDate, y=100, dropx=0, dropy=0, dist=0, lab= "")), aes(as.Date(x, origin = '1970-01-01'), y)) +
        geom_point(aes(as.Date(x, origin = '1970-01-01'), y), shape=15, size=8, color = "red", alpha = 0.25) +
        geom_line(color = "red", size=3) +
        geom_segment(aes(x=startDate, y = 100, xend = endDate, yend = 100), size=3, color = "pink") +
        geom_text(x=(startDate + (endDate - startDate)/2), y = 95, label = "Without intervention", color = "pink", size = 5) +
        geom_label(aes(label = lab)) +
        geom_label(aes(x=startDate, y = 100, label = "100%")) +
        scale_x_date(date_labels="%d%b%Y") +
        coord_cartesian(xlim = c(startDate, endDate), ylim = c(0, 100)) +
        labs(x = "Date", y = "Percentage of daily contacts compared \n to pre-intervention level (%)") +
        theme(panel.background=element_rect(fill = "gray20"),
              panel.grid = element_line(colour = "lightblue1"))
}



# Function to add a point
addPoint <- function(df, xClick, yClick){
    add_row <- data.frame(x = xClick,
    y = yClick,
    dropx = 0,
    dropy = 0,
    dist = 0,
    lab = paste0(round(yClick, digits = 0), "%"))

    # add row to the data.frame
    df <- rbind(df, add_row)
    df
}

# Function to drop a point
dropPoint <- function(df, xClick, yClick){
    df$dropx <- as.numeric(xClick)
    df$dropy <- as.numeric(yClick)
    df$dist <- sqrt((df$x - df$dropx)^2 + (df$y - df$dropy)^2)

    # Drop selected from the data.frame
    df <- filter(df, dist > 20)
}


# Funtion to fill in interim timepoints between clicks

fillTime <- function(df, startDate, endDate){

    timeframe <- data.frame(t = seq(from = 1, to = as.numeric(endDate - startDate), by = 1))

    tidydata <- rbind(data.frame(t=1, value=100),
                      data.frame(
                          t = floor((df$x) - as.numeric(startDate)) ,
                          value = round(df$y, digits=0)
                      )
    )

    fx <- left_join(timeframe, tidydata, by='t') %>%
        as.ts() %>%
        imputeTS::na_interpolation(option='linear')
    out <- data.frame(time = fx[,1], c_reduce = round(fx[,2]/100, digits = 3) )

}



# Function to undo last click
undo <- function(df){
        rem_row <- df[-nrow(df), ]
        rem_row
}


# Function to prepare the clicked values as a presentable dataframe

clickrTable <- function(df, startDate){

rbind(df, data.frame(x=startDate, y=100, dropx=0, dropy=0, dist=0, lab= "100%")) %>%
    dplyr::arrange(x) %>%
    mutate(
        Date = format(as.Date(x, origin = '1970-01-01'), "%A %d %B, %Y"),
        Value = lab
    ) %>%
    select("Date", "Value") %>%
    DT::datatable(options = list(lengthMenu = c(4, 8),
                                 searching = FALSE,
                                 lengthChange = FALSE
    )
    )

}


# Function to update hover value
clickrHover <- function(df){
    out <- data.frame(
        x = df$x,
        y = df$y,
        lab = paste0(format(as.Date(df$x, origin = '1970-01-01'), "%d %b"), "\n", "Value = ", round(df$y, digits=0)),
        fulllab = paste0(format(as.Date(df$x, origin = '1970-01-01'), "%a %d %b, %Y"), ": ", round(df$y, digits=0), "%")
        )
    out
}

