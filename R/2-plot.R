
#' Create ggplot2 ready dataframe: wrapper around tidyr's pivot_longer
#'
#' @param mod DCM model
#'
dcm2df <- function(mod) {
    mod_df = tidyr::pivot_longer(mod$epi,-t,names_to = "compartment",values_to = "number")
    mod_df$compartment = factor(mod_df$compartment,levels=unique(mod_df$compartment),ordered=TRUE)
    mod_df
}

#' Plot with ggplot2
#'
#' @param x model
#' @param y compartments to plot, a character vector
#' @param popfrac If \code{TRUE}, plot prevalence of values rather than numbers
#' @param ... see ggplot2::qplot
#'
#' @method plot covoidd
#'
#' @export
plot.covoidd <- function(x,y,popfrac=FALSE,cumulative=FALSE,main="Doherty model",...) {
#   yn = unique(c("N",y))
    df = dcm2df(x)
    df = df[df$compartment %in% y,]
    if(cumulative){
     df$number <- cumsum(df$number)
    }
    if(popfrac){
     df$number <- df$number / x$epi[["N"]][1]
     if(cumulative){
       ylab <- "Cumulative proportion of the population"
     }else{
       ylab <- "Proportion of the population"
     }
    }else{
     if(cumulative){
       ylab <- "Cumulative number of people"
     }else{
       ylab <- "Number of people"
     }
    }
    xlab <- "time since outbreak (days)"
    p = ggplot2::ggplot(df) + ggplot2::labs(y=ylab, y=xlab) +
        ggplot2::ggtitle(main) +
        ggplot2::geom_line(ggplot2::aes(x = t, y = number, col = compartment)) +
        ggplot2::theme_bw()
    p
}
