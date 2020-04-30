
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
#' @param popfrac logical If \code{TRUE}, plot prevalence of values rather than numbers
#' @param main string Main title for the plot
#' @param ... see ggplot2::qplot
#'
#' @method plot covoidd
#'
#' @export
plot.covoid <- function(x,y,popfrac=FALSE,cumulative=FALSE,main="",...) {
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

#' Plot a contact matrix
#'
#' @param x the contact matrix
#'
#' @export
plot.contact_matrix <- function(x,...) {
    df = as.data.frame(x)
    df$age_contact = as.character(seq(5,80,by=5))
    df$age_contact = factor(df$age_contact,levels=as.character(seq(5,80,by=5)),ordered=TRUE)
    df = tidyr::pivot_longer(data = df,cols = 1:16,names_to = "age_individual",values_to = "contact_rate")
    df$age_individual = factor(df$age_individual,levels=as.character(seq(5,80,by=5)),ordered=TRUE)
    ggplot2::ggplot(df, ggplot2::aes(y=age_contact, x=age_individual, fill=contact_rate)) +
        ggplot2::geom_tile()

}
