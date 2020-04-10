
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
#' @param ... see ggplot2::qplot
#'
#' @method plot covoidd
#'
#' @export
plot.covoidd <- function(x,y,...) {
    df = dcm2df(x)
    df = df[df$compartment %in% y,]
    p = ggplot2::ggplot(df) +
        ggplot2::geom_line(ggplot2::aes(x = t, y = number, col = compartment)) +
        ggplot2::theme_bw()
    p
}
