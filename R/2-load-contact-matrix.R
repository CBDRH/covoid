
#' Available contact matrices
#'
#' @section References
#' Prem, K., Cook, A. R., & Jit, M. (2017). Projecting social contact matrices in 152 countries using contact surveys and demographic data. PLoS computational biology, 13(9), e1005697.
#'
#' @export
available_contact_matrices <- function() {
    obj = local(get(data(extended_polymod_countries)))
    obj
}

#' Import a contact matrix for country and setting
#'
#' @param country A character indicating which country.
#' @param setting The settting for the desired contact matrix.
#' Can be one of "general","work","school","home","other"
#'
#' @section References
#' Prem, K., Cook, A. R., & Jit, M. (2017). Projecting social contact matrices in 152 countries using contact surveys and demographic data. PLoS computational biology, 13(9), e1005697.
#'
#' @examples
#' import_contact_matrix("Australia","general")
#'
#' @export
import_contact_matrix <- function(country,setting) {
    stopifnot(setting %in% c("general","work","school","home","other"))
    stopifnot(country %in% available_contact_matrices())
    tmp_env <- new.env()
    data(list = paste0(country,"_",setting),envir = tmp_env)
    cm = tmp_env$cm_all
    cm
}
