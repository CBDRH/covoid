
#' List countries for which a contact matrix is available
#'
#' @section References
#' Prem, K., Cook, A. R., & Jit, M. (2017). Projecting social contact matrices in 152 countries using contact surveys and demographic data. PLoS computational biology, 13(9), e1005697.
#'
#' @export
available_contact_matrices <- function() {
    tmp_env = new.env()
    data(extended_polymod_countries,envir = tmp_env)
    extended_polymod_countries = tmp_env$extended_polymod_countries
    extended_polymod_countries
}

#' Import a contact matrix for country and setting
#'
#' Each row i of a column j of the contact matrix corresponds to the number of contacts
#' made by an individual in group j with an individual in group i.
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
    tmp_env = new.env()
    data(list = paste0(country,"_",setting),envir = tmp_env)
    cm = tmp_env$cm_all
    class(cm) = c(class(cm),"contact_matrix")
    cm
}

#' List countries for which a (discrete) age distribution is available
#'
#' @section References
#' United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
#'
#' @export
age_distributions_un <- function() {
    tmp_env = new.env()
    data(age_distributions_un,envir = tmp_env)
    age_distributions_un = tmp_env$age_distributions_un
    age_distributions_un
}

#' Load age distribution
#'
#'
#' @param country A character indicating which country.
#'
#' @section References
#' United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
#'
#' @examples
#' import_age_distribution("Australia")
#'
#' @export
import_age_distribution <- function(country) {
    stopifnot(country %in% age_distributions_un())
    tmp_env = new.env()
    data(pop_age_distribution_un,envir = tmp_env)
    pop_age_distribution_un = tmp_env$pop_age_distribution_un
    age_dist = pop_age_distribution_un$prop[pop_age_distribution_un$country == country]
    age_dist[16] = age_dist[16] + sum(age_dist[17:19])
    age_dist = age_dist[1:16]
    class(age_dist) = c(class(age_dist),"age_distribution")
    age_dist
}


#' Load population estimates
#'
#'
#' @param country A character indicating which country.
#'
#' @section References
#' United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
#'
#' @examples
#' import_total_population("Australia")
#'
#' @export
import_total_population <- function(country) {
    stopifnot(country %in% age_distributions_un())
    tmp_env = new.env()
    data(total_population_un,envir = tmp_env)
    total_population_un = tmp_env$total_population_un
    pop = total_population_un$pop[total_population_un$country == country]
    pop
}


