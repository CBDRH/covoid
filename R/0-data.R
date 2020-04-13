#' Australian healthcare capacity
#'
#' A dataset of the number of ICU and Ward (overnight) beds, and maximal number of
#' emergency department (ED) and general practise (GP) consulations per day in Australia and the Australian
#' states and territories
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{icu_beds}{Number of available ICU}
#'   \item{ward_beds}{Number of available Ward beds (minus psychiatric)}
#'   \item{emergency_consultations_daily}{Maximum number of Emergency Department consulations per day}
#'   \item{gp_consulatations_daily}{Maximum number of General Practise consulations per day}
#'   ...
#' }
#' @source \url{https://www.aihw.gov.au/reports/hospitals/hospital-resources-2017-18-ahs/contents/hospitals-and-average-available-beds}
"hosp_cap_au"
