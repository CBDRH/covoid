
#' Michaelis–Menten like dynamics
#'
#' @param p proportion vaccinated
#' @param half point at which vaccination rate halfs
#'
#' @return
#' @export
#'
#' @examples
mm <- function(p,half) {
         (p/(p+half))*(1.0+half)
     }


#' Vaccine allocation with Michaelis–Menten tails
#'
#' @param t time t
#' @param n number of available vaccines at time t
#' @param s number of people in group j
#' @param vac_params a list including:
#'  p: probability of group j getting vaccinates
#'  s0: group size at time 0
#'  half: percentage of population at which slow down begins
#'
#' @return
#' @export
#'
#' @examples
vaccination_allocation_mm <- function(t,n,s,vac_params) {
 # t: time t
 # n: number of available vaccines
 # s: number of people in group j
 # in vac_params:
 # p: probability of group j getting vaccinates
 # s0: group size at time 0
 # half: percentage of population at which slow down begins

 with (vac_params, {
     # number vaccinated
     nvac0 <- pmin(p*n,s*ceiling(p))
     remaining_vac <- n - sum(nvac0)

     # allocate remaining vaccines
     while(remaining_vac > 1 & any((nvac0 != s)[as.logical(ceiling(p))])) {
         fully_allocated <- nvac0 == s
         p1 <- (p*!fully_allocated)/sum(p*!fully_allocated)
         nvac0 <- pmin(nvac0 + p1*remaining_vac,s*ceiling(p))
         remaining_vac <- n - sum(nvac0)
     }
     return(nvac0*mm(s/s0,half))
 })
}



#' Vaccine allocataion incorporating phases and propensity to be vaccinated
#'
#' Distribute vaccines across different phases with time-dependent eligibility for different phases and varying vaccine willingness within phases
#'
#' @param t Time t
#' @param n Number of vaccine doses available at time t
#' @param s A vector with the number of people in group j on day t
#' @param vac_params A list specifying openDay (Day the vaccines open for each phase) and propensity (The probability of being available for vaccination on a single day)
#'
#' @return A vector distributing the \code{n} vaccine doses across the \code{J} groups
#' @export
#'
#' @examples
#' # Distribute 20 vaccines across two phases with a high and low propensity subgroup in each phase
#' vax_allocation(t=1, n=20, s=rep(100,4), vac_params = list(openDay = c(1,8), propensity = c(.3,.7)))
vaccination_allocation_pp <- function(t,n,s,vac_params) {

    with (vac_params, {

        # Logic tests
        stopifnot(all(propensity >=0) & all(propensity <= 1)) # Propensities should be between 0 and 1
        stopifnot(n >= 0) # Can't have negative vaccines available
        stopifnot(length(s) == length(openDay)*length(propensity)) # Number of groups = number of phases x number of hesitancy categories

        # A TRUE/FALSE vector indicating which groups are eligible based on the current day
        eligible <- t >= rep(openDay, each = length(propensity))

        # The number eligible on day t
        # el_t <- ifelse(eligible, popUnvaccinated, 0)
        el_t <- ifelse(eligible, s, 0)

        # The number happy to be vaccinated on day t
        keen <- el_t*rep(propensity, length(openDay))

        # The proportional distribution of available doses across keen groups
        allocatedDoses <- floor(pmin(n*(keen/sum(keen)), keen))

        return(allocatedDoses)

    })
}
