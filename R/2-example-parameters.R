#' SEIHR-Q model preset parameter sets
#'
#' @param set the parameter set to use
#'
#' @export
seihrq_param_set <- function(set) {
    stopifnot(set %in% c("A"))
    switch(set,
           A = seihrq_param(R0=2.5,sigma=0.3,gamma1=0.3,gamma2=0.3,gamma3=0.3,
                             Qeff=0.5,Heff=0.99,rho=0.1,alpha=0.2,eta=0.01))
}
