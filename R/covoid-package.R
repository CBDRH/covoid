## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to COVOID. For more see https://cbdrh.github.io/covoidance/")
}
