#' Launch an interactive Shiny session
#'
#' @param example A Shiny app
#' @examples
#' \dontrun{
#' #coviz("seir")
#' }
#' @export
coviz <- function(example) {
    # locate all the shiny app examples that exist
    validExamples <- list.files(system.file("shiny-interfaces", package = "covoid"))

    validExamplesMsg <-
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")

    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||
        !example %in% validExamples) {
        stop(
            'Please run `coviz()` with a valid interface app as an argument.\n',
            validExamplesMsg,
            call. = FALSE)
    }

    # find and launch the app
    appDir <- system.file("shiny-interfaces", example, package = "covoid")
    shiny::runApp(appDir, display.mode = "normal")
}
