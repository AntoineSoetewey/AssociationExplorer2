#' Launch the AssociationExplorer2 Shiny Application
#'
#' This function launches the AssociationExplorer2 Shiny application in
#' your default web browser. The app provides interactive tools for
#' exploring statistical associations, correlation networks, bivariate
#' visualizations, and summary tables, with optional support for survey
#' weights and range-based filtering of association strengths.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()},
#'   such as \code{port} or \code{launch.browser}.
#'
#' @return The function is called for its side effect of launching the app.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_associationexplorer()
#' }
run_associationexplorer <- function(...) {

  # Required runtime packages
  required_pkgs <- c(
    "shiny", "visNetwork", "ggplot2", "plotly",
    "reactable", "dplyr", "tidyr", "readr",
    "readxl", "stringr", "purrr", "tibble",
    "forcats", "scales", "igraph"
  )

  # Check whether all required packages are installed
  missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "The following required packages are not installed: ",
      paste(missing, collapse = ", "),
      ". Please install them before launching the application.",
      call. = FALSE
    )
  }

  # Locate app directory inside the installed package
  app_dir <- system.file("app", package = "AssociationExplorer2")
  if (app_dir == "") {
    stop(
      "Could not find the application directory within the package. ",
      "Try reinstalling the AssociationExplorer2 package.",
      call. = FALSE
    )
  }

  # Launch the Shiny application
  shiny::runApp(app_dir, ...)
}
