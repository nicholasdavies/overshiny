#' Interactive overlays on Shiny plots
#'
#' `overshiny` provides draggable and resizable rectangular elements that
#' overlay plots in Shiny apps. This may be useful in applications where users
#' need to define regions on the plot for further input or processing.
#' Currently, the overlays are only designed to move along the x axis of the
#' plot.
#'
#' The package exports a setup helper ([useOverlay()]),
#' UI components ([overlayToken()], [overlayPlotOutput()]),
#' a server-side controller ([overlayServer()]), and a function for
#' aligning overlays to a ggplot2 or base plot ([overlayBounds()]).
#'
#' @docType package
#' @name overshiny
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onAttach = function(...) {
    # Create link to javascript and css files for package
    shiny::addResourcePath("overshiny", system.file("www", package = "overshiny"))
}
