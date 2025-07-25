#' Set up a Shiny app to use overshiny
#'
#' Put [useOverlay()] in your Shiny app's UI to use `overshiny`'s interactive
#' plot overlays.
#'
#' This can go anywhere in your UI and it can be inserted multiple times with
#' no ill effect. This also calls [shinyjs::useShinyjs()], as `overshiny`
#' depends on `shinyjs`.
#'
#' @return Returns HTML that gets inserted into the `<head>` of your app.
#'
#' @examples
#' ui <- shiny::fluidPage(
#'     useOverlay()
#'     # further UI elements here . . .
#' )
#'
#' server <- function(input, output) {
#'     # server code here . . .
#' }
#'
#' if (interactive()) {
#'     shiny::shinyApp(ui, server)
#' }
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
useOverlay = function()
{
    shiny::singleton(
        htmltools::tags$head(
            shinyjs::useShinyjs(),
            htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "overshiny/overshiny.css"),
            overshiny_script
        )
    )
}

#' Create an overlay token input control
#'
#' Create a token that can be dragged onto an (overlay plot)[overlayPlotOutput()]
#' to create a new overlay.
#'
#' Note that the DOM ID of the token will be converted to
#' `"overshiny_token_<inputId>"`. This transformed ID is important for internal
#' interaction logic (e.g. for use with JavaScript drag/drop handlers). When
#' referencing the token programmatically (e.g. in CSS selectors or custom
#' JavaScript), use the full prefixed ID (see examples).
#'
#' @param inputId The `input` slot used for the token.
#' @param name Text (or HTML) to be displayed on the token itself.
#' @param label Text label that will appear on the overlay.
#'
#' @return An overlay token input control that can be added to a UI definition.
#'
#' @examples
#' ui <- shiny::fluidPage(
#'     useOverlay(),
#'     overlayToken("add", "Add new overlay", "Overlay"),
#'     # The token's HTML id will be "overshiny_token_add"
#'     shiny::tags$style(shiny::HTML("#overshiny_token_add { cursor: grab; }"))
#' )
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
overlayToken = function(inputId, name, label = name)
{
    htmltools::tags$span(htmltools::HTML(name),
        `data-label` = label,
        id = ovid("token", inputId),
        class = "overshiny-token")
}

overlayColours = function(n)
{
    colours = c("#162d60", "#3771c8", "#37c8ab", "#ffd42a", "#ff7f2a", "#ff0066", "#bc5fd3", "#9d93ac");
    picker = list(2, c(2, 6), c(2, 4, 6), c(2:4, 6), 2:6, 2:7, 1:7, 1:8)

    n = round(n)
    if (n <= 0) return (character(0))
    if (n <= 8) return (colours[picker[[n]]])

    grDevices::colorRampPalette(colours)(n)
}

overlayRects = function(outputId, n, display,
    colours = overlayColours, opacity = 0.25, icon = shiny::icon("gear"))
{
    col = paste0(colours(n), sprintf("%02x", round(255 * pmax(0, pmin(1, opacity)))))
    rects = list()
    for (i in seq_len(n)) {
        rects[[i]] = htmltools::div(
            htmltools::div(icon,
                id = ovid("clicker", display, i),
                class = "overshiny-clicker overshiny-noselect",
                `data-id` = ovid("overlay", outputId, i)),
            htmltools::div(id = ovid("label", outputId, i),
                class = "overshiny-label overshiny-noselect"),
            htmltools::div(id = ovid("dropdown", outputId, i),
                class = "overshiny-dropdown"),
            id = ovid("overlay", outputId, i),
            style = paste0("background-color: ", col[i], "; display: ", display),
            class = "overshiny-noselect overshiny-rect"
        )
    }

    do.call(htmltools::tagList, rects)
}

overlayDisplay = function(outputId, width, height, element)
{
    htmltools::div(id = ovid("display", outputId),
        class = "overshiny-display",
        element,
        htmltools::div(id = ovid("bounds", outputId),
            class = "overshiny-bounds",
            style = paste0("width: ", width, "px; height: ", height, "px")
        )
    )
}

overlayImageOutput = function(outputId, width, height)
{
    overlayDisplay(outputId, width, height, shiny::imageOutput(outputId, width, height))
}

#' Create a plot output element with overlays
#'
#' Render a [shiny::renderPlot()] within an application page, with support for
#' overlays.
#'
#' @param outputId The `output` slot where the plot will be rendered using
#'     [shiny::renderPlot()], with a call to [overlayBounds()].
#' @param width,height Image width and height. Must be a valid CSS unit, like
#'     `"100%"`, `"400px"`, or `"auto"`, or a number, interpreted as pixels.
#'
#' @return A plot output element that can be added to a UI definition.
#'
#' @examples
#' ui <- shiny::fluidPage(
#'     useOverlay(),
#'     overlayPlotOutput("my_plot", 640, 480)
#'     # further UI elements here . . .
#' )
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
overlayPlotOutput = function(outputId, width, height)
{
    overlayDisplay(outputId, width, height, shiny::plotOutput(outputId, width, height))
}
