#' @export
useOverlay = function()
{
    htmltools::tags$head(
        overshiny_style,
        overshiny_script
    )
}

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
            style = paste0("background-color: ", col[i], ";  display: ", display),
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

#' @export
overlayImageOutput = function(outputId, width, height)
{
    overlayDisplay(outputId, width, height, shiny::imageOutput(outputId, width, height))
}

#' @export
overlayPlotOutput = function(outputId, width, height)
{
    overlayDisplay(outputId, width, height, shiny::plotOutput(outputId, width, height))
}
