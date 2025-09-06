#' Align overlays with a ggplot2 or base plot
#'
#' Sets the pixel and coordinate bounds of the overlay area based on a
#' [ggplot2::ggplot()] object or base R plot. This ensures that overlays are
#' positioned correctly in both visual and coordinate space.
#'
#' Call this function within [shiny::renderPlot()], before returning the
#' ggplot object (if using ggplot2) or `NULL` (if using base R plotting).
#'
#' @param ov A [shiny::reactiveValues()] object returned by [overlayServer()].
#' @param plot A [ggplot2::ggplot()] object used for overlay alignment, or the
#'     character string `"base"` if you are using base R plotting.
#' @param xlim,ylim Vectors defining the coordinate limits for overlays.
#'     Use `NA` to inherit axis limits from the plot panel.
#' @param row,col Row and column of the facet panel (if applicable). This only
#'     works with ggplot2 plots; base R plots with multiple panels are not
#'     supported.
#'
#' @return The ggplot object (for ggplot2) or `NULL` (for base R plotting), to
#' be returned from the [shiny::renderPlot()] block.
#'
#' @examples
#' server <- function(input, output) {
#'     ov <- overlayServer("my_plot", 1, 1)
#'     output$my_plot <- shiny::renderPlot({
#'         plot(1:100, sin(1:100 * 0.1), type = "l")
#'         overlayBounds(ov, "base", xlim = c(1, 100))
#'     })
#'     # further server code here . . .
#' }
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
overlayBounds = function(ov, plot, xlim = c(NA, NA), ylim = c(NA, NA), row = 1L, col = 1L)
{
    input = shiny::getDefaultReactiveDomain()$input

    if (ggplot2::is_ggplot(plot)) {
        rect = panel_rects_ggplot(plot)
    } else if (identical(plot, "base")) {
        rect = panel_rects_base()
    } else {
        stop("Unrecognised plot type")
    }

    rect = rect[rect$row == row & rect$col == col]
    if (nrow(rect) != 1) {
        stop("Invalid row, col")
    }

    # Adjust NA to bounds of plot
    if (is.na(xlim[1])) xlim[1] = rect$xmin
    if (is.na(xlim[2])) xlim[2] = rect$xmax
    if (is.na(ylim[1])) ylim[1] = rect$ymin
    if (is.na(ylim[2])) ylim[2] = rect$ymax
    xlim = as.numeric(xlim)
    ylim = as.numeric(ylim)

    # bx, bw: x and w of bounds in normalised image coordinates (0 to 1)
    bx = rect$x + rect$w * (xlim[1] - rect$xmin) / (rect$xmax - rect$xmin)
    bw = rect$w * (xlim[2] - xlim[1]) / (rect$xmax - rect$xmin)

    # by, bh: similar
    by = rect$y + rect$h * (ylim[1] - rect$ymin) / (rect$ymax - rect$ymin)
    bh = rect$h * (ylim[2] - ylim[1]) / (rect$ymax - rect$ymin)

    # Get width and height of target plot, plus left offset of plot
    outputId = shiny::isolate(ov$outputId)
    shinyjs::runjs(paste0(
        'var plot = $("#', outputId, '");\n',
        'Shiny.setInputValue("overshiny_return",',
        ' [plot.width(), plot.height(), plot.offset().left]);'
    ))
    shiny::req(input$overshiny_return)
    img_width = input$overshiny_return[1];
    img_height = input$overshiny_return[2];
    left_offset = input$overshiny_return[3];

    l = bx * img_width
    w = bw * img_width
    b = by * img_height
    h = bh * img_height

    shiny::isolate({
        ov$bound_cx = xlim[1]
        ov$bound_cw = xlim[2] - xlim[1]
        ov$bound_px = l + left_offset
        ov$bound_pw = w
        ov$bound_py = b
        ov$bound_ph = h
        ov$py = rep(0, ov$n)
        ov$ph = h * (1 - 1:ov$n * ov$stagger)
        ov$update_px() # in case bounds have changed
    })

    setcss(ovid("bounds", outputId), left = paste0(l, "px"), bottom = paste0(b, "px"),
        width = paste0(w, "px"), height = paste0(h, "px"))

    return (plot)
}
