#' Snap overlays to a grid
#'
#' Use this function as the `snap` parameter of [overlayServer()] to enable
#' a simple snap-to-grid behaviour for your overlay. It will ensure your
#' overlays stay within the bounds of the plot, and snap both position and
#' width of each overlay to the specified grid.
#'
#' @param anchor The location of any specific gridline.
#' @param step The space between gridlines.
#' @param min_width (optional) Minimum width of an overlay; default (`NA`) sets
#'     to `step`. Use `NULL` for no minimum.
#' @param max_width (optional) Maximum width of an overlay; default (`NA`) sets
#'     to the largest size that accommodates the width of the overlay bounds,
#'     accounting for the grid. Use `NULL` for no maximum.
#'
#' Note that the default values snap overlays to whole numbers.
#'
#' @return A snapping function suitable to pass to [overlayServer()] as the
#' `snap` argument.
#'
#' @examples
#' server <- function(input, output) {
#'     ov <- overlayServer("my_plot", 8, snap = snap_grid())
#'     # further server code here . . .
#' }
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
snap_grid = function(anchor = 0, step = 1,
    min_width = NA, max_width = NA) function(ov, i)
{
    # Helper: snap x to nearest grid point
    snap = function(x, a = anchor, func = round) {
        x = func((x - a) / step)
        return (x * step + a)
    }

    # Set min_width, max_width
    if (is.na(min_width)) {
        min_width = step
    }
    if (is.na(max_width)) {
        scx0 = snap(ov$bound_cx, func = ceiling)
        scx1 = snap(ov$bound_cx + ov$bound_cw, func = floor)
        max_width = scx1 - scx0
    }

    # Remove any "out of bounds" overlays with a tolerance of 1 pixel
    tol = ov$bound_cw / ov$bound_pw
    oob = seq_len(ov$n) %in% i &
        (ov$cx0 < ov$bound_cx - tol | ov$cx1 > ov$bound_cx + ov$bound_cw + tol)
    ov$active[oob] = FALSE

    # Get snapped widths
    widths = snap(ov$cx1[i] - ov$cx0[i], a = 0)
    if (!is.null(min_width)) widths = pmax(min_width, widths)
    if (!is.null(max_width)) widths = pmin(max_width, widths)

    # Adjust position
    ov$cx0[i] = pmax(snap(ov$bound_cx),
        pmin(snap(ov$bound_cx + ov$bound_cw) - widths, snap(ov$cx0[i])))
    ov$cx1[i] = ov$cx0[i] + widths
}
