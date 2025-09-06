#' Headings for overlay dropdown menus
#'
#' Use a call to one of these functions as the `heading` parameter of
#' [overlayServer()] to provide a heading on the overlay dropdown menu
#' reporting the start and end position of the overlay. For numbers, the
#' heading from [rangeHeading()] will be e.g. "1.5 – 3.5". For dates, the
#' heading from [dateHeading()] will be e.g. "2025-05-01 – 2025-06-01".
#'
#' @param sep A separator that will be inserted between the start and end
#'     position of the overlay. Use `NULL` to only print the start position.
#' @param format For [dateHeading()] only, the date format to use, e.g.
#'     "%Y-%m-%d". See the documentation for [format.Date()] for details.
#' @param ... Further arguments to be passed to [format()], such as `digits`,
#'     `scientific`, etc. See the documentation for [format()] for details.
#'
#' @return A heading function suitable to pass to [overlayServer()] as the
#' `heading` argument.
#' @examples
#' server <- function(input, output) {
#'     ov <- overlayServer("my_plot", 8, heading = dateHeading("%b %d"))
#'     # further server code here . . .
#' }
#'
#' @seealso [overlayServer()], for a complete example.
#'
#' @export
rangeHeading = function(..., sep = " – ") function(ov, i)
{
    cx = format(c(ov$cx0[i], ov$cx1[i]), ...)
    if (is.null(sep)) {
        cx[1]
    } else {
        paste(cx[1], cx[2], sep = sep)
    }
}

#' @rdname rangeHeading
#' @export
dateHeading = function(format, ..., sep = " – ") function(ov, i)
{
    cx = as.Date(c(ov$cx0[i], ov$cx1[i]), origin = "1970-01-01")
    cx = format.Date(cx, format, ...)
    if (is.null(sep)) {
        cx[1]
    } else {
        paste(cx[1], cx[2], sep = sep)
    }
}
