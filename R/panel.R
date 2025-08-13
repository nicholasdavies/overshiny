# Panel rects for ggplot2 plots
panel_rects_ggplot = function(plot)
{
    devsize = graphics::par("fin") * 25.4 # Device size in mm
    built = ggplot2::ggplot_build(plot)
    gtable = ggplot2::ggplot_gtable(built)
    to_mm = function(x) grid::convertUnit(x, "mm", valueOnly = TRUE)

    # With a set of lengths (widths/heights) from a gtable and a total length
    # (plot width/height in mm), replace null lengths with mm lengths
    expand_length = function(lengths, total_mm)
    {
        leeway_mm = total_mm - sum(to_mm(lengths))
        null_length = sum(sapply(unclass(lengths),
            function(x) if (x[[3]] == 5L) x[[1]] else 0))

        unit_class = class(lengths)
        lengths = unclass(lengths)
        for (l in seq_along(lengths)) {
            if (lengths[[l]][[3]] == 5L) { # 5L is code for null units
                lengths[[l]][[1]] = leeway_mm * lengths[[l]][[1]] / null_length
                lengths[[l]][[3]] = 7L # change to mm
            }
        }
        class(lengths) = unit_class

        return (lengths)
    }

    # Get width and height in mm for gtable as realized plot
    w = to_mm(expand_length(lengths = gtable$widths, total_mm = devsize[1]))
    h = to_mm(expand_length(lengths = gtable$heights, total_mm = devsize[2]))

    # Create data.frame to return
    layout = as.data.frame(gtable[[2]])
    p = layout[grepl("^panel", layout$name), ]
    p$x = sapply(1:nrow(p), function(i) sum(w[seq_len(p$l[i] - 1)]) / devsize[1])
    p$y = sapply(1:nrow(p), function(i) 1 - sum(h[seq_len(p$t[i])]) / devsize[2])
    p$w = sapply(1:nrow(p), function(i) sum(w[p$l[i]:p$r[i]]) / devsize[1])
    p$h = sapply(1:nrow(p), function(i) sum(h[p$t[i]:p$b[i]]) / devsize[2])

    # Add coordinate information
    # This is more complicated than seems necessary because ggplot doesn't
    # necessarily seem to name the panels correctly, e.g. panel-X-Y may not be
    # either the panel at row X column Y, nor the panel at row Y column X.
    panel_grobids = which(grepl("^panel", gtable[[2]]$name)) # grob indices of panels
    panel_ggnames = gtable[[2]]$name[panel_grobids] # ggplot2 names of panels
    panel_grobnames = sapply(panel_grobids, function(i) gtable[[1]][[i]]$name) # names of grobs for each panel
    panel_pids = as.integer(sapply(regmatches(panel_grobnames,
        regexec("^panel-([0-9]+)\\.", panel_grobnames)), `[`, 2)) # panel ids (1 to N) for each panel

    # Add panel id to each row of the p data frame (NA for not a valid panel...)
    p$panel_id = panel_pids[match(p$name, panel_ggnames)]
    p = p[!is.na(p$panel_id), ]

    # Add coordinate limits:
    p$xmin = sapply(p$panel_id, function(i) built$layout$panel_params[[i]]$x.range[1])
    p$xmax = sapply(p$panel_id, function(i) built$layout$panel_params[[i]]$x.range[2])
    p$ymin = sapply(p$panel_id, function(i) built$layout$panel_params[[i]]$y.range[1])
    p$ymax = sapply(p$panel_id, function(i) built$layout$panel_params[[i]]$y.range[2])

    # Add layout information
    p_id_order = match(p$panel_id, built$layout$layout$PANEL)
    p$row = built$layout$layout[p_id_order, "ROW"]
    p$col = built$layout$layout[p_id_order, "COL"]

    # Add label
    lab0 = which(names(built$layout$layout) == "COL")
    lab1 = which(names(built$layout$layout) == "SCALE_X")
    if (lab1 == lab0 + 1) {
        p$label = NA_character_
    } else if (lab1 == lab0 + 2) {
        p$label = as.character(built$layout$layout[p_id_order, lab0 + 1])
    } else {
        p$label = paste(
            built$layout$layout[p_id_order, lab0 + 1],
            built$layout$layout[p_id_order, lab0 + 2],
            sep = "~")
    }

    # Tidy data frame
    p = p[, c("panel_id", "row", "col", "label", "x", "y", "w", "h", "xmin", "xmax", "ymin", "ymax", "name")]
    names(p)[13] = "grobname"
    p = p[order(p$panel_id), ]
    rownames(p) = 1:nrow(p)

    return (p)
}

# Panel rects for base plots
panel_rects_base = function()
{
    # Get dimensions of plot
    mfrow = graphics::par("mfrow")
    if (!identical(mfrow, c(1L, 1L))) {
        stop("This only works with a single base plot, not multiple plots using mfrow/mfcol.")
    }

    # Add the currently active panel
    plt = graphics::par("plt")
    usr = graphics::par("usr")

    data = data.frame(panel_id = 1L, row = 1L, col = 1L, label = NA,
        x = plt[1], y = plt[3], w = plt[2] - plt[1], h = plt[4] - plt[3],
        xmin = usr[1], xmax = usr[2], ymin = usr[3], ymax = usr[4])

    return (data)
}

#' Adjust margins of a ggplot2 plot
#'
#' To avoid the overlay rectangles moving around when the plot margins change,
#' you can use this function to set specific margins for your plot. You will
#' probably want to specify a large enough margin so that the axes and legends
#' don't go out of the plot area.
#'
#' Note that this only works with `ggplot2` plots. For base plots, you can set
#' the margins using `par(plt = c(x1, x2, y1, y2))`. See [graphics::par()] for
#' details.
#'
#' @param g A `ggplot2` plot.
#' @param t,r,b,l Top, right, bottom, and left margins to set.
#' @param unit Unit for the margins (see [grid::unit()] for permissible units).
#'     The default, `"npc"`, refers to fractions of the overall plot area.
#'
#' @return A `ggplot2` plot with margins adjusted.
#'
#' @examples
#' plot1 = ggplot2::ggplot(data.frame(x = rnorm(10), y = rnorm(10))) +
#'     ggplot2::geom_point(ggplot2::aes(x, y))
#' plot2 = remargin(plot1, 0.1, 0.1, 0.1, 0.1) # plot with 10% margins all around
#'
#' @export
remargin = function(g, t, r, b, l, unit = "npc")
{
    to_npc = function(x) grid::convertUnit(x, "npc", valueOnly = TRUE)

    t = to_npc(grid::unit(t, unit))
    r = to_npc(grid::unit(r, unit))
    b = to_npc(grid::unit(b, unit))
    l = to_npc(grid::unit(l, unit))

    # With a set of lengths (widths/heights) from a gtable, return lengths
    # in normalised parent coordinates of the plot margins
    margins = function(lengths, total_mm)
    {
        nulls = sapply(unclass(lengths), function(x) x[[3]] == 5L)
        if (!any(nulls)) {
            stop("Could not retrieve plot margins.")
        }
        first = which(nulls)[1]
        last = which(nulls)[sum(nulls)]
        length0 = sum(to_npc(lengths[1:(first - 1)]))
        length1 = sum(to_npc(lengths[(last + 1):length(lengths)]))
        return (c(length0, length1))
    }

    grob = cowplot::as_grob(g)
    m_horz = margins(grob$widths)  # left, right
    m_vert = margins(grob$heights) # top, bottom
    W = length(grob$widths)
    H = length(grob$heights)

    grob$widths[[1]]  = grob$widths[[1]]  + (grid::unit(l, "npc") - grid::unit(m_horz[1], "npc"))
    grob$widths[[W]]  = grob$widths[[W]]  + (grid::unit(r, "npc") - grid::unit(m_horz[2], "npc"))
    grob$heights[[1]] = grob$heights[[1]] + (grid::unit(t, "npc") - grid::unit(m_vert[1], "npc"))
    grob$heights[[W]] = grob$heights[[W]] + (grid::unit(b, "npc") - grid::unit(m_vert[2], "npc"))

    cowplot::ggdraw(grob)
}
