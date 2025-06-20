#' Add interactive overlays to a Shiny plot
#'
#' This function sets up server-side infrastructure to support draggable and
#' resizable overlays on a plot. This may be useful in applications where users
#' need to define ranges, regions of interest, or intervals for further input
#' or processing. Currently, the overlays are only designed to move along the
#' x axis of the plot.
#'
#' Call this function once from your server code to initialise a set of overlay
#' rectangles for a specific plot. It creates reactive handlers for move,
#' resize, and dropdown menu actions, and allows adding new overlays by
#' dragging an [overlayToken()] onto the plot.
#'
#' This function also defines a dynamic output UI slot with ID
#' `paste0(outputId, "_menu")`, which can be rendered using [shiny::renderUI()].
#' When a user clicks the overlay's dropdown icon, this menu becomes visible
#' and can be populated with inputs for editing overlay-specific settings, e.g.
#' labels or numeric parameters tied to that overlay.
#'
#' If you provide a coordinate snapping function (`snap` argument), it should
#' have the signature `function(ov, i)` where `ov` is the
#' [shiny::reactiveValues()] object defining the overlays and their settings,
#' and `i` is the set of indices for the rectangles to be updated. The
#' intention is that you should change the values of `ov$cx0[i]` and
#' `ov$cx1[i]`; the pixel coordinates of the overlays will then be updated
#' automatically after the snapping function returns. You should make sure that
#' all `ov$cx0[i]` and `ov$cx1[i]` are within the coordinate bounds defined by
#' the plot, i.e. constrained by `ov$bound_cx` and `ov$bound_cw`, when the
#' function returns. This means, for example, if you are "rounding down"
#' `ov$cx0[i]` to some nearest multiple of a number, you should make sure it
#' doesn't become less than `ov$bound_cx`. Finally, the snapping function will
#' get triggered when the x axis range of the plot changes, so it may be a good
#' idea to provide one if the user might place an overlay onto the plot, but
#' then change the x axis range of the plot such that the overlay is no longer
#' visible. You can detect this by verifying whether the overlay rectangles are
#' "out of bounds" at the top of your snapping function. See example below.
#'
#' @param outputId The ID of the plot output (as used in [overlayPlotOutput()]).
#' @param nrect Number of overlay rectangles to support.
#' @param width Optional default overlay width in plot coordinates. If `NULL`
#'     (default), set to 10% of the plot width.
#' @param snap Function to "snap" overlay coordinates to a grid, or `"none"`
#'     (default) for no snapping. See details for how to specify the snap
#'     function.
#' @param colours A function to assign custom colours to the overlays. Should
#'     be a function that takes a single integer (the number of overlays) and
#'     returns colours in hexadecimal notation (e.g. "#FF0000"). Do not provide
#'     opacity here as a fourth channel; use the `opacity` argument instead.
#' @param opacity Numeric value (0 to 1) indicating overlay transparency.
#' @param icon A Shiny icon to show the dropdown menu.
#' @param stagger Vertical offset between stacked overlays, as a proportion of
#'     height.
#' @param debug If `TRUE`, prints changes to input values to the console for
#'     debugging purposes.
#'
#' @return A [shiny::reactiveValues()] object with the following named fields:
#' \describe{
#'   \item{n}{Number of overlays.}
#'   \item{active}{Logical vector of length `n`; indicates which overlays are active.}
#'   \item{show}{Logical; controls whether overlays are visible.}
#'   \item{editing}{Index of the overlay currently being edited via the
#'       dropdown menu (if any; `NA` otherwise).}
#'   \item{last}{Index of the most recently added overlay.}
#'   \item{snap}{Coordinate snapping function.}
#'   \item{px, pw}{Overlay x-position and width in pixels.}
#'   \item{py, ph}{Overlay y-position and height in pixels.}
#'   \item{cx0, cx1}{Overlay x-bounds in plot coordinates.}
#'   \item{label}{Character vector of labels shown at the top of each overlay.}
#'   \item{outputId}{The output ID of the plot display area.}
#'   \item{bound_cx, bound_cw}{x-position and width of the bounding area in plot coordinates.}
#'   \item{bound_px, bound_pw}{x-position and width of the bounding area in pixels.}
#'   \item{stagger}{Amount of vertical staggering (as proportion of height).}
#'   \item{update_cx(i)}{Function to update `cx0`/`cx1` from `px`/`pw` for overlay `i`.}
#'   \item{update_px()}{Function to update `px`/`pw` from `cx0`/`cx1` for all overlays.}
#' }
#'
#' @examples
#' \dontrun{
#' server <- function(input, output) {
#'     ov <- overlayServer("my_plot", 4)
#'     ov$label <- LETTERS[1:4]
#'
#'     output$my_plot_menu <- renderUI({
#'         i <- req(ov$editing)
#'         textInput("label_input", "Overlay label", value = ov$label[i])
#'     })
#'
#'     observeEvent(input$label_input, {
#'         i <- req(ov$editing)
#'         ov$label[i] <- input$label_input
#'     })
#' }
#'
#' # Example of a valid snapping function: snap to nearest round number and
#' # make sure the overlay is at least 10 units wide.
#' mysnap <- function(ov, i) {
#'     # remove any "out of bounds" overlays
#'     oob <- seq_len(ov$n) %in% i &
#'         (ov$cx0 < ov$bound_cx | ov$cx1 > ov$bound_cx + ov$bound_cw)
#'     ov$active[oob] <- FALSE
#'
#'     # adjust position and with
#'     widths <- pmax(10, round(ov$cx1[i] - ov$cx0[i]))
#'     ov$cx0[i] <- pmax(round(ov$bound_cx),
#'         pmin(round(ov$bound_cx + ov$bound_cw) - widths, round(ov$cx0[i])))
#'     ov$cx1[i] <- pmin(round(ov$bound_cx + ov$bound_cw), ov$cx0[i] + widths)
#' }
#' }
#'
#' @seealso [overlayPlotOutput()], [overlayBounds()]
#'
#' @export
overlayServer = function(outputId, nrect, width = NULL, snap = "none",
    colours = overlayColours, opacity = 0.25, icon = shiny::icon("gear"),
    stagger = 0.045, debug = FALSE)
{
    session = shiny::getDefaultReactiveDomain()
    input = session$input
    output = session$output

    # If debug == TRUE, monitor all input changes
    if (debug) {
        old_inputs = shiny::reactiveVal(list())

        shiny::observe({
            current = shiny::reactiveValuesToList(input)
            previous = old_inputs()
            changed = names(current)[sapply(names(current),
                function(nm) !identical(current[[nm]], previous[[nm]]))]

            for (nm in changed) {
                val = current[[nm]]
                cat("Input changed:", nm, "->",
                    if (is.atomic(val)) toString(val) else paste0("<", class(val), ">"), "\n")
            }
            old_inputs(current)
        })
    }

    # ---------- GLOBAL SETUP ----------

    # Intervention tokens
    shinyjqui::jqui_draggable(ui = ovmatch("token"),
        options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0, zIndex = 9999));

    # TODO Monitor resizing of plot
    # shinyjs::runjs(paste0('observePlotResize("', outputId, '")'));

    # ---------- OVERLAY SETUP ----------

    # Run setup code for the display
    bounds = ovid("bounds", outputId)
    bounds_sel = ovsel("bounds", outputId)

    # Create overlays
    clear_ui(bounds)
    insert_ui(bounds, overlayRects(outputId, nrect, "none", colours, opacity, icon))

    # Set display area as droppable
    shinyjqui::jqui_droppable(ui = ovsel("display", outputId),
        options = list(accept = ovmatch("token"), shiny = shdrop));

    # Set overlays as horizontally draggable and resizable
    # Doesn't seem like using a selector that applies to multiple elements works here, hence the loop.
    for (i in seq_len(nrect)) {
        shinyjqui::jqui_resizable(
            shinyjqui::jqui_draggable(ui = ovsel("overlay", outputId, i),
                options = list(axis = "x", shiny = shsize, containment = bounds_sel)),
            options = list(handles = "w, e", minWidth = 1, shiny = shsize, containment = bounds_sel)
        )
    }

    # Set up overlays
    ov = shiny::reactiveValues(
        n         = nrect,          # number of overlays
        active    = rep(F, nrect),  # is the overlay active
        show      = TRUE,           # show overlays?
        editing   = NA,             # which overlay is currently being edited via dropdown
        last      = NA,             # which overlay was last to be added
        snap      = snap,           # coordinate snapping function
        px        = rep(0, nrect),  # left pixel position of overlay
        pw        = rep(0, nrect),  # pixel width of overlay
        py        = rep(0, nrect),  # bottom pixel position of overlay
        ph        = rep(0, nrect),  # pixel height of overlay
        cx0       = rep(0, nrect),  # left x coord of overlay
        cx1       = rep(1, nrect),  # right x coord of overlay
        label     = rep("", nrect), # label at top of overlay
        outputId  = outputId,       # id of display/bounds
        bound_cx  = 0,              # x-pos of bounds in coords (set by overlayBounds)
        bound_cw  = 0,              # width of bounds in coords (set by overlayBounds)
        bound_px  = 0,              # x-pos of bounds in pixels (set by overlayBounds)
        bound_pw  = 0,              # width of bounds in pixels (set by overlayBounds)
        stagger   = stagger         # how much to stagger down each overlay
    );

    # Set cx0 and cx1 of overlay i from px and pw
    ov$update_cx = function(i) {
        # Set cx0 and cx1
        ov$cx0[i] = (ov$px[i] / ov$bound_pw) * ov$bound_cw + ov$bound_cx;
        ov$cx1[i] = ((ov$px[i] + ov$pw[i]) / ov$bound_pw) * ov$bound_cw + ov$bound_cx;

        if (!identical(ov$snap, "none")) {
            ov$update_px(i)
        }
    }

    # Set px and pw of all overlays from cx0 and cx1
    ov$update_px = function(j = seq_len(ov$n)) {
        if (identical(ov$snap, "none")) {
            # Ensure times are in proper range
            ov$cx0[j] = pmax(ov$cx0[j], ov$bound_cx);
            ov$cx1[j] = pmin(ov$cx1[j], ov$bound_cx + ov$bound_cw);
        } else {
            # Snap coordinates
            ov$snap(ov, j)
        }

        # Set px and pw, as well as updating actual positions
        ov$px[j] = (ov$cx0[j] - ov$bound_cx) * ov$bound_pw / ov$bound_cw;
        ov$pw[j] = (ov$cx1[j] - ov$cx0[j]) * ov$bound_pw / ov$bound_cw;
        for (i in j) {
            setcss(ovid("overlay", outputId, i), position = "absolute",
                left = ov$px[i], width = ov$pw[i], bottom = ov$py[i], height = ov$ph[i]);
        }
    }

    # Make overlays respond to ov$active and ov$show
    shiny::observe({
        for (i in seq_len(ov$n)) {
            setcss(ovid("overlay", outputId, i),
                display = if (ov$active[i] && ov$show) "block" else "none")
        }
    })

    # Close all dropdowns and their contents
    clear_dropdowns = function() {
        setcss(ovmatch("dropdown", outputId), display = "none")
        shiny::removeUI(ovsel("menu"), immediate = TRUE)
    }

    # Observe move, resize, dropdown, remove, defocus, debug events
    shiny::observeEvent(input$overshiny_event, {
        # Extract rectangle id
        i = input$overshiny_event$id
        if (i != "null") {
            i = as.integer(stringr::str_remove(i, paste0("^", ovid("overlay", outputId), "_")))
        }

        if (input$overshiny_event$what == "move") {
            # Move: update overlay's pixel position, then coordinate info
            shiny::isolate({
                ov$px[i] = input$overshiny_event$x
                ov$update_cx(i)
            })
        } else if (input$overshiny_event$what == "resize") {
            # Resize: update overlay's pixel position & width, then coordinate info
            shiny::isolate({
                ov$px[i] = input$overshiny_event$x
                ov$pw[i] = input$overshiny_event$w
                ov$update_cx(i)
            })
        } else if (input$overshiny_event$what == "dropdown") {
            # Dropdown menu
            clear_dropdowns()
            # Insert and make visible new dropdown
            if (!isTRUE(shiny::isolate(ov$editing) == i)) {
                shiny::isolate({
                    ov$editing = i;
                    insert_ui(ovid("dropdown", outputId, i),
                        ui = htmltools::div(id = ovid("menu"),
                            shiny::uiOutput(paste(outputId, "menu", sep = "_")),
                            shiny::actionButton(inputId = "int_remove", label = "Remove",
                                icon = shiny::icon("trash"), class = "overshiny-remove",
                                `data-id` = ovid("overlay", outputId, i)
                            )
                        )
                    );
                })
                setcss(ovid("dropdown", outputId, i), display = "block")
            } else {
                ov$editing = NA;
            }
        } else if (input$overshiny_event$what == "remove") {
            shiny::isolate({
                ov$active[i] = F;
                ov$editing = NA;
            })
            clear_dropdowns()
        } else if (input$overshiny_event$what == "defocus") {
            ov$editing = NA;
            clear_dropdowns()
        } else if (input$overshiny_event$what == "plot_size") {
            # TODO This wasn't quite working anyway.
        } else if (input$overshiny_event$what == "debug") {
            cat(input$overshiny_event$text, "\n")
        }
    })

    # Add new overlay
    drop_event = ovid("display", outputId, "add")
    shiny::observeEvent(input[[drop_event]], priority = 999, {
        i = which(ov$active == F)[1];
        if (!is.na(i)) {
            clear_dropdowns()

            # Set overlay position and label and mark as active
            default_width = if (is.null(width)) 0.1 * ov$bound_pw else width
            shiny::isolate({
                ov$editing = NA;
                ov$last = i;
                ov$pw[i] = min(ov$bound_pw, default_width * ov$bound_pw / ov$bound_cw);
                ov$px[i] = max(0, min(ov$bound_pw - ov$pw[i], input[[drop_event]]$x - ov$bound_px));
                ov$update_cx(i);
                ov$active[i] = TRUE;
                ov$label[i] = input[[drop_event]]$label;

                # Position overlay
                setcss(ovid("overlay", outputId, i),
                    left = ov$px[i], width = ov$pw[i],
                    bottom = ov$py[i], height = ov$ph[i]);
            })
        }
    });

    # Change labels on overlays as needed
    shiny::observeEvent(ov$label, {
        shiny::isolate({
            for (i in seq_len(ov$n)) {
                shinyjs::html(ovid("label", outputId, i), ov$label[i]);
            }
    })})

    return (ov)
}

#' Align overlays with a ggplot2 plot
#'
#' Sets the pixel and coordinate bounds of the overlay area based on a
#' [ggplot2::ggplot()] object or base R plot. This ensures that overlays are
#' positioned correctly in both visual and coordinate space.
#'
#' Call this function within [shiny::renderPlot()], before returning the
#' ggplot object (if using ggplot2) or `NULL` (if using base R plotting).
#'
#' @param ov A [reactiveValues()] object returned by [overlayServer()].
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
#' \dontrun{
#' output$my_plot <- renderPlot({
#'     plot <- ggplot(df, aes(x, y)) + geom_line()
#'     overlayBounds(ov, plot, xlim = c(0, 100), ylim = c(0, NA))
#' })
#' }
#'
#' @seealso [overlayServer()]
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
        ov$py = rep(0, ov$n)
        ov$ph = h * (1 - 1:ov$n * ov$stagger)
        ov$update_px() # in case bounds have changed
    })

    setcss(ovid("bounds", outputId), left = paste0(l, "px"), bottom = paste0(b, "px"),
        width = paste0(w, "px"), height = paste0(h, "px"))

    return (plot)
}

