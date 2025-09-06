#' Add interactive overlays to a Shiny plot
#'
#' This function sets up server-side infrastructure to support draggable and
#' resizable overlays on a plot. This may be useful in applications where users
#' need to define regions on the plot for further input or processing.
#' Currently, the overlays are only designed to move along the x axis of the
#' plot.
#'
#' Call this function once from your server code to initialise a set of overlay
#' rectangles for a specific plot. It creates reactive handlers for move,
#' resize, and dropdown menu actions, and allows adding new overlays by
#' dragging an [overlayToken()] onto the plot. The function returns a
#' [shiny::reactiveValues()] object which you should keep for further use; in
#' the examples and documentation, this object is typically called `ov`.
#'
#' @section `snap` parameter:
#'
#' If you provide your own coordinate snapping function (`snap` argument), it
#' should have the signature `function(ov, i)` where `ov` is the
#' [shiny::reactiveValues()] object defining the overlays and their settings,
#' and `i` is the set of indices for the rectangles to be updated. When the
#' position of any of the overlays is changed, the snapping function will be
#' applied. In this function, you should make sure that all `ov$cx0[i]` and
#' `ov$cx1[i]` are within the coordinate bounds defined by the plot, i.e.
#' constrained by `ov$bound_cx` and `ov$bound_cw`, when the function returns.
#' This means, for example, if you are "rounding down" `ov$cx0[i]` to some
#' nearest multiple of a number, you should make sure it doesn't become less
#' than `ov$bound_cx`. Finally, the snapping function will get triggered when
#' the x axis range of the plot changes, so it may be a good idea to provide
#' one if the user might place an overlay onto the plot, but then change the x
#' axis range of the plot such that the overlay is no longer visible. You can
#' detect this by verifying whether the overlay rectangles are "out of bounds"
#' at the top of your snapping function. See the code for [snapGrid()] for
#' ideas.
#'
#' @section Overlay dropdown menu:
#'
#' Overlays have a little icon in the top-right corner (by default, a gear).
#' When the user clicks on this icon, a dropdown menu appears that allows the
#' user to remove the overlay. You can also provide additional components for
#' this dropdown menu by using the `heading`, `select`, and `menu` parameters
#' to [overlayServer()].
#'
#' **`heading`**: This should be a function with the signature `function(ov, i)`
#' where `ov` is the [shiny::reactiveValues()] object defining the overlays and
#' their settings, and `i` is the (single) index for the current overlay. The
#' function should return a character string that will be used as the heading
#' on thedropdown menu. This can be used to e.g. report the precise start and
#' end point of the overlay, which may be useful to your users. The built-in
#' functions [rangeHeading()] and [dateHeading()] can be used for numeric
#' values and date values on the x-axis, respectively. Or you can use `NULL`
#' for no heading on the dropdown menu.
#'
#' **`select`**: This can be `TRUE` to provide a [shiny::selectInput()] widget
#' on the dropdown menu that users can use to change the type (i.e. label) of
#' the current overlay. Or you can provide a character vector to restrict the
#' widget to specific labels, or use `NULL` to omit this widget.
#'
#' **`menu`**: This can be a function with the signature `function(ov, i)`
#' where `ov` is the [shiny::reactiveValues()] object defining the overlays and
#' their settings, and `i` is the (single) index for the current overlay. It
#' should return UI component(s) (if multiple components, wrapped in a `list`
#' or `tagList`) that will be inserted into the dropdown menu. If you give the
#' input widgets special IDs, the user can use those input widgets to directly
#' modify certain properties of the overlays:
#'
#' \tabular{ll}{
#' **`inputId`** \tab **Modifies** \cr
#' \code{*_label} \tab The label of the overlay currently being edited. \cr
#' \code{*_cx0} \tab Starting x-coordinate of overlay. \cr
#' \code{*_cx1} \tab Ending x-coordinate of overlay. \cr
#' \code{*_cx} \tab X-position of overlay; this is like `cx0`, but also updates `cx1` to keep the same width. \cr
#' \code{*_cw} \tab Width of overlay; this adjusts `cx1` so that the overlay has the given width. \cr
#' \code{*_XYZ} \tab The corresponding entry "XYZ" in `data` for the overlay being edited. \cr
#' \tab Note: above, `*` stands for the `outputId` argument to `overlayServer()`.
#' }
#'
#' See examples for an illustration of this.
#'
#' @param outputId The ID of the plot output (as used in [overlayPlotOutput()]).
#' @param nrect Number of overlay rectangles to support.
#' @param width Optional default overlay width in plot coordinates. If `NULL`
#'     (default), set to 10% of the plot width.
#' @param data Named list of custom overlay-specific properties to be edited in
#'     the overlay dropdown menu.
#' @param snap Function to "snap" overlay coordinates to a grid, or `NULL`
#'     (default) for no snapping. See details for how to specify the snap
#'     function; you can also use the built-in [snapGrid()].
#' @param heading Function to provide a heading for the overlay dropdown menus,
#'     or `NULL` (default) for no heading. See details for how to specify the
#'     heading function; you can also use the built-in [rangeHeading()] or
#'     [dateHeading()].
#' @param select If you want to allow users to change the type (i.e. label) of
#'     the overlay from the overlay dropdown menu, set this to `TRUE` to
#'     provide a select input with all labels or a character vector with
#'     permissible choices. `NULL` (default) to omit this feature.
#' @param menu Function to provide additional UI elements on the overlay
#'     dropdown menu. See details for how to specify the menu function.
#' @param colours A function to assign custom colours to the overlays. Should
#'     be a function that takes a single integer (the number of overlays) and
#'     returns colours in hexadecimal notation (e.g. "#FF0000"). Do not provide
#'     opacity here as a fourth channel; use the `opacity` argument instead.
#' @param opacity Numeric value (0 to 1) indicating overlay transparency.
#' @param icon A Shiny icon to show the dropdown menu.
#' @param stagger Vertical offset between stacked overlays, as a proportion of
#'     height.
#' @param style Named list of character vectors with additional CSS styling
#'     attributes for the overlays. If an element is named "background-color"
#'     then this will override the `colours` and `opacity` arguments. Vectors
#'     are recycled to length `nrect`.
#' @param debug If `TRUE`, prints changes to input values to the console for
#'     debugging purposes.
#'
#' @return A [shiny::reactiveValues()] object with the following named fields:
#' \tabular{ll}{
#' \code{n} \tab Number of overlays (read-only). \cr
#' \code{show} \tab `TRUE`/`FALSE`; controls whether overlays are visible. \cr
#' \code{active} \tab Logical vector of length `n`; indicates which overlays are active. \cr
#' \code{label} \tab Character vector of labels shown at the top of each overlay. \cr
#' \code{data} \tab Custom data for each overlay, to be edited via the dropdown menu. \cr
#' \code{editing} \tab Index of the overlay currently being edited via the dropdown menu; `NA` if none (read-only). \cr
#' \code{last} \tab Index of the most recently added overlay (read-only). \cr
#' \code{snap} \tab Coordinate snapping function. \cr
#' \code{heading} \tab Heading function for the dropdown menu. \cr
#' \code{select} \tab Overlay label select options for the dropdown menu. \cr
#' \code{menu} \tab Function to provide additional UI elements for the dropdown menu. \cr
#' \code{px,pw} \tab Numeric vector; overlay x-position and width in pixels (see note). \cr
#' \code{py,ph} \tab Numeric vector; overlay y-position and height in pixels (read-only). \cr
#' \code{cx0,cx1} \tab Numeric vector; overlay x-bounds in plot coordinates (see note). \cr
#' \code{outputId} \tab The output ID of the plot display area (read-only). \cr
#' \code{bound_cx, bound_cw} \tab x-position and width of the bounding area in plot coordinates (read-only). \cr
#' \code{bound_px, bound_pw} \tab x-position and width of the bounding area in pixels (read-only). \cr
#' \code{bound_py, bound_ph} \tab y-position and height of the bounding area in pixels (read-only). \cr
#' \code{stagger} \tab Amount of vertical staggering, as proportion of height. \cr
#' \code{style} \tab Named list of character vectors; additional styling for rectangular overlays. \cr
#' \code{update_cx(i)} \tab Function to update `cx0`/`cx1` from `px`/`pw` for overlays `i` (see note). \cr
#' \code{update_px(i)} \tab Function to update `px`/`pw` from `cx0`/`cx1` for overlays `i` (see note).
#' }
#'
#' Note: Fields marked "read-only" above should not be changed. Other fields can
#' be changed in your reactive code and this will modify the overlays and their
#' properties. The fields `px` and `pw` which specify the pixel coordinates of
#' each overlay can be modified, but any modifications should be placed in a
#' [shiny::isolate()] call, with a call to `ov$update_cx(i)` at the end to
#' update `cx0` and `cx1` and apply snapping. Similarly, the fields
#' `cx0` and `cx1` which specify the plot coordinates of each overlay can be
#' modified, but modifications should be placed in a [shiny::isolate()] call
#' with a call to `ov$update_px(i)` at the end to update `px` and `pw`
#' and apply snapping. The `i` parameter to these functions can be left out
#' to apply changes to all overlays, or you can pass in the indices of just
#' the overlay(s) to be updated.
#'
#' @examples
#' ui <- shiny::fluidPage(
#'     overlayPlotOutput("my_plot", 640, 480),
#'     overlayToken("add", "Raise")
#'     # further UI elements here . . .
#' )
#'
#' server <- function(input, output) {
#'     menu <- function(ov, i) {
#'         sliderInput("my_plot_amount", "Raise amount",
#'                value = ov$data$amount[i], min = 0, max = 1)
#'     }
#'
#'     ov <- overlayServer("my_plot", 4, 1,
#'         data = list(amount = 0.2),
#'         snap = snapGrid(step = 0.1),
#'         heading = rangeHeading(digits = 3),
#'         menu = menu)
#'
#'     output$my_plot <- shiny::renderPlot({
#'         df <- data.frame(x = seq(0, 2 * pi, length.out = 200))
#'         df$y <- (1 + sin(df$x)) / 2
#'         for (i in which(ov$active)) {
#'             xi <- (df$x >= ov$cx0[i] & df$x <= ov$cx1[i])
#'             df$y[xi] <- df$y[xi] + ov$data$amount[i]
#'         }
#'         plot(df, type = "l")
#'         overlayBounds(ov, "base")
#'     })
#'     # further server code here . . .
#' }
#'
#' if (interactive()) {
#'     shiny::shinyApp(ui, server)
#' }
#'
#' @seealso [overlayPlotOutput()], [overlayBounds()]
#'
#' @export
overlayServer = function(outputId, nrect, width = NULL, data = NULL,
    snap = NULL, heading = NULL, select = NULL, menu = NULL,
    colours = overlayColours, opacity = 0.25, icon = shiny::icon("gear"),
    stagger = 0.045, style = list(), debug = FALSE)
{
    # ---------- VALIDATE PARAMETERS ----------

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

    # Validate data parameter
    if (length(data) > 0 &&
            (!is.list(data) || is.null(names(data)) ||
            any(is.na(names(data))) || any(names(data) == ""))) {
        stop("Data must be a named list or NULL.")
    }
    data = lapply(data, function(x) {
        if (length(x) == 1) {
            rep(x, nrect)
        } else if (length(x) == nrect) {
            x
        } else {
            stop("All elements of data must be either length 1 or length nrect (", nrect, ").")
        }
    })

    # ---------- ENABLE TOKENS ----------

    # Intervention tokens
    shinyjqui::jqui_draggable(ui = ovmatch("token"),
        options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0, zIndex = 9999));

    # Refresh token list
    refresh_tokens = function() {
        shinyjs::runjs(
'var token_labels = $(".overshiny-token").map(function() { return $(this).attr("data-label"); }).get();
Shiny.setInputValue("overshiny_tokens", token_labels);'
        )
    }
    refresh_tokens()

    # TODO Monitor resizing of plot
    # shinyjs::runjs(paste0('observePlotResize("', outputId, '")'));

    # ---------- ENABLE OVERLAYS ----------

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

    # ---------- OVERLAY OBJECT ----------

    # Set up overlays
    ov = shiny::reactiveValues(
        n         = nrect,          # number of overlays
        show      = TRUE,           # show overlays?
        active    = rep(F, nrect),  # is the overlay active
        label     = rep("", nrect), # label at top of overlay
        data      = data,           # user settings
        editing   = NA,             # which overlay is currently being edited via dropdown
        last      = NA,             # which overlay was last to be added
        snap      = snap,           # coordinate snapping function
        heading   = heading,        # overlay menu heading function
        select    = select,         # overlay menu select for label
        menu      = menu,           # overlay menu UI function
        px        = rep(0, nrect),  # left pixel position of overlay
        pw        = rep(0, nrect),  # pixel width of overlay
        py        = rep(0, nrect),  # bottom pixel position of overlay
        ph        = rep(0, nrect),  # pixel height of overlay
        cx0       = rep(0, nrect),  # left x coord of overlay
        cx1       = rep(1, nrect),  # right x coord of overlay
        outputId  = outputId,       # id of display/bounds
        bound_cx  = 0,              # x-pos of bounds in coords (set by overlayBounds)
        bound_cw  = 0,              # width of bounds in coords (set by overlayBounds)
        bound_px  = 0,              # x-pos of bounds in pixels (set by overlayBounds)
        bound_pw  = 0,              # width of bounds in pixels (set by overlayBounds)
        bound_py  = 0,              # y-pos of bounds in pixels (set by overlayBounds)
        bound_ph  = 0,              # height of bounds in pixels (set by overlayBounds)
        stagger   = stagger,        # how much to stagger down each overlay
        style     = style           # additional styling options
    );

    # Set cx0 and cx1 of overlay i from px and pw
    ov$update_cx = function(i) {
        # Set cx0 and cx1
        ov$cx0[i] = (ov$px[i] / ov$bound_pw) * ov$bound_cw + ov$bound_cx;
        ov$cx1[i] = ((ov$px[i] + ov$pw[i]) / ov$bound_pw) * ov$bound_cw + ov$bound_cx;

        if (!is.null(ov$snap)) {
            ov$update_px(i)
        }
    }

    # Set px and pw of all overlays from cx0 and cx1
    ov$update_px = function(j = seq_len(ov$n)) {
        if (is.null(ov$snap)) {
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

    # ---------- OVERLAY REACTIVITY ----------

    # Make overlays respond to ov$active and ov$show
    shiny::observe({
        for (i in seq_len(ov$n)) {
            setcss(ovid("overlay", outputId, i),
                display = if (ov$active[i] && ov$show) "block" else "none")
        }
    })

    # Make overlays respond to ov$label
    shiny::observe({
        for (i in seq_len(ov$n)) {
            shinyjs::html(ovid("label", outputId, i), ov$label[i]);
        }
    })

    # Make overlays respond to ov$style
    shiny::observe({
        for (i in seq_len(ov$n)) {
            attributes = lapply(ov$style, function(x) x[[(i - 1) %% length(x) + 1]])
            do.call(setcss, c(list(ovid("overlay", outputId, i)), attributes))
        }
    })

    # Make overlays respond to ov$stagger
    shiny::observeEvent(ov$stagger, {
        ov$ph = ov$bound_ph * (1 - 1:ov$n * ov$stagger)
        ov$update_px()
    })

    # ---------- OVERLAY DROPDOWN ----------

    # Helper to insert UI for overlay dropdown menu
    insert_dropdown = function(i) {
        # Dropdown menu components
        heading = if (!is.null(ov$heading)) {
            shiny::textOutput(ovid("heading"))
            output[[ovid("heading")]] = shiny::renderText({ ov$heading(ov, i) });
        }
        select = if (!is.null(ov$select)) {
            if (isTRUE(ov$select)) {
                choices = input$overshiny_tokens
            } else if (is.character(ov$select)) {
                choices = ov$select
            } else {
                stop("select must be TRUE or a character vector of overlayToken labels.")
            }
            shiny::selectInput(ovid("select"),
                label = NULL,
                choices = choices,
                selected = ov$label[i])
        }
        extra = if (!is.null(ov$menu)) {
            shiny::isolate(ov$menu(ov, i))
        }
        trash = shiny::actionLink(inputId = ovid("remove"), label = "Remove",
            icon = shiny::icon("trash"), class = "overshiny-remove",
            `data-id` = ovid("overlay", outputId, i))

        # Insert dropdown menu
        insert_ui(ovid("dropdown", outputId, i),
            htmltools::div(id = ovid("menu"),
                heading, select, extra, trash))
    }

    # Helper to close all dropdowns and remove their contents
    clear_dropdowns = function() {
        setcss(ovmatch("dropdown", outputId), display = "none")
        shiny::removeUI(ovsel("menu"), immediate = TRUE)
    }

    # Observe built-in dropdown label select
    shiny::observeEvent(input[[ovid("select")]], {
        i = shiny::req(ov$editing)
        ov$label[i] = input[[ovid("select")]]
        shiny::removeUI(ovsel("menu"), immediate = TRUE)
        insert_dropdown(i)
    })

    # Observe "extra" menu-related input changes
    data_head = paste0("^", outputId, "_")
    shiny::observe({
        names = names(input)
        names = names[grepl(data_head, names)]
        for (nm in names) {
            input[[nm]] # take dependency
        }

        i = shiny::req(shiny::isolate(ov$editing))

        names = names(input)
        names = names[grepl(data_head, names)]
        for (nm in names) {
            inm = stringr::str_remove(nm, data_head)
            if (inm == "label") {
                ov$label[[i]] = input[[nm]]
            } else if (inm == "cx0") {
                shiny::isolate({
                    ov$cx0[[i]] = input[[nm]]
                    ov$update_px(i)
                })
            } else if (inm == "cx1") {
                shiny::isolate({
                    ov$cx1[[i]] = input[[nm]]
                    ov$update_px(i)
                })
            } else if (inm == "cx") {
                shiny::isolate({
                    w = ov$cx1[[i]] - ov$cx0[[i]]
                    ov$cx0[[i]] = input[[nm]]
                    ov$cx1[[i]] = input[[nm]] + w
                    ov$update_px(i)
                })
            } else if (inm == "cw") {
                shiny::isolate({
                    ov$cx1[[i]] = ov$cx0[[i]] + input[[nm]]
                    ov$update_px(i)
                })
            } else if (!is.null(ov$data[[inm]])) {
                ov$data[[inm]][[i]] = input[[nm]]
            }
        }
    })

    # ---------- OVERLAY EVENTS ----------

    # Observe move, resize, dropdown, remove, defocus, plot_size, debug events
    shiny::observeEvent(input$overshiny_event, {
        # Extract rectangle id
        i = input$overshiny_event$id
        if (i != "null") {
            i = as.integer(stringr::str_remove(i, paste0("^", ovid("overlay", outputId), "_")))
        }

        if (input$overshiny_event$what == "move") {
            # Move: update overlay's pixel position, then coordinate info
            ov$px[i] = input$overshiny_event$x
            ov$update_cx(i)
        } else if (input$overshiny_event$what == "resize") {
            # Resize: update overlay's pixel position & width, then coordinate info
            ov$px[i] = input$overshiny_event$x
            ov$pw[i] = input$overshiny_event$w
            ov$update_cx(i)
        } else if (input$overshiny_event$what == "dropdown") {
            # Dropdown menu
            clear_dropdowns()
            # Insert and make visible new dropdown
            if (!isTRUE(ov$editing == i)) {
                ov$editing = i
                insert_dropdown(i)
                setcss(ovid("dropdown", outputId, i), display = "block")
            } else {
                # Close menu
                ov$editing = NA
            }
        } else if (input$overshiny_event$what == "remove") {
            ov$active[i] = F;
            ov$editing = NA;
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
            ov$editing = NA;
            ov$last = i;
            ov$pw[i] = min(ov$bound_pw, default_width * ov$bound_pw / ov$bound_cw);
            ov$px[i] = max(0, min(ov$bound_pw - ov$pw[i], input[[drop_event]]$x - ov$bound_px));
            ov$update_cx(i);
            ov$active[i] = TRUE;
            ov$label[i] = input[[drop_event]]$label;

            # Reset data
            for (nm in names(ov$data)) {
                ov$data[[nm]][[i]] = data[[nm]][[i]]
            }

            # Position overlay
            setcss(ovid("overlay", outputId, i),
                left = ov$px[i], width = ov$pw[i],
                bottom = ov$py[i], height = ov$ph[i]);
        }
    });

    return (ov)
}
