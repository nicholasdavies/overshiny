#' @export
overlayServer = function(outputId, nrect, width = NULL,
    colours = overlayColours, opacity = 0.25, icon = shiny::icon("gear"),
    stagger = 0.045, debug = FALSE)
{
    session = shiny::getDefaultReactiveDomain()
    input = session$input
    output = session$output

    # If debug == TRUE, monitor all input changes
    if (debug) {
        old_inputs = shiny::reactiveVal(list())

        observe({
            current = shiny::reactiveValuesToList(input)
            previous = old_inputs()
            changed = names(current)[sapply(names(current),
                function(nm) !identical(current[[nm]], previous[[nm]]))]

            for (nm in changed) {
                val = current[[nm]]
                cat("Input changed:", nm, "→",
                    if (is.atomic(val)) toString(val) else paste0("<", class(val), ">"), "\n")
            }
            old_inputs(current)
        })
    }

    # ---------- GLOBAL SETUP ----------

    # Intervention tokens
    shinyjqui::jqui_draggable(ui = ovmatch("token"),
        options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));

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
        editing   = NA,             # which overlay is currently being edited via dropdown
        last      = NA,             # which overlay was last to be added
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
        stagger   = stagger,        # how much to stagger down each overlay
        opt       = list()          # container for user-specified settings
    );

    # Set cx0 and cx1 of overlay i from px and pw
    ov$update_cx = function(i) {
        # Set cx0 and cx1
        ov$cx0[i] = (ov$px[i] / ov$bound_pw) * ov$bound_cw + ov$bound_cx;
        ov$cx1[i] = ((ov$px[i] + ov$pw[i]) / ov$bound_pw) * ov$bound_cw + ov$bound_cx;
    }

    # Set px and pw of all overlays from cx0 and cx1
    ov$update_px = function() {
        # Ensure times are in proper range
        ov$cx0 = pmax(ov$cx0, ov$bound_cx);
        ov$cx1 = pmin(ov$cx1, ov$bound_cx + ov$bound_cw);

        # Set px and pw, as well as updating actual positions
        ov$px = (ov$cx0 - ov$bound_cx) * ov$bound_pw / ov$bound_cw;
        ov$pw = (ov$cx1 - ov$cx0) * ov$bound_pw / ov$bound_cw;
        for (i in seq_len(ov$n)) {
            setcss(ovid("overlay", outputId, i), position = "absolute",
                left = ov$px[i], width = ov$pw[i], bottom = ov$py[i], height = ov$ph[i]);
        }
    }

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
                isolate({
                    ov$editing = i;
                    insert_ui(ovid("dropdown", outputId, i),
                        ui = htmltools::div(id = ovid("menu"),
                            uiOutput(paste(outputId, "menu", sep = "_")),
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
            setcss(ovid("overlay", outputId, i), display = "none");
        } else if (input$overshiny_event$what == "defocus") {
            ov$editing = NA;
            clear_dropdowns()
        } else if (input$overshiny_event$what == "debug") {
            cat(input$overshiny_event$text, "\n")
        }
    })


    # ---------- ADD OVERLAY ----------

    # Add new overlay
    drop_event = ovid("display", outputId, "add")
    shiny::observeEvent(input[[drop_event]], priority = 999, {
        i = which(ov$active == F)[1];
        if (!is.na(i)) {
            clear_dropdowns()

            # Set overlay position and label and mark as active
            # TODO there could be some more specific positioning than input[[drop_event]]$x - ov$bound_px.
            # This seems to work well enough, but I haven't quite gotten to the bottom of what input[[drop_event]]$x
            # really means; it doesn't seem to be strictly relative to the "display" element (but who knows).
            default_width = if (is.null(width)) 0.1 * ov$bound_pw else width
            shiny::isolate({
                ov$editing = NA;
                ov$last = i;
                ov$pw[i] = min(ov$bound_pw, default_width * ov$bound_pw / ov$bound_cw);
                ov$px[i] = max(0, min(ov$bound_pw - ov$pw[i], input[[drop_event]]$x - ov$bound_px));
                ov$update_cx(i);
                ov$active[i] = TRUE;
                ov$label[i] = input[[drop_event]]$label;
            })

            # Position and apparate overlay
            setcss(ovid("overlay", outputId, i), display = "block",
                left = ov$px[i], width = ov$pw[i],
                bottom = ov$py[i], height = ov$ph[i]);
        }
    });

    # Change labels on overlays as needed
    shiny::observeEvent(ov$label, {
        shiny::isolate({
            for (i in seq_len(ov$n)) {
                shinyjs::html(ovid("label", ov$outputId, i), ov$label[i]);
            }
    })})

    return (ov)
}

#' @export
overlayBounds = function(ov, plot, xlim = c(NA, NA), ylim = c(NA, NA), row = 1L, col = 1L)
{
    input = shiny::getDefaultReactiveDomain()$input

    if (ggplot2::is_ggplot(plot)) {
        rect = panel_rects_ggplot(plot)
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

        # Get width and height of target plot
        outputId = shiny::isolate(ov$outputId)
        shinyjs::runjs(paste0('Shiny.setInputValue("overshiny_return", [$("#', outputId, '").width(), $("#', outputId, '").height()]);'))
        img_width = input$overshiny_return[1];
        img_height = input$overshiny_return[2];
        l = bx * img_width
        w = bw * img_width
        b = by * img_height
        h = bh * img_height

        shiny::isolate({
            ov$bound_cx = xlim[1]
            ov$bound_cw = xlim[2] - xlim[1]
            ov$bound_px = l
            ov$bound_pw = w
            ov$py = rep(0, ov$n)
            ov$ph = h * (1 - 1:ov$n * ov$stagger)
            ov$update_px() # in case bounds have changed
        })

        setcss(ovid("bounds", outputId), left = paste0(l, "px"), bottom = paste0(b, "px"),
            width = paste0(w, "px"), height = paste0(h, "px"))
    } else {
        stop("Unrecognised plot type")
    }

    return (ov)
}

