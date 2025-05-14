library(data.table)
library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(shinyWidgets)
library(rhandsontable)
library(stringr)
library(leaflet)
library(yaml)
library(qs)
library(lubridate)

source("./vvplot.R")

iv_def = list(
    list(name = "School closures",
        strength_default = 100,
        strength_name = "Schools closed (%)",
        contact = function(x) c(1, 1, 1-x, 1,  1, 1, 1-x, 1),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Social distancing",
        strength_default = 50,
        strength_name = "Intensity of social distancing (%)",
        contact = function(x) c(1, 1-x, 1, 1-x,  1, 1-x, 1, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (stay-at-home)",
        strength_default = 75,
        strength_name = "Proportion of elderly individuals shielded, non-home contacts (%)",
        contact = function(x) c(1, 1, 1, 1,  1, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (full shielding)",
        strength_default = 75,
        strength_name = "Proportion of elderly individuals shielded, all contacts (%)",
        contact = function(x) c(1, 1, 1, 1,  1-x, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Self-isolation",
        strength_default = 50,
        strength_name = "Adherence to self-isolation among symptomatics (%)",
        contact = function(x) c(1, 1, 1, 1,  1, 1, 1, 1),
        fIs = function(x) rep(1 - 0.7*x, 16)
    ),
    list(name = "Lockdown",
        strength_default = 90,
        strength_name = "Intensity of lockdown (%)",
        contact = function(x) c(1, 1-x, 1-x, 1-x,  1, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    )
)

intervention_types = 1:length(iv_def);
names(intervention_types) = sapply(iv_def, function(x) x$name);

intervention_adders = paste0("overshiny-token-",
    c("school_closure", "social_distancing", "elderly_shielding_part", "elderly_shielding_full", "self_isolation", "lockdown"))
intervention_strength_names = c("Schools closed (%)", "Intensity of social distancing (%)", "Effectiveness of elderly shielding (%)",
    "Adherence to self-isolation among symptomatics (%)", "Intensity of lockdown (%)");
intervention_strength_defaults = c(100, 50, 75, 50, 90);

# Helper to change CSS
setcss = function(id, ...)
{
    arguments = list(...);
    n = names(arguments);
    v = sapply(arguments, function(x) { if (is.numeric(x)) paste0(x, "px") else x });
    nv = paste0('"', n, '" : "', v, '"', collapse = ", ");
    command = paste0('$("#', id, '").css({ ', nv, ' });');
    runjs(command);
}


# UI COMPONENTS

nbsp = function(text)
{
    str_replace_all(text, " ", "&nbsp;")
}

iconTab = function(id, title, icon)
{
    HTML(paste0(
        "<span id=\"", id, "\"><i class=\"fa fa-", icon, " fa-3x fa-fw\"></i>",
        "<br />", title, "</span>"))
}

# vaccineRect = function(n, display = "block")
# {
#     colours   = c("#dddddd40", "#dddddd40", "#dddddd40");
#     outlines  = c("#44aa7780", "#4477aa80", "#4444aa80");
#     tags$div(
#         dropdownButton(inputId = paste0("vax_menu_", n),
#             tags$label(id = paste0("vax_timespan_", n), "Date range"),
#             customAgeSlider(paste0("vax_ages_", n), "Ages to vaccinate", value1 = 0, value2 = 80, step = 5),
#             numericInput(paste0("vax_n_", n), "Vaccines administered", value = 1000000, min = 0, step = 5000),
#             radioButtons(paste0("vax_rate_", n), NULL, c("total", "per day"), selected = "total", inline = TRUE),
#             bsButton(inputId = paste0("vax_remove_", n), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
#             circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "int"
#         ),
#         div(id = paste0("vv_title_", n), style = "position: absolute; left: 22px; top: 4px", class = "overshiny-noselect"),
#         id = paste0("vv_", n),
#         style = paste0("font-size: 8pt; background-color: ", colours[n], "; border-color: ", outlines[n], "; border-style: dashed; border-width: 1px 1px 0px 1px; height: ", 430, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
#         class = "overshiny-noselect overshiny-rect"
#     )
# }

# for communicating position and size of intervention rectangles to server code
# elements mv and sz get transformed into input$DIVNAME_mv, input$DIVNAME_sz
shsize = list(
    mv = list(
        `dragstop` = JS('function(event, ui) { return ui.position.left; }')
    ),
    sz = list(
        `resizestop` = JS('function(event, ui) { return { x : ui.position.left, w : ui.size.width }; }')
    )
)

shdrop = list(
    notify = list(
        `drop` = JS("function(event, ui) { return { x : ui.position.left, y : ui.position.top, id : $(ui.draggable).attr('id') }; }")
    )
)


# new stuff
useOverlay = function()
{
    tags$head(
        # For draggable tokens
        tags$style(".overshiny-token { background-color: #f8f5f0; padding: 4px; cursor: copy }"),
        tags$style(".overshiny-token:hover { background-color: #e8e5f0 }"),

        # For overlay rectangle
        tags$style(".overshiny-rect:hover { cursor: grab }"),
        tags$style(".overshiny-rect:active { cursor: grabbing }"),

        # For overlay settings dropdown
        # Class 'btn-overshiny-dropdown' has the 'btn-' prefix added by
        # shinyWidgets::dropdownButton automatically. Class 'dropdown-menu' is
        # the class used by shinyWidgets::dropdownButton for the dropdown menu.
        tags$style(".btn-overshiny-dropdown { background: none; margin: 2px }"),
        tags$style(".dropdown-menu { opacity: 0.75 }"),

        # To not allow text highlighting or long-press callouts on iOS
        tags$style(".overshiny-noselect { -webkit-touch-callout: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none }")
    )
}

overlayColours = function(n, alpha = 1)
{
    colours = c("#162d60", "#3771c8", "#37c8ab", "#ffd42a", "#ff7f2a", "#ff0066", "#bc5fd3", "#9d93ac");
    colours = paste0(colours, sprintf("%02x", round(255 * pmax(0, pmin(1, alpha)))))
    picker = list(2, c(2, 6), c(2, 4, 6), c(2:4, 6), 2:6, 2:7, 1:7, 1:8)

    n = round(n)
    if (n <= 0) return (character(0))
    if (n <= 8) return (colours[picker[[n]]])

    colorRampPalette(colours)(n)
}

#
overlayToken = function(inputId, label)
{
    tags$span(HTML(label),
        id = paste0("overshiny-token-", inputId),  # TODO change to paste0(inputId, "-drop")
        class = "overshiny-token",
        style = "position: relative")
}

#
overlayImage = function(outputId, width, height)
{
    tags$div(id = "display",
        style = "margin: 0px !important; border: none !important; padding: 0px !important; float: none !important; position: relative !important",
        imageOutput(outputId, width, height),
        tags$div(id = "iv_bounds",
            style = paste0("width: ", width, "px; height: ", height, "px; position: absolute; left: 0px; bottom: 0px; margin: 0px; padding: 0px"),
            overlayRects(8, "none")
        )
    )
}

# TODO common definition with above...
overlayPlot = function(outputId, width, height)
{
    tags$div(id = "display",
        style = "margin: 0px !important; border: none !important; padding: 0px !important; float: none !important; position: relative !important",
        plotOutput(outputId, width, height),
        tags$div(id = "iv_bounds",
            style = paste0("width: ", width, "px; height: ", height, "px; position: absolute; left: 0px; bottom: 0px; margin: 0px; padding: 0px"),
            overlayRects(8, "none")
        )
    )
}

overlayRects = function(n, display = "block")
{
    colours = overlayColours(n, 0.25)
    rects = list()
    for (i in seq_len(n)) {
        rects[[i]] = tags$div(
            dropdownButton(inputId = paste0("int_menu_", i),
                tags$label(id = paste0("int_timespan_", i), "Date range"),
                selectInput(inputId = paste0("int_type_", i), label = NULL, choices = intervention_types, selected = 2),
                sliderInput(inputId = paste0("int_strength_", i), label = 'Strength', value = 50, min = 1, max = 100),
                bsButton(inputId = paste0("int_remove_", i), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
                circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "overshiny-dropdown"
            ),
            div(id = paste0("iv_title_", i), style = "position: absolute; left: 22px; top: 4px", class = "overshiny-noselect"),
            id = paste0("iv_", i),
            style = paste0("font-size: 8pt; background-color: ", colours[i], "; height: ", 430 - i * 20, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
            class = "overshiny-noselect overshiny-rect"
        )
    }

    do.call(tagList, rects)
}





# library(grid)
# library(tweak)
# library(ggplot2)

panel_rects_ggplot = function(plot)
{
    devsize = par("fin") * 25.4 # Device size in mm
    built = ggplot2::ggplot_build(plot)
    gtable = ggplot2::ggplot_gtable(built)
    to_mm = function(x) grid::convertUnit(x, "mm", valueOnly = TRUE)

    # With a set of lengths (widths/heights) from a gtable and a total length
    # (plot width/height in mm), replace null lengths with mm lengths
    expand_length = function(lengths, total_mm)
    {
        leeway_mm = total_mm - sum(to_mm(lengths))
        null_length = sum(sapply(unclass(lengths),
            function(x) if (x[[3]] == 5) x[[1]] else 0))

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

panel_rects_base = function()
{
    # Get dimensions of plot
    mfrow = par("mfrow")
    if (!identical(mfrow, c(1L, 1L))) {
        stop("This only works with a single base plot, not multiple plots using mfrow/mfcol.")
    }

    # Add the currently active panel
    plt = par("plt")
    usr = par("usr")

    data = data.frame(panel_id = 1L, row = 1L, col = 1L, label = NA,
        x = plt[1], y = plt[3], w = plt[2] - plt[1], h = plt[4] - plt[3],
        xmin = usr[1], xmax = usr[2], ymin = usr[3], ymax = usr[4])

    return (data)
}





overlay_plot = function(input, plot, row = 1L, col = 1L)
{
    if (is.ggplot(plot)) {
        rect = panel_rects_ggplot(plot)
        rect = rect[rect$row == row & rect$col == col]
        if (nrow(rect) != 1) {
            stop("Invalid row, col")
        }

        # Get width and height of target plot
        runjs('Shiny.setInputValue("overshiny_return", [$("#cases_plot").width(), $("#cases_plot").height()]);')
        img_width = input$overshiny_return[1];
        img_height = input$overshiny_return[2];
        l = rect$x * img_width
        b = rect$y * img_height
        w = rect$w * img_width
        h = rect$h * img_height

        setcss("iv_bounds", left = paste0(l, "px"), bottom = paste0(b, "px"),
            width = paste0(w, "px"), height = paste0(h, "px"))
    } else {
        stop("Unrecognised plot type")
    }
}



DISP_W = 810
