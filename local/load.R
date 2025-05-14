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



DISP_W = 810
