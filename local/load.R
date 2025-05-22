library(data.table)
library(ggplot2)
library(shiny)
library(overshiny)
library(shinythemes)
library(stringr)
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

intervention_adders = paste0("overshiny_token_",
    c("school_closure", "social_distancing", "elderly_shielding_part", "elderly_shielding_full", "self_isolation", "lockdown"))
intervention_strength_names = c("Schools closed (%)", "Intensity of social distancing (%)", "Effectiveness of elderly shielding (%)",
    "Adherence to self-isolation among symptomatics (%)", "Intensity of lockdown (%)");


# UI COMPONENTS

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
