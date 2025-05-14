# covidm shiny app: interface

source("./load.R")

ui = fluidPage(theme = shinytheme("sandstone"),
    useShinyjs(),
    useOverlay(),

    tags$head(
        # Newer version of fontawesome
        tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),

        # Rounded and centered tabs
        tags$style(HTML(".nav-tabs > li { text-align: center; margin-right: 0 }")),
        tags$style(HTML(".nav-tabs > li > a { border-radius: 10px 10px 0 0 }")),

        # Custom slider
        tags$style(".irs-bar, .irs-bar-edge { background-color: #98978b }"),
        tags$style(".irs-slider { border: 3px solid #98978b; border-radius: 10px }"),
        tags$style(".irs-single { background-color: #f0f0f0; color: #000000 }"),
        tags$style(".irs-min, .irs-max { background-color: #f8f5f0; color: #98978b }"),

        # For help button
        tags$style(".btn-info.active { background-color: #e48721 }"),
        tags$style(".btn-info.active:hover { background-color: #e48721 }"),
        tags$style(".btn-info.active:focus { background-color: #e48721 }"),
        tags$style(".btn-info.active:active { background-color: #e48721 }"),
        tags$style(".btn-info { background-color: #98978b }"),
        tags$style(".btn-info:hover { background-color: #98978b }"),
        tags$style(".btn-info:focus { background-color: #98978b }"),
        tags$style(".btn-info:active { background-color: #98978b }"),
    ),
    chooseSliderSkin("Square"),

    h3(id = "title", style = "text-align: center", "CMMID COVID-19 transmission app"),
    h4(style = "text-align: center", "Beta testing version: This app is still under development"),
    bsPopover("help_tooltips", "Show help", "Enables in-app help, which appears in bubbles like this one.", placement = "bottom", trigger = "hover", options = list(container = "body")),

    # ---------- DISPLAY PANEL ----------

    # these points now apply to overlayImage / overlayPlot's internal <div>
    # margin can be changed with no ill effect.
    # max-width and min-width can be changed, but they affect the display
    # float: none is critical because other float options remove the element from normal document flow.
    # padding: 10px can be adjusted, but this needs to be accounted for within the iv_bounds element.
    # position: relative is critical.
    div(style = "max-width: 1000px; min-width: 1000px; margin: 0 auto; float: none; padding: 10px; border-radius: 20px 10px 10px 10px; border: 1px solid #dddddd; position:relative",
        # Help button
        div(id = "help_loc", style = "position: absolute; right: 10px; top: 10px",
            bsButton("help_tooltips", label = HTML("Show help"), icon = icon("question-circle", "fa"), style = "info", size = "small", type = "toggle", value = FALSE)
        ),

        # Compare button
        div(id = "compare_loc", style = "position: absolute; right: 112px; top: 10px",
            bsButton("compare", label = HTML("Compare"), icon = icon("exchange-alt", "fa"), style = "info", size = "small", type = "toggle", value = TRUE)
        ),

        # Display tabs
        tabsetPanel(id = "display_tabs",
            tabPanel(value = "cases", title = iconTab("tab_cases", "Cases", "head-side-cough"),
                overlayImage("cases_plot", width = 960, height = 480), style = "padding:10px")
        )
    ),

    # ---------- CONTROL PANEL ----------

    div(id = "control", style = "max-width: 980px; min-width: 980px; margin: 0 auto !important; float: none !important; padding: 10px; border-radius: 0px 0px 10px 10px; border-width: 0px 1px 1px 1px; border-color: #dddddd; border-style: solid; position:relative",
        tabsetPanel(id = "control_tabs",
            # Intervention controls
            tabPanel(value = "interventions", title = iconTab("tab_interventions", "Interventions", "star-of-life"),
                tags$br(),
                fluidRow(
                    column(12,
                        tags$h5(HTML("Drag the labels below onto the plot above to add new interventions.<br/>They can be moved and resized directly on the plot.")),
                        overlayToken("School&nbsp;closure", inputId = "school_closure"),
                        overlayToken("Social&nbsp;distancing", inputId = "social_distancing"),
                        tags$br(), tags$br(),
                        overlayToken("Elderly&nbsp;shielding&nbsp;(stay-at-home)", inputId = "elderly_shielding_part"),
                        overlayToken("Elderly&nbsp;shielding&nbsp;(full&nbsp;shielding)", inputId = "elderly_shielding_full"),
                        tags$br(), tags$br(),
                        overlayToken("Self-isolation", inputId = "self_isolation"),
                        overlayToken("Lockdown", inputId = "lockdown")
                    )
                )
            )
        )
    ),

    # Notices
    div(id = "notices", style = "max-width: 980px; min-width: 980px; margin: 0 auto !important; float: none !important; padding: 10px",
        tags$br(),
        h6(HTML("Made by the Centre for Mathematical Modelling of Infectious Diseases &middot; London School of Hygiene and Tropical Medicine")),
        tags$br()
    )
)
