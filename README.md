# overshiny: Interactive overlays on Shiny plots

<!-- badges: start -->
[![R-CMD-check](https://github.com/nicholasdavies/overshiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nicholasdavies/overshiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`overshiny` provides draggable and resizable rectangular elements that
overlay plots in Shiny apps. This may be useful in applications where users
need to define regions on the plot for further input or processing. Currently, 
the overlays are only designed to move along the x axis of the plot.

## Status

`overshiny` is under active development. There are some missing features.

Missing features:

- Doesn't handle window resizing or plot resizing very well.
- No simple functionality to add a new overlay (without using a draggable 
overlay token)


## Installation

You can install the released version of overshiny from CRAN:

``` r
install.packages("overshiny")
```

Or, install the development version of overshiny from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("nicholasdavies/overshiny")
```

## Example

This example shows the basic functionality of `overshiny`:

``` r
library(shiny)
library(ggplot2)
library(overshiny)

# --- User interface ---
ui <- fluidPage(
    titlePanel("Overlay demo"),

    sidebarLayout(
        sidebarPanel(
            # Control whether overlays are displayed and whether they alter the plot
            checkboxInput("show_overlays", "Show overlays", value = TRUE),
            checkboxInput("enable_logic", "Enable overlay logic", value = TRUE),
            tags$hr(),

            # Select date range for the plot
            dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
            tags$hr(),

            # Overlay controls: tokens that can be dragged onto the plot
            h5("Drag tokens below onto the plot:"),
            overlayToken("grow", "Grow"),
            overlayToken("shrink", "Shrink")
        ),

        mainPanel(
            # Main plot with support for overlays
            overlayPlotOutput("display", width = "100%", height = 300)
        )
    )
)

# --- App logic ---
server <- function(input, output, session)
{
    # --- OVERLAY SETUP ---

    # Initialise 8 draggable/resizable overlays
    ov <- overlayServer("display", 8, width = 56, # 56 days = 8 weeks default width
        data = list(strength = 50), snap = snapGrid(),
        heading = dateHeading("%b %e"), select = TRUE)

    # Toggle overlay visibility based on checkbox
    observe({
        ov$show <- isTRUE(input$show_overlays)
    })

    # --- OVERLAY DROPDOWN MENU ---

    # Render dropdown menu when an overlay is being edited
    ov$menu <- function(ov, i) {
        list(
            sliderInput("display_strength", "Strength", min = 0, max = 100, value = ov$data$strength[i]),
            dateInput("display_cx", "Start date", value = ov$cx0[i]),
            sliderInput("display_cw", "Duration", min = 1, max = floor(ov$bound_cw), value = ov$cx1[i] - ov$cx0[i])
        )
    }

    # --- DATA PROCESSING BASED ON OVERLAY POSITION ---

    # Reactive dataset: oscillating signal modified by active overlays
    data <- reactive({
        date_seq <- seq(input$date_range[1], input$date_range[2], by = "1 day")
        y <- 1 + 0.5 * sin(as.numeric(date_seq) / 58)  # oscillating signal

        # Modify signal according to active overlays if logic is enabled
        if (isTRUE(input$enable_logic)) {
            for (i in which(ov$active)) {
                start <- as.Date(ov$cx0[i], origin = "1970-01-01")
                end <- as.Date(ov$cx1[i], origin = "1970-01-01")
                in_range <- date_seq >= start & date_seq <= end
                factor <- ov$data$strength[i] / 100
                y[in_range] <- y[in_range] * if (ov$label[i] == "Grow") (1 + factor) else (1 - factor)
            }
        }

        data.frame(date = date_seq, y = y)
    })

    # --- RENDERING OF DATA ---

    # Render plot and align overlays to current axis limits
    output$display <- renderPlot({
        plot <- ggplot(data()) +
            geom_line(aes(x = date, y = y)) +
            ylim(0, 3) +
            labs(x = NULL, y = "Signal")

        overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
    })
}

# --- Run app ---
shinyApp(ui, server)
```
