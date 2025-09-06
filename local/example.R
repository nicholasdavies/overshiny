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
            overlayPlotOutput("plot", width = "100%", height = 300)
        )
    )
)

# --- App logic ---
server <- function(input, output, session)
{
    # --- OVERLAY SETUP ---

    # Initialise 8 draggable/resizable overlays
    ov <- overlayServer("plot", 8, width = 56, # 56 days = 8 weeks default width
        data = list(strength = 50), snap = snapGrid())

    # Toggle overlay visibility based on checkbox
    observe({
        ov$show <- isTRUE(input$show_overlays)
    })

    # --- OVERLAY DROPDOWN MENU ---

    # Render dropdown menu when an overlay is being edited
    output$plot_menu <- renderUI({
        i <- req(ov$editing)  # Current overlay being edited
        fmt <- function(t) format(as.Date(t, origin = "1970-01-01"), "%b %d")

        list(
            div(paste(fmt(ov$cx0[i]), "–", fmt(ov$cx1[i]))),
            selectInput("plot_label", NULL, choices = c("Grow", "Shrink"), selected = ov$label[i]),
            sliderInput("plot_strength", "Strength", min = 0, max = 100, value = ov$data$strength[i]),
            dateInput("plot_cx", "Start date", value = ov$cx0[i]),
            sliderInput("plot_cw", "Duration", min = 1, max = floor(ov$bound_cw), value = ov$cx1[i] - ov$cx0[i])
        )
    })

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
    output$plot <- renderPlot({
        plot <- ggplot(data()) +
            geom_line(aes(x = date, y = y)) +
            ylim(0, 3) +
            labs(x = NULL, y = "Signal")

        overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
    })
}

# --- Run app ---
shinyApp(ui, server)
