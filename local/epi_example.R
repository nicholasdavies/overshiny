library(epidemics)
library(shiny)
library(overshiny)
library(ggplot2)

# --- User interface ---
ui <- fluidPage(
    titlePanel("SEIRV model with interventions"),

    # Main plot with support for overlays
    overlayPlotOutput("display", width = "100%", height = 400),

    # 3 columns of inputs
    fluidRow(
        column(4,
            # Basic epidemic settings
            h3("Epidemic"),
            dateRangeInput("date_range", "Date range", start = "2025-01-01", end = "2025-12-31"),
            numericInput("pop_size", "Population size (millions)", value = 59, min = 1),
            sliderInput("init_infec", "Initial proportion infectious (%)", value = 0.1, min = 0, max = 1, step = 0.01)
        ),

        column(4,
            # Pathogen settings
            h3("Pathogen"),
            sliderInput("R0", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
                value = 1.3, min = 0, max = 5, step = 0.05),
            sliderInput("latent_pd", "Latent period (days)", value = 2, min = 1, max = 7, step = 0.1),
            sliderInput("infec_pd", "Infectious period (days)", value = 7, min = 1, max = 10, step = 0.1)
        ),

        column(4,
            # Overlay controls: tokens that can be dragged onto the plot
            h3("Interventions"),
            overlayToken("vx", "Vaccination"),
            overlayToken("tx", "Transmission")
        )
    )
)

# --- App logic ---
server <- function(input, output)
{
    # --- OVERLAY SETUP ---

    # Dropdown menu for overlays
    menu <- function(ov, i) {
        if (ov$label[i] == "Vaccination") {
            numericInput("display_vac_rate", "Vaccines per day (thousands)",
                value = ov$data$vac_rate[i], min = 0, max = 10000)
        } else if (ov$label[i] == "Transmission") {
            sliderInput("display_int_strength", "Transmission reduction (%)",
                value = ov$data$int_strength[i], min = 0, max = 100)
        }
    }

    # Initialise 8 draggable/resizable overlays
    ov <- overlayServer("display", 8, width = 56, # 56 days = 8 weeks default width
        data = list(vac_rate = 10, int_strength = 20),
        snap = snapGrid(),
        heading = dateHeading("%b %e"),
        select = TRUE,
        menu = menu)

    # --- EPIDEMIC MODEL RUNS BASED ON OVERLAY POSITIONS ---

    # Model run function
    run_model <- function(...)
    {
        # Transform parameters
        I0 <- input$init_infec / 100;
        duration <- as.numeric(input$date_range[2] - input$date_range[1])
        infec_rate <- 1 / input$latent_pd
        recov_rate <- 1 / input$infec_pd
        trans_rate <- input$R0 * recov_rate

        # Build population
        pop <- population(
            name = "Utopia",
            contact_matrix = matrix(1),
            demography_vector = input$pop_size * 1000000,
            initial_conditions = matrix(c(1 - I0, 0, I0, 0, 0), nrow = 1)
        )

        # Run model (with additional parameters from ...)
        results <- model_default(pop, transmission_rate = trans_rate,
            infectiousness_rate = infec_rate, recovery_rate = recov_rate,
            time_end = duration, ...)

        # Transform results -- construct date and only return infection prevalence
        results$date <- results$time + input$date_range[1]
        results <- results[results$compartment == "infectious", ]
        return (results)
    }

    # Unmitigated epidemic
    epi_unmitigated <- reactive({
        run_model()
    })

    # Mitigated epidemic
    epi_mitigated <- reactive({
        # Create interventions
        tx_int <- list()
        vax <- NULL
        for (i in which(ov$active)) {
            begin <- ov$cx0[i] - as.numeric(input$date_range[1])
            end <- ov$cx1[i] - as.numeric(input$date_range[1])
            if (ov$label[i] == "Vaccination") {
                nu <- ov$data$vac_rate[i] * 1000 / (input$pop_size * 1000000)
                if (is.null(vax)) {
                    vax <- vaccination(name = as.character(i), nu = matrix(nu),
                        time_begin = matrix(begin), time_end = matrix(end))
                } else {
                    ov$active[i] <- FALSE
                }
            } else if (ov$label[i] == "Transmission") {
                reduc <- ov$data$int_strength[i] / 100
                tx_int[[length(tx_int) + 1]] <- intervention(name = as.character(i),
                    type = "rate", time_begin = matrix(begin), time_end = matrix(end),
                    reduction = reduc)
            }
        }

        # Get mitigated model results
        int <- list()
        if (length(tx_int)) {
            int[["transmission_rate"]] <- do.call(c, tx_int)
        }
        run_model(vaccination = vax,
            intervention = if (length(int)) int else NULL)
    })

    # --- RENDERING OF EPI CURVES ---

    # Render plot and align overlays to current axis limits
    output$display <- renderPlot({
        plot <- ggplot() +
            geom_line(data = epi_unmitigated(),
                aes(x = date, y = value/1000), alpha = 0.5) +
            geom_line(data = epi_mitigated(),
                aes(x = date, y = value/1000)) +
            labs(x = NULL, y = "Infection prevalence (thousands)") +
            ylim(0, NA)

        overlayBounds(ov, plot, xlim = c(input$date_range), ylim = c(0, NA))
    })
}

# --- Run app ---
shinyApp(ui, server)
