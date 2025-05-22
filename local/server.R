# covidm shiny app: server

function(input, output, session)
{
    # ---------- SET UP ----------
    ov = overlayServer("cases_plot", 8, width = 56,
        colours = heat.colors, opacity = 0.25, icon = icon("play"))
    opt = reactiveValues(
        type = rep(1, 8),
        strength = rep(50, 8)
    )

    output$cases_plot_menu = renderUI({
        i = req(ov$editing);
        tagList(
            textOutput("menuLabel"),
            selectInput(inputId = "int_type", label = NULL, choices = intervention_types, selected = opt$type[i]),
            sliderInput(inputId = "int_strength", label = 'Strength', value = opt$strength[i], min = 0, max = 100)
        )
    })

    output$menuLabel = renderText({
        i = req(ov$editing);
        d = function(t) format(as.Date(round(t), origin = "1970-01-01"), "%b %d");
        return (c(d(ov$cx0[i]), "–", d(ov$cx1[i])))
    })

    observeEvent(input$int_strength, {
        i = req(ov$editing);
        opt$strength[i] = input$int_strength
    })

    observeEvent(input$int_type, {
        i = req(ov$editing);
        opt$type[i] = as.integer(input$int_type)
        ov$label[i] = names(intervention_types)[as.integer(input$int_type)]
    })


    # TODO
    # something like this will need to be added when date range changes
    # observeEvent(parameters(), { ov$update_x() })

    # ---------- SIMULATION ----------

    # Some fake parameters
    params = list(
        date0 = ymd("2025-01-01"),
        time0 = 0,
        time1 = 365
    )

    # Simulation dynamics for unmitigated epidemic
    unmitigated = reactive({
        dynamics = data.table(
            t = params$time0 : params$time1
        )
        dynamics[, date := params$date0 + t];
        dynamics[, y := 150 + 100 * sin(t / 50)];
        return (dynamics)
    });

    # Simulation dynamics for mitigated epidemic
    mitigated = reactive({
        dynamics = data.table(
            t = params$time0 : params$time1
        )
        dynamics[, date := params$date0 + t];
        dynamics[, y := 150 + 10 * sin(t / 50)];

        # Read from interventions
        tref = as.numeric(params$date0);
        for (i in 1:length(ov$active)) {
            if (ov$active[i]) {
                times = c(ov$cx0[i] - tref, ov$cx1[i] - tref)
                req(opt$strength)
                strength = opt$strength[i] / 100
                dynamics[t %between% times, y := y * (1 - strength)]
            }
        }
        return (dynamics)
    });

    # ---------- DISPLAY PANEL ----------

    # Cases view
    output$cases_plot = renderPlot({
        dynU = unmitigated();
        dynM = mitigated();

        plot = ggplot() +
            geom_line(data = dynU, aes(x = date, y = y), colour = "#afc6e9") +
            geom_line(data = dynM, aes(x = date, y = y), colour = "#0044aa") +
            ylim(0, NA) +
            labs(x = NULL, y = "Cases")

        overlayBounds(ov, plot,
            xlim = c(params$date0, params$date0 + params$time1),
            ylim = c(0, NA))

        return (plot)
    })
}

    # Cases view
    # output$cases_plot = renderImage({
    #     dynU = unmitigated();
    #     dynM = mitigated();
    #
    #     embed_svg(vvplot(960, 480, bgcol = "#f8f8f8",
    #         vvpanel(
    #             if (input$compare) vvline(dynU$date, dynU$y, style = "stroke:#afc6e9") else NULL,
    #             vvline(dynM$date, dynM$y, style = "stroke:#0044aa; stroke-width: 2px"),
    #             vvlegend("line", 0.8, 0.95, "#0044aa", "Cases"),
    #             ylab = "Cases"
    #         ),
    #
    #         vvpanel(vvbar(c("Unmit.", "Mit."), c(100, 90), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
    #         vvpanel(vvbar(c("Unmit.", "Mit."), c(100, 90), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
    #         vvpanel(vvbar(c("Unmit.", "Mit."), c(100, 90), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
    #         layout = list(
    #             c(0, 0, 860, 480),
    #             c(860, 0, 100, 160),
    #             c(860, 160, 100, 160),
    #             c(860, 320, 100, 160)
    #         )
    #     ));
    # }, deleteFile = TRUE);
