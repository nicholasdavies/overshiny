# covidm shiny app: server

Run = function(N, I0, beta, delta, gamma, t1)
{
    set.seed(12345)
    fx = function(K, lambda) {
        # rbinom(1, K, 1 - exp(-lambda))
        K * (1 - exp(-lambda))
    }

    dt = data.table(time = 0:t1, S = N - I0, E = I0, I = 0, R = 0)
    for (t in 1:t1) {
        St = dt[time == t - 1, S]
        Et = dt[time == t - 1, E]
        It = dt[time == t - 1, I]
        Rt = dt[time == t - 1, R]
        SE = fx(St, beta[t] * It/N)
        EI = fx(Et, delta)
        IR = fx(It, gamma)
        RS = fx(Rt, 0.02)

        St = St - SE + RS
        Et = Et + SE - EI
        It = It + EI - IR
        Rt = Rt + IR - RS

        dt[time == t, S := St]
        dt[time == t, E := Et]
        dt[time == t, I := It]
        dt[time == t, R := Rt]
    }
    return (dt[])
}

function(input, output, session)
{
    # ---------- SET UP ----------
    ov = overlayServer("plot", 8, width = 56, opacity = 0.25)
    opt = reactiveValues(
        type = rep(1, 8),
        strength = rep(50, 8)
    )

    output$plot_menu = renderUI({
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
        dynamics[, R0 := y / 100]
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
        for (i in which(ov$active)) {
            times = c(ov$cx0[i] - tref, ov$cx1[i] - tref)
            req(opt$strength)
            strength = opt$strength[i] / 100
            dynamics[t %between% times, y := y * (1 - strength)]
        }
        dynamics[, R0 := y / 100]
        return (dynamics)
    });

    # ---------- DISPLAY PANEL ----------

    output$plot = renderPlot({
        dynU = unmitigated();
        dynM = mitigated();

        r0 = dynM
        trace = Run(1000, 2, r0[, R0] * 0.4, 0.5, 0.2, 365)

        if (input$display_tabs == "transmission") {
            ov$show = TRUE
            plot = ggplot(r0) +
                geom_line(aes(x = date, y = R0), colour = "#af4433") +
                ylim(0, 2) +
                labs(x = NULL, y = "Basic reproduction number")

            overlayBounds(ov, plot,
                xlim = c(params$date0, params$date0 + params$time1),
                ylim = c(0, NA))

            return (plot)
        } else if (input$display_tabs == "cases") {
            ov$show = TRUE
            plot(x = params$date0 + trace$time, y = trace$I, type = "l", col = "#0044aa")

            overlayBounds(ov, "base",
                xlim = c(params$date0, params$date0 + params$time1),
                ylim = c(0, NA))

            return (NULL)
        }
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
