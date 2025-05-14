# covidm shiny app: server

function(input, output, session)
{
    # ---------- SET UP ----------
    # Set intervention rectangles as horizontally draggable and resizable
    jqui_resizable(jqui_draggable(ui = "#iv_1", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 410, minHeight = 410, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_2", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 390, minHeight = 390, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_3", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 370, minHeight = 370, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_4", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 350, minHeight = 350, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_5", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 330, minHeight = 330, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_6", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 310, minHeight = 310, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_7", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 290, minHeight = 290, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_8", options = list(axis = "x", containment = "#iv_bounds")),
        options = list(maxHeight = 270, minHeight = 270, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))

    # New interventions
    jqui_draggable(ui = "#overshiny-token-school_closure",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-social_distancing",      options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-elderly_shielding_part", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-elderly_shielding_full", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-self_isolation",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-lockdown",               options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#overshiny-token-custom",                 options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_droppable(ui = "#display", options = list(accept = "[id^=overshiny-token-]", shiny = shdrop)); ## TODO here she is "display"

    # ---------- INTERVENTIONS ----------

    # Set up interventions
    iv = reactiveValues(
        active   = rep(F, 8),
        x        = rep(0, 8), # left pixel position of intervention rectangle
        w        = rep(0, 8), # pixel width of intervention rectangle
        t0       = rep(0, 8), # absolute start time of intervention
        t1       = rep(1, 8)  # absolute end time of intervention
    );

    # Set t0 and t1 of interventions from x and w, updating label for intervention i
    update_iv_t = function(i) {
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));

        # Set t0 and t1
        iv$t0 = (iv$x / DISP_W) * (ax_t1 - ax_t0) + ax_t0;
        iv$t1 = ((iv$x + iv$w) / DISP_W) * (ax_t1 - ax_t0) + ax_t0;

        # Set timespan text
        d = function(t) format(as.Date(round(t), origin = "1970-01-01"), "%b %d");
        html(paste0("int_timespan_", i), paste0(d(iv$t0[i]), " &ndash; ", d(iv$t1[i])));
    }

    # Set x and w of interventions from t0 and t1
    update_iv_x = function() {
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));

        # Ensure times are in proper range
        iv$t0 = pmax(iv$t0, min(as.numeric(ymd(params$date0)), ax_t0));
        iv$t1 = pmin(iv$t1, max(as.numeric(ymd(params$date0) + params$time1), ax_t1));
        iv$t1 = pmax(iv$t1, iv$t0 + 7);

        # Set x and w, as well as updating actual positions
        iv$x = (iv$t0 - ax_t0) * DISP_W / (ax_t1 - ax_t0);
        iv$w = (iv$t1 - iv$t0) * DISP_W / (ax_t1 - ax_t0);
        for (i in 1:8) {
            setcss(paste0("iv_", i), left = iv$x[i], width = iv$w[i]);
        }
    }

    # Observe changing type of interventions
    observeEvent(input$int_type_1, { i = as.numeric(input$int_type_1); updateSliderInput(session, inputId = "int_strength_1", label = iv_def[[i]]$strength_name); html("iv_title_1", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_2, { i = as.numeric(input$int_type_2); updateSliderInput(session, inputId = "int_strength_2", label = iv_def[[i]]$strength_name); html("iv_title_2", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_3, { i = as.numeric(input$int_type_3); updateSliderInput(session, inputId = "int_strength_3", label = iv_def[[i]]$strength_name); html("iv_title_3", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_4, { i = as.numeric(input$int_type_4); updateSliderInput(session, inputId = "int_strength_4", label = iv_def[[i]]$strength_name); html("iv_title_4", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_5, { i = as.numeric(input$int_type_5); updateSliderInput(session, inputId = "int_strength_5", label = iv_def[[i]]$strength_name); html("iv_title_5", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_6, { i = as.numeric(input$int_type_6); updateSliderInput(session, inputId = "int_strength_6", label = iv_def[[i]]$strength_name); html("iv_title_6", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_7, { i = as.numeric(input$int_type_7); updateSliderInput(session, inputId = "int_strength_7", label = iv_def[[i]]$strength_name); html("iv_title_7", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_8, { i = as.numeric(input$int_type_8); updateSliderInput(session, inputId = "int_strength_8", label = iv_def[[i]]$strength_name); html("iv_title_8", nbsp(iv_def[[i]]$name)); });

    # Observe dragging and resizing of interventions
    observeEvent(input$iv_1_mv, { iv$x[1] = input$iv_1_mv; update_iv_t(1); });
    observeEvent(input$iv_2_mv, { iv$x[2] = input$iv_2_mv; update_iv_t(2); });
    observeEvent(input$iv_3_mv, { iv$x[3] = input$iv_3_mv; update_iv_t(3); });
    observeEvent(input$iv_4_mv, { iv$x[4] = input$iv_4_mv; update_iv_t(4); });
    observeEvent(input$iv_5_mv, { iv$x[5] = input$iv_5_mv; update_iv_t(5); });
    observeEvent(input$iv_6_mv, { iv$x[6] = input$iv_6_mv; update_iv_t(6); });
    observeEvent(input$iv_7_mv, { iv$x[7] = input$iv_7_mv; update_iv_t(7); });
    observeEvent(input$iv_8_mv, { iv$x[8] = input$iv_8_mv; update_iv_t(8); });

    observeEvent(input$iv_1_sz, { iv$x[1] = input$iv_1_sz$x; iv$w[1] = input$iv_1_sz$w; update_iv_t(1); });
    observeEvent(input$iv_2_sz, { iv$x[2] = input$iv_2_sz$x; iv$w[2] = input$iv_2_sz$w; update_iv_t(2); });
    observeEvent(input$iv_3_sz, { iv$x[3] = input$iv_3_sz$x; iv$w[3] = input$iv_3_sz$w; update_iv_t(3); });
    observeEvent(input$iv_4_sz, { iv$x[4] = input$iv_4_sz$x; iv$w[4] = input$iv_4_sz$w; update_iv_t(4); });
    observeEvent(input$iv_5_sz, { iv$x[5] = input$iv_5_sz$x; iv$w[5] = input$iv_5_sz$w; update_iv_t(5); });
    observeEvent(input$iv_6_sz, { iv$x[6] = input$iv_6_sz$x; iv$w[6] = input$iv_6_sz$w; update_iv_t(6); });
    observeEvent(input$iv_7_sz, { iv$x[7] = input$iv_7_sz$x; iv$w[7] = input$iv_7_sz$w; update_iv_t(7); });
    observeEvent(input$iv_8_sz, { iv$x[8] = input$iv_8_sz$x; iv$w[8] = input$iv_8_sz$w; update_iv_t(8); });

    # ---------- SIMULATION ----------

    # Some fake parameters
    parameters = reactive({
        list(
            date0 = ymd("2025-01-01"),
            time0 = 0,
            time1 = 365
        )
    });

    # Update intervention rectangles when parameters change
    observeEvent(parameters(), { update_iv_x() });

    # Simulation dynamics for unmitigated epidemic
    unmitigated = reactive({
        params = parameters();
        dynamics = data.table(
            t = params$time0 : params$time1
        )
        dynamics[, date := params$date0 + t];
        dynamics[, y := 150 + 100 * sin(t / 50)];
        return (dynamics)
    });

    # Simulation dynamics for mitigated epidemic
    mitigated = reactive({
        params = parameters();
        dynamics = data.table(
            t = params$time0 : params$time1
        )
        dynamics[, date := params$date0 + t];
        dynamics[, y := 150 + 10 * sin(t / 50)];

        # Read from interventions
        tref = as.numeric(params$date0);
        for (i in 1:length(iv$active)) {
            if (iv$active[i]) {
                times = c(iv$t0[i] - tref, iv$t1[i] - tref)
                strength = input[[paste0("int_strength_", i)]] / 100
                dynamics[t %between% times, y := y * (1 - strength)]
            }
        }
        return (dynamics)
    });

    # ---------- DISPLAY PANEL ----------

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

    output$cases_plot = renderPlot({
        dynU = unmitigated();
        dynM = mitigated();

        # TODO
        # here, need to set the this and that

        ggplot() +
            geom_line(data = dynU, aes(x = date, y = y), colour = "#afc6e9") +
            geom_line(data = dynM, aes(x = date, y = y), colour = "#0044aa") +
            labs(x = NULL, y = "Cases")
    })

    # ---------- CONTROL PANEL ----------

    # INTERVENTIONS CONTROL PANEL
    # Add new intervention
    observeEvent(input$display_notify, {
        if (!is.null(input$display_notify)) {
            if (input$display_notify$id %like% "^overshiny-token-") {
                available = which(iv$active == F)[1];
                if (!is.na(available)) {
                    # Calculate range of times shown on X axis
                    params = parameters();
                    xaxis_breaks = breaks_pretty(5)(c(params$date0, params$date0 + params$time1));
                    ax_t0 = as.numeric(xaxis_breaks[1] - params$date0);
                    ax_t1 = as.numeric(tail(xaxis_breaks, 1) - params$date0);

                    iv_type_i = match(input$display_notify$id, intervention_adders, nomatch = 1);
                    iv$active[available]   = T;
                    iv$w[available]        = 56 * DISP_W / (ax_t1 - ax_t0);
                    iv$x[available]        = max(0, min(DISP_W - iv$w[available], input$display_notify$x - 55));
                    update_iv_t(available);

                    updateSelectInput(session, inputId = paste0("int_type_", available), label = NULL, choices = intervention_types, selected = iv_type_i);
                    updateSliderInput(session, inputId = paste0("int_strength_", available), label = iv_def[[iv_type_i]]$strength_name, value = iv_def[[iv_type_i]]$strength_default);
                    html(paste0("iv_title_", available), nbsp(iv_def[[iv_type_i]]$name));
                    setcss(paste0("iv_", available), display = "block", left = iv$x[available], width = iv$w[available]);
                }
            }
        }
    });

    # Remove interventions
    observeEvent(input$int_remove_1, { iv$active[1] = F; setcss("iv_1", display = "none"); toggleDropdownButton("int_menu_1"); });
    observeEvent(input$int_remove_2, { iv$active[2] = F; setcss("iv_2", display = "none"); toggleDropdownButton("int_menu_2"); });
    observeEvent(input$int_remove_3, { iv$active[3] = F; setcss("iv_3", display = "none"); toggleDropdownButton("int_menu_3"); });
    observeEvent(input$int_remove_4, { iv$active[4] = F; setcss("iv_4", display = "none"); toggleDropdownButton("int_menu_4"); });
    observeEvent(input$int_remove_5, { iv$active[5] = F; setcss("iv_5", display = "none"); toggleDropdownButton("int_menu_5"); });
    observeEvent(input$int_remove_6, { iv$active[6] = F; setcss("iv_6", display = "none"); toggleDropdownButton("int_menu_6"); });
    observeEvent(input$int_remove_7, { iv$active[7] = F; setcss("iv_7", display = "none"); toggleDropdownButton("int_menu_7"); });
    observeEvent(input$int_remove_8, { iv$active[8] = F; setcss("iv_8", display = "none"); toggleDropdownButton("int_menu_8"); });
}
