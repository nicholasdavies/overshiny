library(scales)
library(viridis)
library(png)
library(base64enc)

# create integer representation of viridis palette
vir_R = strtoi(paste0("0x", str_sub(viridis_pal()(32), 2, 3)))
vir_G = strtoi(paste0("0x", str_sub(viridis_pal()(32), 4, 5)))
vir_B = strtoi(paste0("0x", str_sub(viridis_pal()(32), 6, 7)))

# <defs><style type="text/css">@import url("https://fonts.googleapis.com/css?family=Roboto");</style></defs>


nice = function(x) {
    if (is.numeric(x)) {
        if (max(abs(x)) > 1000000) {
            x = paste0(signif(x, 3) / 1000000, "M");
        } else if (max(abs(x) > 1000)) {
            x = paste0(signif(x, 3) / 1000, "k");
        } else {
            x = signif(x, 3);
        }
    }
    x
}

# matrix to embedded png
to_embedded_image = function(mat, x, y, width, height)
{
    w = ncol(mat)
    h = nrow(mat)

    # transform to log scale
    transformed = log(0.5 + mat);
    transformed = t(transformed)[h:1,];
    minv = min(transformed);
    maxv = max(transformed);

    # alias
    aliased = pmin(31, floor(32 * (transformed - minv) / (maxv - minv)))

    # convert to raw array format
    rawarray = array(data = as.raw(c(vir_R[aliased + 1], vir_G[aliased + 1], vir_B[aliased + 1], rep(255, w * h))), dim = c(h, w, 4))
    rawarray = aperm(rawarray, c(3, 2, 1))

    # base-64 encode as a PNG
    b64 = base64encode(writePNG(rawarray))

    # return SVG representation
    paste0('<image x="', x, '" y="', y, '" width="', width, '" height="', height, '" style="image-rendering: pixelated" href="data:image/png;base64,', b64, '" />')
}

embed_svg = function(svg_plot, w, h)
{
    # Temp file for output
    filename = tempfile(fileext = '.svg');
    f = file(filename, open = "wt");

    # Generate plot
    writeLines(svg_plot$svg, f);
    close(f);

    # Return a list containing the filename
    list(src = filename, width = svg_plot$w, height = svg_plot$h, contentType = 'image/svg+xml', alt = "Plot");
}

show_svg = function(svg_plot)
{
    # Temp file for output
    filename = tempfile(fileext = '.svg');
    f = file(filename, open = "wt");

    # Generate plot
    writeLines(svg_plot$svg, f);
    close(f);

    system(paste("open", filename, "-a Google\\ Chrome.app"))
}


# Axes
vvaxes = function(xscale, yscale, px, py, pw, ph, xtick, xlab, ylab, title, margin, xpositive, xdateformat)
{
    # Extent of the actual plotting area
    pa_x = px + margin[4];
    pa_w = pw - margin[2] - margin[4];
    pa_y = py + margin[1];
    pa_h = ph - margin[1] - margin[3];

    get_breaks_etc = function(scale, pa_x, pa_w, breaks_func, loose)
    {
        if (is.list(scale)) {
            x_breaks = breaks_func(5, only.loose = loose)(c(scale$min, scale$max));
            unit_x = pa_w / as.numeric(tail(x_breaks, 1) - x_breaks[1]);
            x_to_pixel = function(x_breaks) function(x) {
                cx = as.character(x);
                round(ifelse(cx == "$U$", unit_x, ifelse(cx == "$X$", pa_x, ifelse(cx == "$W$", pa_w, pa_x + unit_x * as.numeric(x - x_breaks[1])))), 2);
            }
        } else {
            x_breaks = scale;
            unit_x = pa_w / length(x_breaks);
            x_to_pixel = function(x_breaks) function(x) {
                cx = as.character(x);
                round(ifelse(cx == "$U$", unit_x, ifelse(cx == "$X$", pa_x, ifelse(cx == "$W$", pa_w, pa_x + unit_x * (match(x, x_breaks) - 0.5)))), 2);
            }
        }
        return (list(x_breaks, x_to_pixel(x_breaks)))
    }

    xetc = get_breaks_etc(xscale, pa_x, pa_w, breaks_pretty, T);
    yetc = get_breaks_etc(yscale, pa_y + pa_h, -pa_h, breaks_extended, T);
    x_breaks = xetc[[1]];
    y_breaks = yetc[[1]];

    list(
        x_to_pixel = xetc[[2]],
        y_to_pixel = yetc[[2]],
        render = function(x_to_pixel, y_to_pixel) {
            # PLOT COMPONENTS
            # -- Axis lines --
            ax_points = paste(c(x_to_pixel("$X$"), x_to_pixel("$X$"), x_to_pixel("$X$") + x_to_pixel("$W$")),
                c(y_to_pixel("$X$") + y_to_pixel("$W$"), y_to_pixel("$X$"), y_to_pixel("$X$")), sep = ",", collapse = " ");
            svg_axes = paste0('<polyline points="', ax_points, '" class="vv_axis" />');

            # -- Make nice tick labels --
            nice_labels = function(labels, axlabel) {
                if (is.numeric(labels)) {
                    if (max(abs(labels)) > 1000000) {
                        labels = labels / 1000000;
                        axlabel = paste0(c(axlabel, "(millions)"), collapse = " ");
                    } else if (max(abs(labels) > 1000)) {
                        labels = labels / 1000;
                        axlabel = paste0(c(axlabel, "(thousands)"), collapse = " ");
                    }
                }
                list(labels, axlabel);
            }

            xl = nice_labels(x_breaks, xlab);
            x_labels = if (xpositive) abs(xl[[1]]) else xl[[1]];
            x_labels = if (is.Date(x_labels)) format(x_labels, xdateformat) else x_labels;
            xlab = xl[[2]];

            yl = nice_labels(y_breaks, ylab);
            y_labels = yl[[1]];
            ylab = yl[[2]];

            # -- Ticks and tick labels --
            tick_length = 3;
            y_tick = function(yb, label) {
                paste0('<line x1="', pa_x - tick_length, '" x2="', pa_x, '" y1="', y_to_pixel(yb), '" y2="', y_to_pixel(yb), '" class="vv_tick" />',
                    '<text x="', pa_x - 2 * tick_length, '" y="', y_to_pixel(yb), '" class="vv_ticklaby">', label, '</text>', collapse = '\n')
            }
            x_tick = function(xb, label) {
                if (is.null(xtick)) {
                    paste0('<line x1="', x_to_pixel(xb), '" x2="', x_to_pixel(xb), '" y1="', pa_y + pa_h, '" y2="', pa_y + pa_h + tick_length, '" class="vv_tick" />',
                        '<text x="', x_to_pixel(xb), '" y="', pa_y + pa_h + 2 * tick_length, '" class="vv_ticklabx">', label, '</text>', collapse = '\n')
                } else {
                    paste0('<line x1="', x_to_pixel(xb), '" x2="', x_to_pixel(xb), '" y1="', pa_y + pa_h, '" y2="', pa_y + pa_h + tick_length, '" class="vv_tick" />',
                        '<text x="', x_to_pixel(xb), '" y="', pa_y + pa_h + 2 * tick_length, '" transform="rotate(', -xtick, ' ', x_to_pixel(xb), ' ', pa_y + pa_h + 2 * tick_length, ')" ',
                        'class="vv_ticklabx_rot">', label, '</text>', collapse = '\n')
                }
            }
            svg_ticks = paste(y_tick(y_breaks, y_labels), x_tick(x_breaks, x_labels), sep = '\n');

            # -- Titles --
            svg_titles = "";
            if (!is.null(title)) {
                svg_titles = paste0(svg_titles, '<text x="', pa_x + pa_w / 2, '" y="', pa_y / 2, '" class="vv_title">', title, '</text>', collapse = '\n');
            }
            if (!is.null(xlab)) {
                svg_titles = paste0(svg_titles, '<text x="', pa_x + pa_w / 2, '" y="', py + ph, '" class="vv_labelx">', xlab, '</text>', collapse = '\n');
            }
            if (!is.null(ylab)) {
                svg_titles = paste0(svg_titles, '<text x="', px, '" y="', pa_y + pa_h / 2,
                    '" transform="rotate(', -90, ' ', px, ' ', pa_y + pa_h / 2, ')" class="vv_labely">', ylab, '</text>', collapse = '\n');
            }

            paste(svg_axes, svg_ticks, svg_titles, sep = '\n')
        }
    )
}

# Line
vvline = function(x, y, style = "")
{
    list(
        xscale = list(min = min(x, na.rm = T), max = max(x, na.rm = T)),
        yscale = list(min = min(c(0, y), na.rm = T), max = max(c(0, y), na.rm = T)),
        render = function(x_to_pixel, y_to_pixel) {
            points = paste(x_to_pixel(x), y_to_pixel(y), sep = ",", collapse = " ");
            paste0('<polyline points="', points, '" class="vv_line" style="', style, '" />');
        }
    )
}

# Ribbon
vvribbon = function(x, ymin, ymax, style = "")
{
    list(
        xscale = list(min = min(x, na.rm = T), max = max(x, na.rm = T)),
        yscale = list(min = min(c(0, ymin, ymax), na.rm = T), max = max(c(0, ymin, ymax), na.rm = T)),
        render = function(x_to_pixel, y_to_pixel) {
            points = paste(x_to_pixel(c(x, rev(x))), y_to_pixel(c(ymin, rev(ymax))), sep = ",", collapse = " ");
            paste0('<polygon points="', points, '" class="vv_ribbon" style="', style, '" />');
        }
    )
}

# Horizontal line
vvhline = function(y, style = "")
{
    list(
        xscale = NULL,
        yscale = list(min = min(c(0, y), na.rm = T), max = max(c(0, y), na.rm = T)),
        render = function(x_to_pixel, y_to_pixel) {
            x1 = x_to_pixel("$X$");
            x2 = x1 + x_to_pixel("$W$");
            y = y_to_pixel(y);
            paste0('<line x1="', x1, '" x2="', x2, '" y1="', y, '" y2="', y, '" class="vv_line" style="', style, '" />');
        }
    )
}

# Points
# TODO rescale is a bit of a hack here...
vvpoint = function(x, y, r = 2, rescale = T, style = "")
{
    list(
        xscale = if (rescale) list(min = min(x, na.rm = T), max = max(x, na.rm = T)) else NULL,
        yscale = list(min = min(c(0, y), na.rm = T), max = max(c(0, y), na.rm = T)),
        render = function(x_to_pixel, y_to_pixel) {
            paste0('<circle cx="', x_to_pixel(x), '" cy="', y_to_pixel(y), '" r="', r, '" class="vv_point" style="', style, '" />', collapse = "\n");
        }
    )
}

# Bar chart
vvbar = function(x, y, style = "", show = F)
{
    list(
        xscale = unique(x),
        yscale = list(min = min(c(0, y), na.rm = T), max = max(c(0, y), na.rm = T)),
        render = function(x_to_pixel, y_to_pixel) {
            if (show) {
                paste0('<rect x="', x_to_pixel(x) - x_to_pixel("$U$") / 2, '" y="', y_to_pixel(y), '" width="', x_to_pixel("$U$"), '" height="', y_to_pixel(0) - y_to_pixel(y),
                    '" class="vv_bar" style="', style, '" />',
                    '<text x="', x_to_pixel(x), '" y="', y_to_pixel(y) - 2, '" class="vv_textshow">', nice(y), '</text>', collapse = "\n");
            } else {
                paste0('<rect x="', x_to_pixel(x) - x_to_pixel("$U$") / 2, '" y="', y_to_pixel(y), '" width="', x_to_pixel("$U$"), '" height="', y_to_pixel(0) - y_to_pixel(y),
                    '" class="vv_bar" style="', style, '" />', collapse = "\n");
            }
        }
    )
}

# Pyramid chart
vvbarh = function(x, y, style = "")
{
    list(
        xscale = list(min = min(c(0, x), na.rm = T), max = max(c(0, x), na.rm = T)),
        yscale = unique(y),
        render = function(x_to_pixel, y_to_pixel) {
            paste0('<rect x="', ifelse(x > 0, x_to_pixel(0), x_to_pixel(x)), '" y="', y_to_pixel(y) + y_to_pixel("$U$") / 2,
                '" width="', abs(x_to_pixel(x) - x_to_pixel(0)), '" height="', -y_to_pixel("$U$"),
                '" class="vv_bar" style="', style, '" />', collapse = "\n");
        }
    )
}

# Matrix as image
vvmatrix = function(mat)
{
    cnames = unique(colnames(mat));
    if (is.null(cnames)) {
        cnames = seq_len(ncol(mat));
    }

    rnames = unique(rownames(mat));
    if (is.null(rnames)) {
        rnames = seq_len(nrow(mat));
    }

    list(
        xscale = cnames,
        yscale = rnames,
        render = function(x_to_pixel, y_to_pixel) {
            to_embedded_image(mat, x_to_pixel("$X$"), y_to_pixel("$X$") + y_to_pixel("$W$"), x_to_pixel("$W$"), -y_to_pixel("$W$"));
        }
    )
}

# Text annotation
vvtextann = function(x, y, text)
{
    list(
        xscale = NULL,
        yscale = NULL,
        render = function(x_to_pixel, y_to_pixel) {
            paste0('<text x="', x_to_pixel("$X$") + x * x_to_pixel("$W$"), '" y="', y_to_pixel("$X$") + y * y_to_pixel("$W$"), '" class="vv_textann">', text, '</text>', collapse = '\n')
        }
    )
}

# Limits
vvlimits = function(x, y)
{
    list(
        xscale = if (is.null(x)) NULL else list(min = min(x), max = max(x)),
        yscale = if (is.null(y)) NULL else list(min = min(y), max = max(y)),
        render = function(x_to_pixel, y_to_pixel) NULL
    )
}

# Legend
vvlegend = function(mode, x, y, colours, labels, yspacing = 12, xspacing = 4, keysize = 8)
{
    list(
        xscale = NULL,
        yscale = NULL,
        render = function(x_to_pixel, y_to_pixel) {
            xp = x_to_pixel("$X$") + x * x_to_pixel("$W$");
            yp = y_to_pixel("$X$") + y * y_to_pixel("$W$");
            n = length(labels);
            colours = rep_len(colours, n);
            if (mode == "line") {
                paste0('<line x1="', xp - xspacing / 2, '" x2="', xp - keysize - xspacing / 2,
                    '" y1="', yp + (0:(n-1)) * yspacing, '" y2="', yp + (0:(n-1)) * yspacing, '" style="stroke:', colours, '" />',
                    '<text x="', xp + xspacing/2, '" y="', yp + (0:(n-1)) * yspacing, '" class="vv_legend">', labels, '</text>', collapse = '\n');
            } else if (mode == "rect") {
                paste0('<rect x="', xp - xspacing / 2 - keysize, '" y="', yp + (0:(n-1)) * yspacing - keysize / 2,
                    '" width="', keysize, '" height="', keysize, '" style="stroke:none; fill:', colours, '" />',
                    '<text x="', xp + xspacing/2, '" y="', yp + (0:(n-1)) * yspacing, '" class="vv_legend">', labels, '</text>', collapse = '\n');
            } else if (mode == "point") {
                paste0('<circle cx="', xp - xspacing / 2, '" cy="', yp + (0:(n-1)) * yspacing, '" r="2" style="stroke:none; fill:', colours, '" />',
                    '<text x="', xp + xspacing/2, '" y="', yp + (0:(n-1)) * yspacing, '" class="vv_legend">', labels, '</text>', collapse = '\n');
            }
        }
    )
}

vvpanel = function(..., xtick = NULL, xlab = NULL, ylab = NULL, title = NULL, margin = c(10, 10, 40, 40), xpositive = F, xdateformat = "%e %b %Y")
{
    elements = list(...);
    function(px, py, pw, ph) {
        # -- Data scale --
        xscale = NULL;
        yscale = NULL;
        accommodate = function(existing, new) {
            if (is.null(existing)) {
                return (new)
            } else if (is.null(new)) {
                return (existing)
            } else if (is.list(new)) {
                return (list(min = min(existing$min, new$min, na.rm = T), max = max(existing$max, new$max, na.rm = T)))
            } else {
                return (union(existing, new))
            }
        }

        for (el in elements)
        {
            xscale = accommodate(xscale, el$xscale);
            yscale = accommodate(yscale, el$yscale);
        }
        axes = vvaxes(xscale, yscale, px, py, pw, ph, xtick, xlab, ylab, title, margin, xpositive, xdateformat);

        # -- Plot elements --
        svg_body = axes$render(axes$x_to_pixel, axes$y_to_pixel);
        for (el in elements) {
            if (!is.null(el)) {
                svg_body = paste(svg_body, el$render(axes$x_to_pixel, axes$y_to_pixel));
            }
        }
        return (svg_body)
    }
}

vvplot = function(width, height, ..., nrow = 1, ncol = 1, layout = NULL, bgcol = "transparent")
{
    panels = list(...);

    # -- Head / foot --
    svg_head = paste0(
'<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="', width, '" height="', height, '">
<style>
svg { background-color: ', bgcol, ' }
.vv_axis { fill:none; stroke:black; stroke-width: 0.5 }
.vv_tick { fill:none; stroke:black; stroke-width: 0.5 }
.vv_ticklabx { dominant-baseline:hanging; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:10px }
.vv_ticklabx_rot { dominant-baseline:central; text-anchor:end; font-family:Roboto,Arial,Helvetica; font-size:10px }
.vv_ticklaby { dominant-baseline:central; text-anchor:end; font-family:Roboto,Arial,Helvetica; font-size:10px }
.vv_title { dominant-baseline:central; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:10px; font-weight:bold }
.vv_labelx { dominant-baseline:text-after-edge; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:10px; font-weight:bold }
.vv_labely { dominant-baseline:hanging; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:10px; font-weight:bold }
.vv_textann { dominant-baseline:central; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:10px }
.vv_legend { dominant-baseline:central; text-anchor:start; font-family:Roboto,Arial,Helvetica; font-size:10px }
.vv_line { fill:none; stroke:black; stroke-width:0.5 }
.vv_ribbon { fill:black; stroke:none;  }
.vv_textshow { dominant-baseline:baseline; text-anchor:middle; font-family:Roboto,Arial,Helvetica; font-size:8px }
</style>');
    svg_foot = '</svg>';

    # -- Panels --
    svg_body = "";
    row = 0;
    col = 0;
    i = 1;
    for (p in panels)
    {
        if (!is.null(layout)) {
            svg_body = paste(svg_body, p(layout[[i]][1], layout[[i]][2], layout[[i]][3], layout[[i]][4]), sep = "\n");
        } else {
            svg_body = paste(svg_body, p(col * width / ncol, row * height / nrow, width / ncol, height / nrow), sep = "\n");
        }
        i = i + 1;
        col = col + 1;
        if (col >= ncol) {
            row = row + 1;
            col = 0;
            if (row >= nrow) {
                row = 0;
            }
        }
    }

    return (list(svg = paste(svg_head, svg_body, svg_foot, sep = "\n"), w = width, h = height))
}
