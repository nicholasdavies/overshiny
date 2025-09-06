test_that("panel_rects_ggplot works", {
    # suppress creation of Rplots.pdf
    pdf(NULL)

    plot <- ggplot2::ggplot(mtcars) +
        ggplot2::geom_point(ggplot2::aes(x = mpg, y = disp)) +
        ggplot2::facet_wrap(~cyl)
    rects <- overshiny:::panel_rects_ggplot(plot)

    expect_equal(rects$panel_id, c(1, 2, 3))
    expect_equal(rects$row, c(1, 1, 1))
    expect_equal(rects$col, c(1, 2, 3))
    expect_equal(rects$label, c('4', '6', '8'))
    expect_condition(rects$label$x[3] > rects$label$x[2] &&
            rects$label$x[2] > rects$label$x[1])

    expect_lte(rects$xmin[1], 10.4)
    expect_gte(rects$xmax[1], 33.9)
    expect_lte(rects$ymin[1], 71.1)
    expect_gte(rects$ymax[1], 472)

    expect_equal(rects$xmin[1], rects$xmin[2])
    expect_equal(rects$xmin[2], rects$xmin[3])
    expect_equal(rects$ymin[1], rects$ymin[2])
    expect_equal(rects$ymin[2], rects$ymin[3])
})
