# For communicating position and size of intervention rectangles to server code
shsize = list(
    # Actual handling of size
    position = list(
        `dragstop` = htmlwidgets::JS('function(event, ui) {
            Shiny.setInputValue("overshiny_event", {
                id: event.target.id,
                what: "move",
                x: ui.position.left
            }, { priority: "event" })
        }'),
        `drag dragcreate` = htmlwidgets::JS("function() { }")
    ),
    size = list(
        `resizestop` = htmlwidgets::JS('function(event, ui) {
            Shiny.setInputValue("overshiny_event", {
                id: event.target.id,
                what: "resize",
                x: ui.position.left,
                w: ui.size.width
            }, { priority: "event" })
        }'),
        `resize resizecreate` = htmlwidgets::JS("function() { }")
    ),
    # These cancel refiring of position/size events while drag/resize is underway.
    is_dragging = list(
        `drag dragcreate dragstop` = htmlwidgets::JS("function() { }")
    ),
    is_resizing = list(
        `resize resizecreate resizestop` = htmlwidgets::JS("function() { }")
    )
)

# For dropping tokens onto overlay display
shdrop = list(
    add = list(
        `drop` = htmlwidgets::JS('function(event, ui) {
            return {
                x: ui.helper.offset().left, /*ui.position.left,*/
                y: ui.helper.offset().top, /*ui.position.top,*/
                id: $(ui.draggable).attr("id"),
                label: $(ui.draggable).data("label"),
                count: overshiny_count++
            };
        }')
    )
)

# TODO Plot resize
# function observePlotResize(id, delay = 200) {
#     var el = document.getElementById(id);
#     if (!el) return;
#
#     let timeoutId = null;
#
#     new ResizeObserver(entries => {
#         for (let entry of entries) {
#             if (timeoutId) clearTimeout(timeoutId);
#             timeoutId = setTimeout(() => {
#                 const { width, height } = entry.contentRect;
#                 Shiny.setInputValue("overshiny_event", {
#                     id: "null",
#                     what: "plot_size",
#                     plotId: id,
#                     width: width,
#                     height: height
#                 });
#             }, delay);
#         }
#     }).observe(el);
# }
