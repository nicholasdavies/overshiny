var overshiny_count = 0;
// General overlay dropdown handler
$(document).on("click", ".overshiny-clicker", function() {
    var id = $(this).data("id");
    Shiny.setInputValue("overshiny_event", {
        id: id,
        what: "dropdown",
        count: overshiny_count++
    }, { priority: "event" });
});

// General overlay remove handler
$(document).on("click", ".overshiny-remove", function() {
    var id = $(this).data("id");
    Shiny.setInputValue("overshiny_event", {
        id: id,
        what: "remove",
        count: overshiny_count++
    }, { priority: "event" });
});

// Close dropdowns when the plot is clicked
$(document).on("click", ".overshiny-display", function(event) {
    if ($(event.target).closest(".overshiny-rect, .overshiny-dropdown").length > 0) {
        return; // Ignore this click
    }
    Shiny.setInputValue("overshiny_event", {
        id: "null",
        what: "defocus",
        count: overshiny_count++
    }, { priority: "event" });
});

// Patch selectize to work with jqui
$(document).on("mousedown", ".selectize-dropdown .option", function(event) {
    event.preventDefault();
    $(this).trigger("click");
});
