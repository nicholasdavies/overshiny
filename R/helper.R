nbsp = function(text)
{
    stringr::str_replace_all(text, c(" " = "&nbsp;", "-" = "&#8209;"))
}

ovid = function(...)
{
    paste("overshiny", ..., sep = "_")
}

ovsel = function(...)
{
    paste0("#", ovid(...))
}

ovmatch = function(...)
{
    paste0("[id^='", ovid(...), "']")
}

# insert ui into element #id
insert_ui = function(id, ui)
{
    shiny::insertUI(paste0("#", id), where = "beforeEnd",
        ui = ui, immediate = TRUE)
}

# clear out any children of element #id
clear_ui = function(id)
{
    shiny::removeUI(paste0("#", id, " *"), immediate = TRUE)
}

# Helper to change CSS
setcss = function(id, ...)
{
    arguments = list(...);
    n = names(arguments);
    v = sapply(arguments, function(x) { if (is.numeric(x)) paste0(x, "px") else x });
    nv = paste0('"', n, '" : "', v, '"', collapse = ", ");
    if (grepl("^[[:alnum:]]", id)) {
        id = paste0("#", id)
    }
    command = paste0('$("', id, '").css({ ', nv, ' });');
    shinyjs::runjs(command);
}
