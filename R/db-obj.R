#' @title
#' Base Database Object
#'
#' @description
#' Base database object inherited by all objects in this package.
#'
#' @export
new_db_obj <- function(x = list(), ..., class = character()) {
  structure(
    x,
    ...,
    class = c(class, "db_obj")
  )
}

#' @export
db_classes.db_obj <- function(x, full = FALSE) {
  class_string <- if (full) {
    paste0(rev(class(x)), collapse = ":")
  } else {
    class(x)[[1]]
  }
  paste0("[", class_string, "] ")
}

#' @export
db_name <- function(x) {
  UseMethod("db_name")
}

#' @export
db_schema <- function(x) {
  UseMethod("db_schema")
}

#' @export
db_classes <- function(x, ...) {
  UseMethod("db_classes")
}

#' @export
db_validate <- function(x, value) {
  UseMethod("db_validate")
}

#' @export
db_generate <- function(x, value) {
  UseMethod("db_generate")
}

#' @export
print.db_obj <- function(x, full = FALSE) {
  cli_text("{db_classes(x, full = full)} {db_name(x)}")
}
