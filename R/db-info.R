#' @export
db_default_schema <- function(conn) {
  UseMethod("db_default_schema")
}

#' @export
db_default_schema.default <- function(conn) {
  .NotYetImplemented()
}

#' @export
db_default_schema.PqConnection <- function(conn) {
  dbxSelect(conn, "select current_schema()")[["current_schema"]]
}

#' @export
db_max_id_length <- function(conn) {
  UseMethod("db_max_id_length")
}

#' @export
db_max_id_length.default <- function(conn) {
  .NotYetImplemented()
}

#' @export
db_max_id_length.PqConnection <- function(conn) {
  63L
}
