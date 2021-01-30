#' @export
db_model <- function(name, conn, ..., .x = list(), .class = character()) {

  tables <- list(...)
  assert_that(tables %all_inherits% "db_table")
  assert_that(is.function(conn))
  assert_that(inherits(conn(), "DBIConnection"))
  assert_that(is.string(name))

  .x$tables <- db_raise_names(tables)
  .x$conn   <- conn
  .x$name   <- name

  new_db_obj(.x, class = c(.class, "db_model"))

}

#' @export
db_name.db_model <- function(x) {
  as.character(glue("{x$name}"))
}

#' @export
print.db_model <- function(x) {
  NextMethod()
  purrr::walk(x$tables, print)
}
