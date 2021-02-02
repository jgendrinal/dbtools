#' @export
db_model <- function(name, ..., .x = list(), .class = character()) {

  tables <- db_eval(...)
  assert_that(tables %all_inherits% "db_table")
  assert_that(is.string(name))

  .x$tables <- tables
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
