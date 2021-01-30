#' @title
#' JSON Column
#'
#' @export
db_column_json <- function(name,
                           binary   = TRUE,
                           default  = NULL,
                           validate = function(value) {},
                           nullable = TRUE,
                           .x       = list(),
                           .class   = character()) {
  assert_that(is.flag(binary))
  .x$binary <- binary
  new_db_column(
    x        = .x,
    name     = name,
    default  = {{ default }},
    validate = validate,
    nullable = nullable,
    class    = c(.class, "db_column_json")
  )
}

#' @export
db_validate.db_column_json <- function(x, value) {
  validate_set(
    validate_that(is.list(value)),
    validate_that(is.null(value) || value %all_inherits% "json"),
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_json <- function(x, conn) {
  NextMethod(data_type = if (x$binary) "JSONB" else "JSON")
}
