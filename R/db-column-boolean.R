#' @title
#' Boolean Column
#'
#' @export
db_column_boolean <- function(name,
                              default  = NULL,
                              validate = function(value) {},
                              nullable = TRUE,
                              .x       = list(),
                              .class   = character()) {
  new_db_column(
    x        = .x,
    name     = name,
    default  = {{ default }},
    validate = validate,
    nullable = nullable,
    class    = c(.class, "db_column_boolean")
  )
}

#' @export
db_validate.db_column_boolean <- function(x, value) {
  validate_set(
    validate_that(is.logical(value)),
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_boolean <- function(x, conn) {
  NextMethod(data_type = "BOOLEAN")
}
