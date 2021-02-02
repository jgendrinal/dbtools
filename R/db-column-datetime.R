#' @title
#' Date-Time (Timestamp) Column
#'
#' @export
db_column_datetime <- function(table,
                               name,
                               default  = NULL,
                               validate = function(value) {},
                               nullable = TRUE) {
  db_column(
    table  = table,
    column = new_db_column_datetime(
      name     = name,
      default  = {{ default }},
      validate = validate,
      nullable = nullable
    )
  )

}

new_db_column_datetime <- function(name,
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
    class    = c(.class, "db_column_datetime")
  )
}

#' @export
db_validate.db_column_datetime <- function(x, value) {
  validate_set(
    validate_that(is.POSIXct(value)),
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_datetime <- function(x, conn) {
  NextMethod(data_type = "TIMESTAMP WITH TIME ZONE")
}
