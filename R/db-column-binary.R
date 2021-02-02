#' @title
#' Binary Column
#'
#' @export
db_column_binary <- function(table,
                             name,
                             default  = NULL,
                             validate = function(value) {},
                             nullable = TRUE) {
  db_column(
    table  = table,
    column = new_db_column_binary(
      name     = name,
      default  = {{ default }},
      validate = validate,
      nullable = nullable,
    )
  )
}

new_db_column_binary <- function(name,
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
    class    = c(.class, "db_column_binary")
  )
}

#' @export
db_validate.db_column_binary <- function(x, value) {
  validate_set(
    validate_that(is.list(value)),
    validate_that(is.null(value) || value %all_inherits% "raw"),
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_binary <- function(x, conn) {
  NextMethod(data_type = "BYTEA")
}
