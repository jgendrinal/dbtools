#' @title
#' Double Precision Column
#'
#' @export
db_column_double <- function(table,
                             name,
                             default  = NULL,
                             validate = function(value) {},
                             nullable = TRUE) {
  db_column(
    table  = table,
    column = new_db_column_double(
      name     = name,
      default  = {{ default }},
      validate = validate,
      nullable = nullable
    )
  )
}

new_db_column_double <- function(name,
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
    class    = c(.class, "db_column_double")
  )
}

#' @export
db_validate.db_column_double <- function(x, value) {
  validate_set(
    validate_that(is.double(value)),
    NextMethod()
  )
}
#' @export
db_sql_postgres.db_column_double <- function(x, conn) {
  NextMethod(data_type = "DOUBLE PRECISION")
}
