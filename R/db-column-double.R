#' @title
#' Double Precision Column
#'
#' @export
db_column_double <- function(name,
                             default  = NULL,
                             validate = function(value) {},
                             nullable = TRUE,
                             .x       = list(),
                             .class   = character()) {
  new_db_column(
    x        = .x,
    name     = name,
    default  = default,
    validate = validate,
    nullable = nullable,
    class    = c(.class, "db_column_double")
  )
}

#' @export
db_validate.db_column_double <- function(x, value) {
  validate_set(
    validate_that(is.double(value)),
    NextMethod(x, value)
  )
}
