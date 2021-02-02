#' @title
#' Text Column
#'
#' @param max_char maximum length
#' @export
db_column_text <- function(table,
                           name,
                           default  = NULL,
                           max_char = Inf,
                           validate = function(value) {},
                           nullable = TRUE) {
  db_column(
    table  = table,
    column = new_db_column_text(
      name     = name,
      default  = {{ default }},
      max_char = max_char,
      validate = validate,
      nullable = nullable
    )
  )
}

new_db_column_text <- function(name,
                               default  = NULL,
                               max_char = Inf,
                               validate = function(value) {},
                               nullable = TRUE,
                               .x       = list(),
                               .class   = character()) {
  assert_that(is.number(max_char) && max_char >= 1,
              msg = "Maximum characters should be a positive integer.")
  .x$max_char <- max_char
  new_db_column(
    x        = .x,
    name     = name,
    default  = {{ default }},
    validate = validate,
    nullable = nullable,
    class    = c(.class, "db_column_text")
  )
}

#' @export
db_validate.db_column_text <- function(x, value) {
  validate_set(
    validate_that(is.character(value)),
    validate_that(is.infinite(x$max_char) || all(nchar(value) <= x$max_char),
                  msg = "Maximum character length breached."),
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_text <- function(x, conn, data_type = NULL) {
  data_type <- data_type %||%
    if (is.infinite(x$max_char)) {
      "TEXT"
    } else {
      glue("VARCHAR({x$max_char})")
    }
  NextMethod(data_type = data_type)
}
