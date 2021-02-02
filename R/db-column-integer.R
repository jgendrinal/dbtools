#' @title
#' Integer Column
#'
#' @param identity is this an identity column? Can be:
#' * `"none"`    - not an identity column
#' * `"default"` - can be overwritten on insert
#' * `"always"`  - can not be overwritten
#' @export
db_column_integer <- function(table,
                              name,
                              default  = NULL,
                              int_type = c("integer", "smallint", "bigint"),
                              validate = function(value) {},
                              nullable = TRUE,
                              identity = c("none", "by default", "always")) {
  db_column(
    table  = table,
    column = new_db_column_integer(
      name     = name,
      default  = {{ default }},
      int_type = int_type,
      validate = validate,
      nullable = nullable,
      identity = identity
    )
  )
}

new_db_column_integer <- function(name,
                                  default  = NULL,
                                  int_type = c("integer", "smallint", "bigint"),
                                  validate = function(value) {},
                                  nullable = TRUE,
                                  identity = c("none", "by default", "always"),
                                  .x       = list(),
                                  .class   = character()) {

  .x$identity <- arg_match(identity)
  .x$int_type <- arg_match(int_type)

  new_db_column(
    x        = .x,
    name     = name,
    default  = {{ default }},
    validate = validate,
    nullable = nullable,
    class    = c(.class, "db_column_integer")
  )
}

#' @export
db_validate.db_column_integer <- function(x, value) {
  validate_set(
    validate_that(is.integer(value) || is.integer64(value)),
    validate_that(
      abs(value) <= switch(
        x$int_type,
        "smallint" = 32767L,
        "integer"  = 2147483647L,
        "bigint"   = as.integer64(9223372036854774784)
      ),
      msg = "Integer is beyond the maximum range for the field"
    ),
    if (x$identity != "none") {
      validate_that(n_distinct(value) == length(value),
                    msg = "Integers must be unique due to identity constraint")
    },
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_integer <- function(x, conn) {
  data_type <- toupper(x$int_type)
  if (x$identity != "none") {
    data_type <- glue("{data_type} GENERATED {toupper(x$identity)} AS IDENTITY")
  }
  NextMethod(data_type = data_type)
}
