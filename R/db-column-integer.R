#' @title
#' Integer Column
#'
#' @param integer integers will be bound within the range [-intrange, +intrange]
#' @param identity is this an identity column? Can be:
#' * `"none"`    - not an identity column
#' * `"default"` - can be overwritten on insert
#' * `"always"`  - can not be overwritten
#' @export
db_column_integer <- function(name,
                              default  = NULL,
                              intrange = 2147483647L,
                              validate = function(value) {},
                              nullable = TRUE,
                              identity = c("none", "by default", "always"),
                              .x       = list(),
                              .class   = character()) {
  assert_that(
    is.number(intrange) && intrange >= 1L && (
      is.integer(intrange) || is.integer64(intrange)
    ),
    msg = "Integer range should be a positive integer"
  )
  assert_that(intrange <= as.integer64(9223372036854774784),
              msg = "Maximum integer is 9223372036854774784")
  identity <- arg_match(identity)
  assert_that(is.string(identity))
  .x$identity <- identity
  .x$intrange <- intrange
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
    validate_that(all(value >= -x$intrange), all(value <=  x$intrange),
                  msg = "Integer overflow"),
    if (x$identity != "none") {
      validate_that(n_distinct(value) == length(value),
                    msg = "Integers must be unique due to identity constraint")
    },
    NextMethod()
  )
}

#' @export
db_sql_postgres.db_column_integer <- function(x, conn) {
  data_type <- if (x$intrange <= 32767L) {
    "SMALLINT"
  } else if (x$intrange <= 2147483647L) {
    "INTEGER"
  } else {
    "BIGINT"
  }
  if (x$identity != "none") {
    data_type <- glue("{data_type} GENERATED {toupper(x$identity)} AS IDENTITY")
  }
  NextMethod(data_type = data_type)
}
