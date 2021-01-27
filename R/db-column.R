#' @title
#' Database Column
#'
#' @description
#' These constructors represent database columns and allow us to specify the
#' structure of a database table.
#'
#' @name db_column
NULL

#' @rdname db_column
#' @param name     name of the column, as string
#' @param default  default value of the column in SQL code, default no default
#' @param validate function that takes the value as an argument, returns
#'                 TRUE or NULL if the provided data is valid, throws an error
#'                 or returns anything other than a TRUE value.
#' @param nullable whether or not this column is nullable, default TRUE
#' @param class    sub-classes as character string
#' @param x        base type (list)
#' @param ...      extra attributes
#' @export
new_db_column <- function(x = list(),
                          name,
                          default  = NULL,
                          validate = function(value) {},
                          nullable = TRUE,
                          ...,
                          class = character()) {

  assert_that(is.string(name))
  x$name <- ident(name)

  if (!is.null(default)) {
    assert_that(is.string(default))
    x$default <- sql(default)
  } else {
    x$default <- default
  }

  assert_that(is.function(validate) && validate %has_args% "value")
  x$validate <- validate

  assert_that(is.flag(nullable))
  x$nullable <- nullable

  new_db_obj(x, class = c(class, "db_column"))

}

#' @export
db_name.db_column <- function(x) {
  as.character(x$name)
}

#' @export
db_validate.db_column <- function(x, value) {
  validate_function(x$validate, value)
}

#' @export
db_generate.db_column <- function(x, value) {
  value
}
