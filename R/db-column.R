#' @title
#' Database Column
#'
#' @description
#' These constructors represent database columns and allow us to specify the
#' structure of a database table.
#'
#' @name db_column
db_column <- function(table, column) {
  assert_that(inherits(table, "db_table"))
  assert_that(!db_name(column) %in% names(table$column))
  table$columns[[db_name(column)]] <- column
  table
}

#' @rdname db_column
#' @param name     name of the column, as string
#' @param default  default value of the column in expression
#' @param validate function that takes the value as an argument, returns
#'                 TRUE or NULL if the provided data is valid, throws an error
#'                 or returns anything other than a TRUE value.
#' @param nullable whether or not this column is nullable, default TRUE
#' @param class    sub-classes as character string
#' @param x        base type (list)
#' @param ...      extra attributes
new_db_column <- function(x = list(),
                          name,
                          default  = NULL,
                          validate = function(value) {},
                          nullable = TRUE,
                          ...,
                          class = character()) {

  assert_that(is.string(name))
  x$name <- ident(name)

  x$default <- if (quo_text(enexpr(default)) == "NULL") {
    default
  } else {
    enexpr(default)
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

#' @export
db_sql_postgres.db_column <- function(x, conn, data_type, ...) {
  assert_that(is.string(data_type))
  assert_that(nchar(x$name) <= db_max_id_length(conn),
              msg = "Column name is too long!")
  build_sql(
    x$name, " ", sql(data_type),
    if (!x$nullable) sql(" NOT NULL"),
    if (!is.null(x$default)) build_sql(
      " DEFAULT ",
      sql_expr(!!x$default[[2]], con = conn),
      con = conn
    ),
    con = conn
  )
}

#' @export
db_dependencies.db_column <- function(x) {
  NULL
}
