#' @title
#' Database Table
#'
#' @export
db_table <- function(name,
                     ...,
                     schema = NULL,
                     .x = list(),
                     .class = character()) {

  assert_that(is.string(name))
  .x$name <- ident(name)

  assert_that(is.null(schema) || is.string(schema))
  .x$schema <- if (is.null(schema)) NULL else ident(schema)

  columns <- list(...)
  assert_that(columns %all_inherits% "db_column")
  .x$columns <- db_raise_names(columns)

  .x$constraints <- list()
  .x$indexes <- list()

  new_db_obj(
    x = .x,
    class = c(.class, "db_table")
  )

}

#' @export
db_name.db_table <- function(x) {
  paste0(glue(x$schema, "."), x$name)
}

#' @export
print.db_table <- function(x) {
  NextMethod()
  purrr::walk(x$columns, print)
  purrr::walk(x$constraints, print)
  purrr::walk(x$indexes, print)
}

#' @export
db_table_add_schema <- function(table, schema) {
  assert_that(is.null(schema) || is.string(schema))
  table$schema <- if (is.null(schema)) NULL else ident(schema)
  table
}

#' @export
db_sql_postgres.db_table <- function(x, conn) {
  assert_that(is.null(x$schema) || nchar(x$schema) <= db_max_id_length(conn),
              msg = "Schema name is too long!")
  assert_that(nchar(x$name) <= db_max_id_length(conn),
              msg = "Table name is too long!")
  build_sql(
    ident(x$schema),
    if (!is.null(x$schema)) ".",
    x$name,
    con = conn
  )
}
