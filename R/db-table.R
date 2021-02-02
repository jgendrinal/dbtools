#' @title
#' Database Table
#'
#' @export
db_table <- function(name,
                     schema = NULL,
                     .x = list(),
                     .class = character()) {

  assert_that(is.string(name))
  .x$name <- ident(name)

  assert_that(is.null(schema) || is.string(schema))
  .x$schema <- if (is.null(schema)) NULL else ident(schema)

  .x$columns <- list()
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

#' @export
db_dependencies.db_table <- function(table) {

}
