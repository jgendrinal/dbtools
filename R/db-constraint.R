#' @title
#' Database Constraint
#'
#' @name db_constraint
db_constraint <- function(table, constraint) {
  assert_that(!db_name(constraint) %in% names(table$constraints))
  table$constraints[[db_name(constraint)]] <- constraint
  table
}

#' @rdname db_constraint
#' @param name constraint name
new_db_constraint <- function(x = list(),
                              name,
                              ...,
                              class = character()) {

  assert_that(is.string(name))
  x$name <- name

  new_db_obj(x, ..., class = c(class, "db_constraint"))
}

#' @export
db_name.db_constraint <- function(x) {
  x$name
}

#' @export
db_sql_postgres.db_constraint <- function(x, conn, definition) {
  assert_that(nchar(x$name) <= db_max_id_length(conn),
              msg = "Constraint name too long!")
  build_sql(
    sql("CONSTRAINT "), ident(x$name), " ", sql(definition),
    con = conn
  )
}

#' @export
db_dependencies.db_constraint <- function(x) {
  NULL
}
