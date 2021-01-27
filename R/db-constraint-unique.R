#' @title
#' Primary Key Constraint
#'
#' @param table database table
#' @param ...   unquoted column_names subject to the primary key constraint
#' @param name  name of the constraint
#'
#' @export
db_constraint_unique <- function(table,
                                 ...,
                                 name = NULL) {

  assert_that(inherits(table, "db_table"))

  db_constraint(
    table      = table,
    constraint = new_db_constraint_unique(
      table   = table,
      columns = db_select(table, ...),
      name    = name
    )
  )

}

new_db_constraint_unique <- function(table, columns, name = NULL,
                                   .x = list(),
                                   .class = character()) {

  name <- name %||% {
    table_name   <- db_name(table)
    column_names <- paste0(map_chr(columns, db_name), collapse = "_")
    glue("{table_name}-{column_names}-unique")
  }
  assert_that(is.string(name))
  .x$name <- ident(name)

  new_db_constraint(
    x     = .x,
    name  = name,
    class = c(.class, "db_constraint_unique")
  )

}