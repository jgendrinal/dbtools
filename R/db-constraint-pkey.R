#' @title
#' Primary Key Constraint
#'
#' @param table database table
#' @param ...   unquoted column_names subject to the primary key constraint
#' @param name  name of the constraint
#'
#' @export
db_constraint_pkey <- function(table,
                               ...,
                               name = NULL) {

  assert_that(inherits(table, "db_table"))
  assert_that(table$constraints %none_inherits% "db_constraint_pkey",
              msg = "Primary Key Constraint already exists!")

  db_constraint(
    table      = table,
    constraint = new_db_constraint_pkey(
      table   = table,
      columns = db_select(table, ...),
      name    = name
    )
  )

}

new_db_constraint_pkey <- function(table, columns, name = NULL,
                                   .x = list(),
                                   .class = character()) {

  assert_that(columns %all_inherits% "db_column")
  .x$columns <- columns

  name <- name %||% glue("{db_name(table)}-{db_name(columns)}-pkey")

  assert_that(is.string(name))
  .x$name <- ident(name)

  new_db_constraint(
    x     = .x,
    name  = name,
    class = c(.class, "db_constraint_pkey")
  )

}

#' @export
db_sql_postgres.db_constraint_pkey <- function(x, conn) {
  NextMethod(
    definition = build_sql(
      sql("PRIMARY KEY "), db_sql_postgres(x$columns, conn),
      con = conn
    )
  )
}
