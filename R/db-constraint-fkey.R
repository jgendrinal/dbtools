#' @title
#' Foreign Key Constraint
#'
#' @param table  database table
#' @param column table column to be used
#' @param foreign_table  foreign database table
#' @param foreign_column foreign column
#' @param on_delete setting for when the foreign key is deleted
#' @param on_update setting for when the foreign key is updated
#' @param name   name of the constraint
#'
#' @export
db_constraint_fkey <- function(
  table,
  foreign_table,
  column,
  foreign_column = NULL,
  on_update = c("no action", "set null", "restrict", "set default", "cascade"),
  on_delete = c("no action", "set null", "restrict", "set default", "cascade"),
  name = NULL
) {

  column <- db_select(table, {{ column }})
  foreign_column <- if (quo_text(enexpr(foreign_column)) == "NULL") {
    db_select(foreign_table, map_chr(column, db_name))
  } else {
    db_select(foreign_table, {{ foreign_column }})
  }

  db_constraint(
    table      = table,
    constraint = new_db_constraint_fkey(
      table          = table,
      foreign_table  = foreign_table,
      column         = column,
      foreign_column = foreign_column,
      on_update      = on_update,
      on_delete      = on_delete,
      name           = name
    )
  )

}

new_db_constraint_fkey <- function(
  table,
  foreign_table,
  column,
  foreign_column,
  on_update = c("no action", "set null", "restrict", "set default", "cascade"),
  on_delete = c("no action", "set null", "restrict", "set default", "cascade"),
  name = NULL,
  .x = list(),
  .class = character()
) {

  assert_that(inherits(foreign_table, "db_table"))

  .x$foreign_table <- foreign_table
  .x$column <- column
  .x$foreign_column <- foreign_column

  on_update <- arg_match(on_update)
  assert_that(is.string(on_update))
  .x$on_update <- on_update

  on_delete <- arg_match(on_delete)
  assert_that(is.string(on_delete))
  .x$on_delete <- on_delete

  assert_that(column %all_inherits% "db_column")
  assert_that(foreign_column %all_inherits% "db_column")

  name <- name %||% glue(
    "{db_name(table)}_{db_name(column)}-",
    "{db_name(foreign_table)}_{db_name(foreign_column)}-",
    "fkey"
  )
  assert_that(is.string(name))
  .x$name <- ident(name)

  new_db_constraint(
    x     = .x,
    name  = name,
    class = c(.class, "db_constraint_fkey")
  )

}

#' @export
db_sql_postgres.db_constraint_fkey <- function(x, conn) {
  NextMethod(
    definition = build_sql(
      sql("FOREIGN KEY "), db_sql_postgres(x$column, conn),
      sql(" REFERENCES "), db_sql_postgres(x$foreign_table, conn), " ",
      db_sql_postgres(x$foreign_column, conn),
      con = conn
    )
  )
}
