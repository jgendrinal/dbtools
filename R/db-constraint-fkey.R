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

  assert_that(inherits(table, "db_table"))
  assert_that(inherits(foreign_table, "db_table"))

  column <- db_select(table, {{ column }})[[1]]
  foreign_column <- if (rlang::quo_text(enexpr(foreign_column)) == "NULL") {
    db_select(foreign_table, db_name(column))[[1]]
  } else {
    db_select(foreign_table, {{ foreign_column }})[[1]]
  }

  assert_that(inherits(column, "db_column"))
  assert_that(inherits(foreign_column, "db_column"))

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

  .x$table <- table
  .x$foreign_table <- foreign_table
  .x$column <- column
  .x$foreign_column <- foreign_column

  on_update <- arg_match(on_update)
  assert_that(is.string(on_update))
  .x$on_update <- on_update

  on_delete <- arg_match(on_delete)
  assert_that(is.string(on_delete))
  .x$on_delete <- on_delete

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
