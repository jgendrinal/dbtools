#' @title
#' Primary Key Constraint
#'
#' @param table database table
#' @param check check expression to be checked against new inputs
#' @param name  name of the constraint
#'
#' @export
db_constraint_check <- function(table,
                                check,
                                name = NULL) {

  assert_that(inherits(table, "db_table"))

  db_constraint(
    table      = table,
    constraint = new_db_constraint_check(table, {{ check }}, name)
  )

}

new_db_constraint_check <- function(table, check, name = NULL,
                                    .x = list(),
                                    .class = character()) {

  check <- enexpr(check)
  assert_that(is_expression(check))
  .x$check <- check

  name <- name %||% {
    table_name <- db_name(table)
    check_hash<- digest(check)
    glue("{table_name}-{check_hash}-check")
  }
  assert_that(is.string(name))
  .x$name <- ident(name)

  new_db_constraint(
    x     = .x,
    name  = name,
    class = c(.class, "db_constraint_check")
  )

}
