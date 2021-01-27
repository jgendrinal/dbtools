#' @title
#' Database Index based on a SQL expression
#'
#' @param table      database table
#' @param expression unquoted expression that will form the index
#' @param name       name of the index
#'
#' @export
db_index_expression <- function(table,
                                expression,
                                unique  = FALSE,
                                partial = NULL,
                                name    = NULL) {

  assert_that(inherits(table, "db_table"))

  db_index(
    table = table,
    index = new_db_index_expression(
      table      = table,
      expression = expression,
      name       = name,
      unique     = unique,
      partial    = partial
    )
  )

}

new_db_index_expression <- function(table,
                                    expression,
                                    name    = NULL,
                                    unique  = FALSE,
                                    partial = NULL,
                                    .x      = list(),
                                    .class  = character()) {


  expression <- enexpr(expression)
  assert_that(is_expression(expression))
  .x$expression <- expression

  name <- name %||% {
    table_name <- db_name(table)
    expression_hash <- digest(expression)
    glue("{table_name}-{expression_hash}-index")
  }
  assert_that(is.string(name))
  .x$name <- ident(name)

  new_db_index(
    x       = .x,
    name    = name,
    unique  = unique,
    partial = partial,
    class   = c(.class, "db_index_expression")
  )

}
