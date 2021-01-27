#' @title
#' Database Index based on a column or columns
#'
#' @param ...   unquoted column names subject to the index
#'
#' @export
db_index_column <- function(table,
                            ...,
                            unique  = FALSE,
                            partial = NULL,
                            name    = NULL) {

  assert_that(inherits(table, "db_table"))

  db_index(
    table = table,
    index = new_db_index_column(
      table   = table,
      columns = db_select(table, ...),
      name    = name,
      unique  = unique,
      partial = partial
    )
  )

}

new_db_index_column <- function(table,
                                columns,
                                name    = NULL,
                                unique  = FALSE,
                                partial = NULL,
                                .x      = list(),
                                .class  = character()) {

  name <- name %||% {
    table_name   <- db_name(table)
    column_names <- paste0(map_chr(columns, db_name), collapse = "_")
    glue("{table_name}-{column_names}-index")
  }
  assert_that(is.string(name))
  .x$name <- ident(name)

  assert_that(columns %all_inherits% "db_column")
  .x$columns <- columns

  new_db_index(
    x       = .x,
    name    = name,
    unique  = unique,
    partial = partial,
    class   = c(.class, "db_index_columns")
  )

}
