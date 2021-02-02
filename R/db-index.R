#' @title
#' Database Index
#'
#' @name db_index
db_index <- function(table, index) {
  assert_that(!db_name(index) %in% names(table$indexes))
  table$indexes[[db_name(index)]] <- index
  table
}

#' @rdname db_index
#' @param name    index name
#' @param unique  whether or not index must be on unique values
#' @param partial if partial index, R expression that defines the bounds of the
#'                index, but NULL if not partial
new_db_index <- function(x = list(),
                         name,
                         ...,
                         unique  = FALSE,
                         partial = NULL,
                         class   = character()) {

  assert_that(is.string(name))
  assert_that(is.flag(unique))

  x$name    <- ident(name)
  x$unique  <- unique
  x$partial <- enexpr(partial)

  new_db_obj(
    x,
    ...,
    class = c(class, "db_index")
  )
}

#' @export
db_name.db_index <- function(x) {
  as.character(x$name)
}

#' @export
db_sql_postgres.db_index <- function(x, conn, definition, table, ...) {
  build_sql(
    if (x$unique) sql("UNIQUE INDEX ") else sql("INDEX "),
    x$name, sql(" ON "), db_sql_postgres(table, conn), " ",
    sql(definition),
    if (quo_text(x$partial) != "NULL") {
      build_sql(
        sql(" WHERE "),
        sql_expr(!!x$partial[[2]], con = conn),
        con = conn
      )
    },
    con = conn
  )
}
