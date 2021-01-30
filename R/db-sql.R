#' @export
db_sql <- function(conn, x) {
  UseMethod("db_sql")
}

#' @export
db_sql.PqConnection <- function(conn, x) {
  db_sql_postgres(x, conn)
}
