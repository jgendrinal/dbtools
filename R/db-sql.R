#' @export
db_sql <- function(conn, x) {
  UseMethod("db_sql")
}

#' @export
db_sql.DBIConnection <- function(conn, x, sql) {
  structure(sql, class = c("db_sql", class(sql)))
}

#' @export
db_sql.PqConnection <- function(conn, x) {
  NextMethod(sql = db_sql_postgres(x, conn))
}

#' @export
#' @importFrom stringr str_replace_all
format.db_sql <- function(x, ...) {
  NextMethod(
    x = x %>%
      str_replace_all("\\(", "\\(\n  ") %>%
      str_replace_all("\\)", "\n\\)") %>%
      str_replace_all(",[ ]*", ",\n  "),
    ...
  )
}
