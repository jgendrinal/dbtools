#' @export
db_sql_postgres <- function(x, conn, ...) {
  UseMethod("db_sql_postgres")
}

#' @export
db_sql_postgres.default <-function(x, conn, ...) {
  .NotYetImplemented()
}

#' @export
db_sql_postgres.db_migration_create <- function(x, conn, ...) {
  db_sql_postgres_create(x$new_obj, conn)
}

#' @export
db_sql_postgres_create <- function(x, conn, ...) {
  UseMethod("db_sql_postgres_create")
}

#' @export
db_sql_postgres_create.db_table <- function(x, conn) {
  c(
    build_sql(
      sql("CREATE TABLE "), db_sql_postgres(x, conn), " ",
      append(
        unname(map(x$columns, db_sql_postgres, conn = conn)),
        unname(map(x$constraints, db_sql_postgres, conn = conn))
      ),
      con = conn
    ),
    sql(unname(map_chr(
      x$indexes,
      db_sql_postgres_create,
      conn = conn,
      table = x
    )))
  )
}

#' @export
db_sql_postgres_create.db_index <- function(x, conn, table) {
  build_sql(
    sql("CREATE "), db_sql_postgres(x, conn, table),
    con = conn
  )
}

#' @export
db_sql_postgres.list <- function(x, conn) {
  if (x %all_inherits% "db_column") {
    build_sql(unname(map(x, "name")), con = conn)
  } else {
    stop("Unsupported list")
  }
}

