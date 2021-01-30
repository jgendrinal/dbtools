#' @export
db_sql_postgres <- function(x, conn, ...) {
  UseMethod("db_sql_postgres")
}

#' @export
db_sql_postgres.default <-function(x, conn) {
  .NotYetImplemented()
}

#' @export
db_sql_postgres.db_migration_create <- function(x, conn, ...) {
  db_sql_postgres_create(x$new_obj, conn)
}

#' @export
db_sql_postgres_create <- function(x, conn) {
  UseMethod("db_sql_postgres_create")
}

#' @export
db_sql_postgres_create.db_table <- function(x, conn) {
  build_sql(
    sql("CREATE TABLE "), db_sql_postgres(x, conn), " (\n",
    sql(glue_collapse(
      map_chr(x$columns, db_sql_postgres, conn = conn),
      sep = ",\n"
    )),
    "\n);",
    con = conn
  )
}

#' @export
db_sql_postgres.list <- function(x, conn) {
  if (x %all_inherits% "db_column") {
    build_sql(
      sql(glue_collapse(map_chr(x, "name"), sep = ', ')),
      conn = con
    )
  } else {
    NextMethod()
  }
}
