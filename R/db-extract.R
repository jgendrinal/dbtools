#' @export
db_extract <- function(x, ...) {
  UseMethod("db_extract")
}

#' @export
db_extract.default <- function(x, ...) {
  .NotYetImplemented()
}

#' @export
db_extract.PqConnection <- function(conn, tables) {
  system_schemas <- c("pg_catalog", "information_schema")

  existing_tables <-
    tbl(conn, in_schema("information_schema", "tables")) %>%
    filter(!table_schema %in% system_schemas) %>%
    collect(n = Inf)

  # existing_indexes <-
  tbl(conn, "pg_indexes") %>%
    filter(!schemaname %in% system_schemas) %>%
    collect(n = Inf)

  tbl()

}
