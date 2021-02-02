#' @title
#' UUID Column
#'
#' @description
#' A Universally Unique Identifier column
#'
#' @export
db_column_uuid <- function(table,
                           name,
                           default  = NULL,
                           validate = function(value) {},
                           nullable = TRUE) {
  db_column(
    table  = table,
    column = new_db_column_uuid(
      name     = name,
      default  = {{ default }},
      validate = validate,
      nullable = nullable
    )
  )
}

new_db_column_uuid <- function(name,
                               default  = NULL,
                               validate = function(value) {},
                               nullable = TRUE,
                               .x       = list(),
                               .class   = character()) {
  new_db_column_text(
    .x       = .x,
    max_char = 36L,
    name     = name,
    default  = {{ default }},
    validate = validate,
    nullable = nullable,
    .class   = c(.class, "db_column_uuid")
  )
}

#' @export
db_validate.db_column_uuid <- function(x, value) {
  uuid_regex <-
    "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}"
  validate_set(
    validate_that(all(stringr::str_detect(value, uuid_regex)),
                  msg = "UUIDs must conform to general standard"),
    NextMethod()
  )
}

#' @export
db_generate.db_column_uuid <- function(x, value) {
  value <- coalesce(value, map_chr(value, ~uuid::UUIDgenerate()))
  NextMethod()
}

#' @export
db_sql_postgres.db_column_uuid <- function(x, conn) {
  NextMethod(data_type = "UUID")
}
