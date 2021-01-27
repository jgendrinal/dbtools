#' @title
#' UUID Column
#'
#' @description
#' A Universally Unique Identifier column
#'
#' @export
db_column_uuid <- function(name,
                           default  = NULL,
                           validate = function(value) {},
                           nullable = TRUE,
                           .x       = list(),
                           .class   = character()) {
  db_column_text(
    .x       = .x,
    max_char = 36L,
    name     = name,
    default  = default,
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
    NextMethod(x, value)
  )
}

db_generate.db_column_uuid <- function(x, value) {
  value <- coalesce(value, map_chr(value, ~uuid::UUIDgenerate()))
  NextMethod(x, value)
}
