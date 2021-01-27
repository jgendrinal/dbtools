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
#' @export
new_db_index <- function(x = list(),
                         name,
                         ...,
                         unique  = FALSE,
                         partial = NULL,
                         class   = character()) {

  assert_that(is.string(name))
  x$name <- name

  assert_that(is.flag(unique))
  x$unique <- unique

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
