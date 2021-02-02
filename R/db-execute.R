#' @export
db_execute <- function(x, conn) {
  UseMethod("db_execute")
}

#' @export
db_execute.sql <- function(x, conn) {
  map(x, ~dbxExecute(conn = conn, statement = .))
}
