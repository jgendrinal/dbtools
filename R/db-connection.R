db_connection_factory <- function(slug,
                                  adapter = "postgres",
                                  host = NULL,
                                  user = NULL,
                                  pass = NULL,
                                  name = NULL,
                                  ...) {
  host <- host %||% Sys.getenv(glue("{toupper(slug)}_DB_HOST"))
  db_connection <- memoise(function() {
    dbxConnect(
      adapter  = adapter,
      host     = host,
      user     = user %||% Sys.getenv(glue("{toupper(slug)}_DB_USER")),
      password = pass %||% Sys.getenv(glue("{toupper(slug)}_DB_PASS")),
      dbname   = name %||% Sys.getenv(glue("{toupper(slug)}_DB_NAME")),
      ...
    )
  })
  function() {
    test <- try({dbGetQuery(db_connection(), "select 1")})
    if (inherits(test, "try-error")) {
      warn(glue("Connection to {host} was refreshed as it was invalid"))
      forget(db_connection)
    } else {
      db_connection()
    }
  }
}

db_tjpalanca <- db_connection_factory("tjpalanca")
