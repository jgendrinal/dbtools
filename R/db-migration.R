new_db_migration <- function(x = list(), ..., class = character()) {
  new_db_obj(x, ..., class = c(class, "db_migration"))
}

db_migration_create <- function(new_obj) {
  assert_that(inherits(new_obj, "db_obj"))
  new_db_migration(
    x = list(
      new_obj = new_obj
    ),
    class = "db_migration_create"
  )
}

db_migration_update <- function(old_obj, new_obj) {
  assert_that(inherits(old_obj, "db_obj"))
  assert_that(inherits(new_obj, "db_obj"))
  new_db_migration(
    x = list(
      new_obj = new_obj,
      old_obj = old_obj
    ),
    class = "db_migration_update"
  )
}

db_migration_delete <- function(old_obj) {
  assert_that(inherits(old_obj, "db_obj"))
  new_db_migration(
    x = list(
      old_obj = old_obj
    ),
    class = "db_migration_delete"
  )
}

#' @export
db_name.db_migration_create <- function(x) {
  glue("Create {db_name(x$new_obj)}")
}

#' @export
db_name.db_migration_update <- function(x) {
  glue("Update {db_name(x$old_obj)} to {db_name(x$new_obj)}")
}

#' @export
db_name.db_migration_delete <- function(x) {
  glue("Delete {db_name(x$old_obj)}")
}
