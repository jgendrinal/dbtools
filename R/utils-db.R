db_raise_names <- function(x, overwrite = TRUE) {

  assert_that(is.list(x))
  assert_that(is.flag(overwrite))

  db_names <- map_chr(x, db_name)

  if (!overwrite && !is.null(names(x))) {
    db_names <- coalesce(na_if(names(x), ""), db_names)
  }

  set_names(x, db_names)

}

db_select <- function(table, ...) {

  assert_that(inherits(table, "db_table"))

  selected <- table$columns[eval_select(expr(c(...)), table$columns)]
  assert_that(length(selected) > 0)

  selected

}
