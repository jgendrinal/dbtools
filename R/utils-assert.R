`%all_inherits%` <- function(x, class_name) {
  purrr::every(x, ~inherits(., class_name))
}

`%none_inherits%` <- function(x, class_name) {
  purrr::none(x, ~inherits(., class_name))
}

is_named <- function(x) {
  !is.null(names(x)) && !any(names(x) == "")
}

validate_set <- function(...) {
  validations <- purrr::discard(list(...), ~isTRUE(. %||% TRUE))
  validations <- if (length(validations) == 0) {
    TRUE
  } else {
    rlang::squash(validations)
  }
  validations
}

validate_function <- function(f, ..., silent = TRUE) {
  validation <- try(f(...), silent = silent)
  validation <- if (inherits(validation, "try-error")) {
    as.character(validation)
  } else if (is.character(validation)) {
    paste0(validation, collapse = ",")
  } else if (is.null(validation)) {
    TRUE
  } else if (!isTRUE(validation)) {
    glue("Custom Validation Failed: {validation %||% 'NULL'}")
  }
  validation
}
