#' @import dbplyr
#' @import dbx
#' @import cli
#' @import DBI
#' @import pool
#' @import bit64
#' @import logger
#' @importFrom digest
#'             digest
#' @importFrom lubridate
#'             is.Date
#'             is.POSIXct
#' @importFrom purrr
#'             map
#'             map_lgl
#'             map_chr
#'             set_names
#' @importFrom memoise
#'             memoise
#'             forget
#' @importFrom tibble
#'             new_tibble
#'             tibble
#' @importFrom assertthat
#'             is.flag
#'             is.string
#'             assert_that
#'             validate_that
#'             %has_args%
#'             is.number
#' @importFrom tidyselect
#'             eval_select
#'             eval_rename
#' @importFrom snakecase
#'             to_snake_case
#' @importFrom magrittr
#'             %>%
#' @importFrom rlang
#'             %||%
#'             arg_match
#'             enexpr expr is_expression
#' @importFrom glue
#'             glue
#'             glue_collapse
#' @importFrom dplyr
#'             na_if
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(...) {
  logger::log_layout(logger::layout_glue_colors)
}
