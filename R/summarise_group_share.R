#' @export
summarise_group_share <- function(.data, ..., .x, .g, na.rm = TRUE) {
  if(rlang::quo_is_symbol(rlang::enquo(.x))) {
    .x <- rlang::enquo(.x)
  } else {
    .x <- rlang::sym(.x)
  }

  if(rlang::quo_is_symbol(rlang::enquo(.g))) {
    .g <- rlang::enquo(.g)
  } else {
    .g <- rlang::sym(.g)
  }

  .data %>%
    dplyr::group_by(..., !!.g) %>%
    dplyr::summarise("{{.x}}" := sum(!!.x, na.rm = na.rm)) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate("share_{{.x}}" := !!.x / sum(!!.x, na.rm = na.rm)) %>%
    dplyr::ungroup()
}
