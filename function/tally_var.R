#' Tally a variable by district
#'
#' @param map a `redist_map` object
#' @param pop a variable to tally. Tidy-evaluated.
#' @param .data a `redist_plans` object
#'
#' @return a vector containing the tallied values by district and plan (column-major)
#' @export
#'
#' Note: This function was taken from the alarm-redist/fifty-states repository
#' Link: https://github.com/alarm-redist/fifty-states/blob/main/R/summary_stats.R
tally_var <- function(map, pop, .data = redist:::cur_plans()) {
  redist:::check_tidy_types(map, .data)
  if (length(unique(diff(as.integer(.data$district)))) > 2)
    warning("Districts not sorted in ascending order; output may be incorrect.")
  idxs <- unique(as.integer(.data$draw))
  pop <- rlang::eval_tidy(rlang::enquo(pop), map)
  as.numeric(redist:::pop_tally(get_plans_matrix(.data)[, idxs, drop = FALSE],
                                pop, attr(map, "ndists")))
}
