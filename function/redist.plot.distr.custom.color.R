#' Plot quantities by district with customized color
#'
#' Plots a boxplot of a quantity of interest across districts, with districts
#' optionally sorted by this quantity. Adds reference points for each reference
#' plan, if applicable.
#'
#' @param plans the \code{redist_plans} object.
#' @param qty \code{\link[dplyr:dplyr_data_masking]{<data-masking>}} the
#' quantity of interest.
#' @param color_var the variable that will be used for color scaling
#' @param sort set to \code{"asc"} to sort districts in ascending order of
#' \code{qty} (the default), \code{"desc"} for descending order, or
#' \code{FALSE} or \code{"none"} for no sorting.
#' @param geom the geom to use in plotting the simulated districts: either
#' \code{"jitter"} or \code{"boxplot"}
#' @param color_thresh if a number, the threshold to use in coloring the points.
#' Plans with quantities of interest above the threshold will be colored
#' differently than plans below the threshold.
#' @param size The dot size for \code{geom="jitter"}.
#' @param ... passed on to \code{\link[ggplot2]{geom_boxplot}}
#'
#' @returns A ggplot
#'
#' @examples
#' library(dplyr)
#' data(iowa)
#'
#' iowa <- redist_map(iowa, existing_plan = cd_2010, pop_tol = 0.05, total_pop = pop)
#' plans <- redist_smc(iowa, nsims = 100, silent = TRUE)
#' plans %>%
#'     mutate(pct_dem = group_frac(iowa, dem_08, tot_08)) %>%
#'     redist.plot.distr_qtys(pct_dem)
#'
#' @concept plot
#' @export
#'
#' Note: This function was originally taken from the alarm-redist/redist repository,
#' and customized by Tyler Simko, with minor modification by the redist-japan team.
#'

redist.plot.distr.custom.color <- function (plans, qty, color_var,
                                            sort = "asc", geom = "jitter",
                                            color_thresh = NULL,
                                            size = 0.1, ...) {

  library(redist)

  if (!inherits(plans, "redist_plans")) cli_abort("{.arg plans} must be a {.cls redist_plans}")

  if (isFALSE(sort) || sort == "none") {
    plans <- dplyr::group_by(plans, .data$draw) %>%
      dplyr::mutate(.distr_no = as.factor(.data$district))
  } else {
    ord <- if (sort == "asc") 1  else if (sort == "desc") -1 else
      cli_abort("{.arg sort} not recognized: {.code {sort}}")
    plans <- dplyr::group_by(plans, .data$draw) %>%
      dplyr::mutate(.distr_no = as.factor(rank(ord*{{ qty }},
                                               ties.method = "first")))
  }

  val <- rlang::eval_tidy(enquo(qty), plans)
  if (redist:::is_const_num(val, plans$draw)) {
    cli_warn(c("{.arg {rlang::as_label(enquo(qty))}} is constant across districts. ",
               "Consider using {.fun hist} instead."))
  }
  pl_samp <- as.data.frame(subset_sampled(plans, matrix = FALSE))
  if (is.null(color_thresh)) {
    p <- ggplot(pl_samp, aes(.data$.distr_no, {{ qty }},
                             color = {{ color_var }}))
  }
  else {
    if (!is.numeric(color_thresh))
      cli_abort("{.arg color_thresh} must be numeric.")
    p <- ggplot(pl_samp, aes(.data$.distr_no, {{ qty }},
                             color = {{ color_var }})) +
      ggplot2::guides(color = "none")
  }
  if (geom == "jitter") {
    p <- p + ggplot2::geom_jitter(size = size, ...)
  }
  else if (geom == "boxplot") {
    p <- p + ggplot2::geom_boxplot(..., outlier.size = 1)
  } else {
    cli_abort("{.arg geom} must be either \"jitter\" or \"boxplot\"")
  }

  if (isFALSE(sort) || sort == "none")
    p <- p + labs(x = "District")
  else
    p <- p + labs(x = "Ordered district")






  if (redist:::get_n_ref(plans) > 0) {
    pl_ref <- as.data.frame(subset_ref(plans, matrix = FALSE))
    if (is.null(color_thresh)) {
      p <- p + labs(color = "Plan", shape = "Plan")
      if (geom == "jitter") {
        p <- p + ggplot2::geom_segment(aes(as.integer(.data$.distr_no) - 0.5,
                                           xend = as.integer(.data$.distr_no) + 0.5,
                                           yend = {{ qty }}),
                                       color = "black",
                                       data = pl_ref, size = 1.2) +
          ggplot2::geom_segment(aes(as.integer(.data$.distr_no) - 0.45,
                                           xend = as.integer(.data$.distr_no) + 0.45,
                                           yend = {{ qty }},
                                           color = {{ color_var }}),
                                       data = pl_ref, size = 0.8)
      } else {
        p <- p + ggplot2::geom_point(aes(color = .data$draw), shape = 15,
                                     size = 2, data = subset_ref(plans))
      }
    } else {
      if (geom == "jitter") {
        p <- p + labs(lty = "Plan") +
          ggplot2::geom_segment(aes(as.integer(.data$.distr_no) - 0.5,
                                    xend = as.integer(.data$.distr_no) + 0.5,
                                    yend = {{ qty }},
                                    lty = .data$draw),
                                data = pl_ref,
                                size = 1.2, color = "black")
      } else {
        p <- p + labs(shape = "Plan") +
          ggplot2::geom_point(aes(shape = .data$draw), color = "black",
                              size = 2, data = pl_ref)
      }
    }
  }
  p
}
