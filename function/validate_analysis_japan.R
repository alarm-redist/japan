#' Create a plot validating an analysis for Japan
#' The code was taken from the Fifty-States Project,
#' and the redist-japan team modified it for this project.
#'
#' Saves to the `data-out/validation/` folder
#'
#' @param plans a `redist_plans` object with summary stats
#'
#' @returns the output path, invisibly
#' @export
#'
validate_analysis_japan <- function(plans, map, pref_code, pref_name){
  library(ggplot2)
  p_wgts <- plot(plans) + theme_bw()

  plan_div <- plans_diversity(plans, n_max = 150)
  p_div <- qplot(plan_div, bins = I(40), xlab = "VI distance", main = "Plan diversity") + theme_bw()

  p_dev <- hist(plans, plan_dev, bins = 40) + labs(title = "Population deviation") + theme_bw()
  p_comp1 <- hist(plans, comp_edge, bins = 40) + labs(title = "Compactness: fraction kept") + theme_bw()
  p_comp2 <- redist.plot.distr_qtys(plans, comp_polsby, geom = "boxplot") + labs(title = "Compactness: Polsby-Popper") + theme_bw()

  if ("gun_split" %in% names(plans)) {
    p_split1 <- hist(plans, gun_split) + labs(title = "County splits") + theme_bw()
  } else p_split1 <- patchwork::plot_spacer()
  if ("mun_split" %in% names(plans)) {
    p_split2 <- hist(plans, mun_split) + labs(title = "Municipality splits") + theme_bw()
  } else p_split2 <- patchwork::plot_spacer()

  enac_sum <- plans %>%
    filter(draw == "lh_2022") %>%
    # TODO: match with what gets plotted
    mutate(
      dist_lab = paste0(str_pad(.$district, width = 2, pad = '0')),
      ruling_share_rank = rank(ruling_share), # ascending order
      compact_rank = rank(comp_polsby),
    )

  # add label
  p_comp2 <-  p_comp2 +
    geom_text(data = enac_sum,
              aes(x = compact_rank,
                  label = dist_lab), # vjust = "inward" is more proper but at the edges
              vjust = 3,
              y = Inf,
              size = 2.5,
              fontface = "bold",
              lineheight = 0.8,
              alpha = 0.8,
              color = "red")

  p_ruling_share <- redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                                 geom = "boxplot") +
                        # add label
                        geom_text(data = enac_sum,
                                  aes(x = ruling_share_rank,
                                      label = dist_lab),
                                  vjust = 3,
                                  y = Inf,
                                  size = 2.5,
                                  fontface = "bold",
                                  lineheight = 0.8,
                                  alpha = 0.8,
                                  color = "red") +
                        labs(title = "Ruling (LDP + Komei) share") +
                        theme_bw()

  draws <- sample(levels(subset_sampled(plans)$draw), 3)
  p_ex1 <- redist.plot.plans(plans, draws[1], map)
  p_ex2 <- redist.plot.plans(plans, draws[2], map)
  p_ex3 <- redist.plot.plans(plans, draws[3], map)

  layout <- "
AAABBB
CCCDDD
EEEEEE
FFFGGG
HHHHHH
IIJJKK
IIJJKK"
  p <- patchwork::wrap_plots(A = p_wgts, B = p_div, C = p_dev, D = p_comp1,
                             E = p_comp2, F = p_split1, G = p_split2,
                             H = p_ruling_share, I = p_ex1, J = p_ex2, K = p_ex3, design = layout) +
    patchwork::plot_annotation(title = paste(as.character(pref_code),
                                             "_",
                                             as.character(pref_name),
                                             "_validation",
                                             sep = ""),
                               subtitle = paste(Sys.time(),
                                                " (",
                                                Sys.timezone(),
                                                ") ",
                                                sep = "")) +
    patchwork::plot_layout(guides = "collect")
  out_path <- here(paste("data-out/validation/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "_validation.png",
                         sep = ""))
  ggsave(out_path, plot = p, height = 15, width = 10)
  if (rstudioapi::isAvailable()) rstudioapi::viewer(out_path)
  invisible(out_path)
}
