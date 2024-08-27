#' Checking whether the plan has any discontiguous areas/enclaves
#'
#' @param plans_matrix output of `redist::get_plans_matrix` after `redist_smc`
#' @param mainland sf object of cleaned, collated, census data for a prefecture, without islands or isolated areas
#' @param mainland_adj (edited) adjacency list for a prefecture, without islands or isolated areas
#' @return logical vector
#'
#' @concept filter validated plans
#'
#' @export

check_contiguous <- function(pref_smc_plans, mainland, mainland_adj) {

  # Empty vector
  valid <- vector(length = ncol(pref_smc_plans))

  # Plans matrix: focus on mainland
  mainland_plans <- pref_smc_plans[mainland$unit, ]

  # Check contiguity
  mainland$component <- geomander::check_contiguity(adj = mainland_adj)$component

  for (k in 1:ncol(pref_smc_plans)){
    valid[k] <- max(geomander::check_contiguity(mainland_adj, mainland_plans[, k])$component) == 1
  }

  return(valid)

}
