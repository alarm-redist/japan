#' Calculate Original Compactness (cut edge)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#' @param pop_tol the vector of precinct populations. Defaults to the pop, population, or total_pop columns, if one exists.
#'
#' @return a vector with the compactness score for the vector of plans
#'
#' @concept calculate
#'
#' @export
#'

original_cut_edge <- function(pref, pop_tol){

  pop <- pref$pop # Population by region
  prefadj <- redist::redist.adjacency(shiga) # Adjacency list

  pref_map <- redist::redist_map(pref,
                        ndists = length(unique(pref$cd)),
                        pop_tol= pop_tol,
                        total_pop = pop,
                        adj = prefadj)

  # Number of Edges
  nedge <- as.numeric(length(unlist(prefadj)))

  # Original Plan
  n_rem_orig <- redist::redist.compactness(shp = pref,
                                           plans = pref$cd,
                                           measure = c("EdgesRemoved"),
                                           total_pop = pop,
                                           adj = prefadj)[1, ]$EdgesRemoved

  # Original Compactness
  ecc_orig <- n_rem_orig/nedge

  return(ecc_orig)
}
