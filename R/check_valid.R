#' Checking whether the plan is valid without discontinuity
#'
#' @param pref_n sf object of cleaned, collated, census data for a prefecture with n municipalities are split
#' @param plans_matrix out put of `redist::get_plans_matrix` after `redist_smc`
#' @param bridges list of vectors with municipality codes connected with bridges
#' @return logical vector
#'
#' @concept filter validated plans
#'
#' @export


####-------------- check_valid() function -------------------------####
check_valid <- function(pref_n, plans_matrix, bridges) {

  pref_sep <- data.frame(unit = 1, geometry = sf::st_cast(pref_n[1, ]$geometry, "POLYGON"))

  for (i in 2:nrow(pref_n))
  {
    pref_sep <- rbind(pref_sep, data.frame(unit = i, geometry = sf::st_cast(pref_n[i, ]$geometry, "POLYGON")))
  }

  pref_sep <- sf::st_as_sf(pref_sep)
  pref_sep_adj <- redist::redist.adjacency(pref_sep)

  mainland <- pref_sep[which(unlist(lapply(pref_sep_adj, length)) > 0), ]
  mainland_adj <- redist::redist.adjacency(mainland)
  mainland$component <- geomander::check_contiguity(adj = mainland_adj)$component

  for (j in 1:length(bridges))
  {
    
    if("pre_gappei_code" %in% colnames(pref_n))
    {
      start <- which(pref_n$pre_gappei_code == bridges[[j]][1])
      end <- which(pref_n$pre_gappei_code == bridges[[j]][2])
    }
    else
    {
      start <- which(pref_n$code == bridges[[j]][1])
      end <- which(pref_n$code == bridges[[j]][2])
    }
    

    for (x in which(mainland$unit == start))
    {
      for (y in which(mainland$unit == end))
      {
        mainland_adj <- geomander::add_edge(mainland_adj,
                                            x,
                                            y,
                                            zero = TRUE)
      }
    }
  }

  checks <- vector(length = ncol(plans_matrix))

  for (k in 1:ncol(plans_matrix))
  {
    mainland_plan <- plans_matrix[mainland$unit, k]
    # mainland$temp_plan <- mainland_plan
    # checks[k] <- max(check_polygon_contiguity(shp=mainland, group=temp_plan)$component) == 1 
    checks[k] <- max(check_contiguity(mainland_adj, mainland_plan)$component) == 1
  }

  return(checks)

}
