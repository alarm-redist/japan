#' Merge Components of Municipalities of Choice
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param split_codes an array of codes for municipalities to split
#'
#' @return an sf object with the chosen municipalities split
#'
#' @concept getdata
#'

merge_small <- function(pref, split_codes){
  
  # initialize the merge the municipalities that are not designated to be split by user
  merged <- pref[pref$code %in% split_codes == FALSE, ]  %>% group_by(code) %>% 
    summarize(geometry = st_union(geometry), pop = sum(pop))
  
  # adding back the split municipalities
  for (muni in split_codes) {
    merged <- dplyr::dplyr::bind_rows(pref[pref$code == muni, ], merged)
  }
  
  return(merged)
  
}
