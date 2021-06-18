#' Merge Components of Municipalities of Choice
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param split_codes an array of codes for municipalities to split
#'
#' @return an sf object with the chosen municipalities split
#'
#' @concept getdata
#'

#Ex: nagasaki <- merge_small(nagasaki, split_codes = c(42201, 42202))
merge_small <- function(pref, split_codes){

  # initialize the merge the municipalities that are not designated to be split by user
  merged <- pref[pref$code %in% split_codes == FALSE, ]  %>%
            group_by(code) %>%
            summarize(geometry = sf::st_union(geometry), pop = sum(pop))
  
  # adding back the split municipalities
  for(i in split_codes){
          split <- dplyr::bind_rows(nagasaki[nagasaki$code == split_codes,])
        }
  # Bind together
        bound <- dplyr::bind_rows(split, merged)
  
  # return the result
  return(bound)

}
