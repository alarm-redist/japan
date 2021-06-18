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
  collapsed <- pref[pref$code %in% intact_codes == FALSE,] %>%
          group_by(code) %>%
          summarise(geometry = st_union(geometry), pop = sum(pop))

  # adding back the split municipalities
  for(i in intact_codes){
                intact <- pref[pref$code ==  i,] %>%
                group_by(code) %>%
                 summarise(geometry = st_union(geometry), pop = sum(pop))
            }
  # Bind together
        bound <- dplyr::bind_rows(intact, collapsed)
  
  # return the result
  pref <- bound
  return(pref)

}
