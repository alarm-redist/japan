#' Estimate 2020 Populations For Lower-Level Census Units from Municipality-Level Data
#'
#' @param pref an sf object with cleaned, collated version of the census data
#' @param census2020 the output of clean_2020_census
#'
#' @return vector of population estimates
#'
#' @concept estimatedata
#'
#' @export
#'

estimate_2020_pop <- function(pref, census2020) {
  
  pref_new <- pref
  
  # Get the aggregate municipality-level totals from the 2015 Census
  totals_2015 <- cbind.data.frame(pref$code, pref$pop_national)
  names(totals_2015) <- c("code", "pop_2015")
  totals_2015 <- totals_2015 %>%
    dplyr::group_by(code) %>%
    dplyr::summarize(pop_2015 = sum(pop_2015))
  
  # initialize 2020 population estimates vector
  estimates <- vector(length = dim(pref)[1])
  
  for(i in 1:dim(pref)[1]){
    
    # multiply 2015 population by municipality-level population growth factor (from 2015 to 2020) for estimates
    estimates[i] <- round(pref$pop_national[i] * 
                            (totals_2015 %>% dplyr::filter(totals_2015$code == pref$code[i], ))$pop_2015 / 
                            (census2020 %>% dplyr::filter(census2020$code == pref$code[i], ))$pop_national)
    
  }
  
  pref_new$pop_estimate <- estimates
  
  # return result
  return(pref_new)
  
}
