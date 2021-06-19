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
merge_small <- function(pref, split_codes = NULL, intact_codes = NULL){
  if(missing(split_codes)) {
     if(missing(intact_codes)){
     #######No municipality split +  no wards to keep#########
       print("No need to use this function")
     }
    
    else{
    #######No municipality split + one or more wards to keep intact########
    #(as in the case of Kyoto)
      #municipalities to keep intact  
      collapsed <- pref[pref$code %in% intact_codes == FALSE,] %>%
        group_by(code) %>%
        summarise(geometry = st_union(geometry), pop = sum(pop))
      #group together the wards
      for(i in intact_codes){
        intact <- dplyr::bind_rows(pref[pref$code ==  intact_codes,] %>%
             group_by(code) %>%
             summarise(geometry = st_union(geometry), pop = sum(pop)))
        }
      #Bind together
      bound <- dplyr::bind_rows(intact, collapsed)
    }
    
  } else {
    if(missing(intact_codes)){
    ########One or more municipality split + no wards to keep intact########
    #(as in the case of Nagasaki)
        #municipalities to keep intact
        merged <- pref[pref$code %in% split_codes == FALSE, ]  %>%
            group_by(code) %>%
            summarize(geometry = sf::st_union(geometry), pop = sum(pop))
        # municipalities to split(Ex: Nagasaki, Sasebo)
        for(i in split_codes){
          split <- dplyr::bind_rows(pref[pref$code == split_codes,])
        }
        # adding back the municipalities  
        bound <- dplyr::bind_rows(split, merged)
        
    }else{
      #########One or more municipality split + one or more wards to keep intact#########
      #municipalities to split
      for(i in split_codes){
        split <- dplyr::bind_rows(pref[pref$code == split_codes,])
      }
      #group together the wards
      for(i in intact_codes){
        intact <- dplyr::bind_rows(pref[pref$code ==  intact_codes,] %>%
                                     group_by(code) %>%
                                     summarise(geometry = st_union(geometry), pop = sum(pop)))
      }
      #other municipalities
      merged <- pref[pref$code %in% c(split_codes, intact_codes) == FALSE, ]  %>%
        group_by(code) %>%
        summarize(geometry = sf::st_union(geometry), pop = sum(pop))
      #adding back the municipalities
      bound <- dplyr::bind_rows(split, intact, merged)
    }
  }
  return(bound)
}
