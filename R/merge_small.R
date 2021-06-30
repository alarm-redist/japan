#' Merge Components of Municipalities of Choice
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param split_codes an array of codes for municipalities to split
#'
#' @return an sf object with the chosen municipalities split
#'
#' @concept getdata
#'
#' @export
#'

#Ex: nagasaki <- merge_small(nagasaki, split_codes = c(42201, 42202))
merge_small <- function(pref, split_codes = NULL, intact_codes = NULL){
  if(missing(split_codes)) {
     if(missing(intact_codes)){
      #######No municipality split +  no wards to keep#########
       bound <- pref %>%
         group_by(code) %>%
         summarise(geometry = st_union(geometry), pop = sum(pop))
     }

    else{
    #######No municipality split + one or more wards to keep intact########
    #(as in the case of Kyoto)
      #municipalities to keep intact
      bound <- pref[pref$code %in% intact_codes == FALSE,] %>%
        group_by(code) %>%
        summarise(geometry = st_union(geometry), pop = sum(pop))
      #group together the wards and merge dataframe
      for(i in 1:length(intact_codes)){
        intact <- pref[pref$code ==  intact_codes[i],] %>%
          group_by(code) %>%
          summarise(geometry = st_union(geometry), pop = sum(pop))
        bound <- dplyr::bind_rows(bound, intact)
      }
    }

  } else {
    if(missing(intact_codes)){
    ########One or more municipality split + no wards to keep intact########
    #(as in the case of Nagasaki)
        #municipalities to keep intact
        bound <- pref[pref$code %in% split_codes == FALSE, ]  %>%
            group_by(code) %>%
            summarize(geometry = st_union(geometry), pop = sum(pop))
        # specify municipalities to split(Ex: Nagasaki, Sasebo)
        # and merge dataframe
        for(i in 1:length(split_codes)){
          split <- pref[pref$code == split_codes[i],]
          bound <- dplyr::bind_rows(bound, split)
        }

    }else{
      #########One or more municipality split + one or more wards to keep intact#########
      #other municipalities
      bound <- pref[pref$code %in% c(split_codes, intact_codes) == FALSE, ]  %>%
        group_by(code) %>%
        summarize(geometry = sf::st_union(geometry), pop = sum(pop))
      #specify municipalities to split and merge dataframe
      for(i in 1:length(split_codes)){
        split <- pref[pref$code == split_codes[i],]
        bound <- dplyr::bind_rows(bound, split)
      }
      #group together the wards and merge dataframe
      for(i in 1:length(intact_codes)){
        intact <- pref[pref$code ==  intact_codes[i],] %>%
                                     group_by(code) %>%
                                     summarise(geometry = st_union(geometry), pop = sum(pop))
        bound <- dplyr::bind_rows(bound, intact)
      }
    }
  }
  return(bound)
}

