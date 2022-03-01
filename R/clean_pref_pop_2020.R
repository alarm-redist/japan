#' Clean 2020 Census data at census block level
#'
#' @param pref_pop_2020 raw 2020 census data
#'
#' @return a data frame object of cleaned 2020 census data
#'
#' @concept getdata
#'
#' @export
#'

clean_pref_pop_2020 <- function(pref_pop_2020){

  pref_pop_2020 <- pref_pop_2020 %>%

    # Select and rename columns
    select(V2, V3, V4, V12, V10, V15) %>%
    rename(mun_code = V2, sub_code = V3, sub_name = V10,
           level_code = V4, total_2020 = V12, foreign_2020 = V15) %>%
    mutate(total_2020 = as.numeric(total_2020),
           foreign_2020 = as.numeric(foreign_2020)) %>%

    # Discard aggregated/disaggregated data
    filter(level_code %in% c("1", "4") == FALSE) %>%

    # Replace NA's with 0
    mutate(total_2020 = ifelse(is.na(total_2020), 0, total_2020),
           foreign_2020 = ifelse(is.na(foreign_2020), 0, foreign_2020),
           pop = total_2020 - foreign_2020) %>%
    select(mun_code, sub_code, sub_name, pop)

  pref_pop_2020 <- as_tibble(pref_pop_2020)

  return(pref_pop_2020)

}
