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

clean_pref_pop_2020 <- function(pref_pop_2020, sub_code = FALSE){

  pref_pop_2020 <- pref_pop_2020 %>%

    # Select and rename columns
    select(X2, X3, X4, X12, X10, X15) %>%
    rename(mun_code = X2, sub_code = X3, sub_name = X10,
           level_code = X4, total_2020 = X12, foreign_2020 = X15) %>%
    mutate(mun_code = as.numeric(mun_code),
           sub_code = as.numeric(sub_code),
           total_2020 = as.numeric(total_2020),
           foreign_2020 = as.numeric(foreign_2020))

  if(sub_code == TRUE){ # Returns population data at the 小地域 level

    pref_pop_2020 <- pref_pop_2020 %>%

      # Discard aggregated/disaggregated data and keep data at 小地域 level
      filter(level_code %in% c("1", "4") == FALSE)


  } else { # Returns population data ath the municipality level

    pref_pop_2020 <- pref_pop_2020 %>%

      # Filter out municipality level data
      filter(level_code  == "1")
  }

  pref_pop_2020 <- pref_pop_2020 %>%

    # Replace NA's with 0
    mutate(total_2020 = ifelse(is.na(total_2020), 0, total_2020),
           foreign_2020 = ifelse(is.na(foreign_2020), 0, foreign_2020),
           pop = total_2020 - foreign_2020) %>%

    select(mun_code, sub_code, sub_name, pop)

  pref_pop_2020 <- as_tibble(pref_pop_2020)

  return(pref_pop_2020)
}
