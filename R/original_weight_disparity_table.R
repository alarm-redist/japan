#' Calculate Original Ippyo-no-kakusa (maximum weight disparity in voter power)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#'
#' @return a table with the ippyo no kakusa parameters for the original plan
#'
#' @concept calculate
#'
#' @export
#'
#' @importFrom reldist gini
#'

original_weight_disparity_table <- function(pref){

  # group by districts
  orig <- pref %>%
    group_by(cd) %>%
    summarise(JINKO=sum(JINKO))

  # Max to Min ratio
  max_to_min <- NA
  max_to_min <- max(orig$JINKO)/min(orig$JINKO)

  # Gini Coefficient
  Gini <- NA
  Gini <- reldist::gini(x = orig$JINKO,
                        weights = rep(1, nrow(orig)))

  # Loosemore Hanby index
  LH <- NA
  for (i in 1:nrow(orig)){
    LH <-  (abs((orig$JINKO[i] / sum(orig$JINKO)) - (1 / nrow(orig)))) / 2
  }

  # Hirshman Herfindahl index
  HH <- NA
  HH <- sum(orig$JINKO^2)/sum(orig$JINKO)^2

  # create table output
  original_weight_disparity_table <- NA
  original_weight_disparity_table <- dplyr::tibble(
    `max_to_min` = max_to_min,
    `Gini` = Gini,
    `LH` = LH,
    `HH` = HH
  )

  return(original_weight_disparity_table)
}
