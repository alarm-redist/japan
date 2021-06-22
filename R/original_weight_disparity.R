#' Calculate Original Ippyo-no-kakusa (maximum weight disparity in voter power)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#' @param parameter character vector of parameter (`max_to_min`, `Gini`, `LH`, and `HH`).
#'
#' @return a vector with the ippyo no kakusa for the original plan
#'
#' @concept calculate
#'

original_weight_disparity <- function(pref, parameter){

  orig <- pref %>%
    group_by(cd) %>%
    summarise(pop=sum(pop))

  score <- NA

  if(parameter == "max_to_min"){

    score <- max(orig$pop)/min(orig$pop)

  }else if(parameter == "Gini"){
    score <- reldist::gini(x = orig$pop,
                           weights = rep(1, nrow(orig)))

  }else if(parameter == "LH"){
    for (i in 1:nrow(orig)){
      score <-  (abs((orig$pop[i] / sum(orig$pop)) - (1 / nrow(orig)))) / 2
    }

  }else if(parameter == "HH"){

    score <- sum(orig$pop^2)/sum(orig$pop)^2
  }

  return(score)
}
