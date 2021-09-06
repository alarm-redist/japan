#' Count Number of Block Overlaps
#'
#' @param plans matrix of redistricting plans
#' @param blocks vector of block assignments (key)
#' @param ndists number of districts
#'
#' @return a table with the number of block overlaps per plan
#'
#' @concept getdata
#'
#' @export
#'

overlap_vector <- function(plans, blocks){

  # find number of plans in matrix
  N <- ncol(plans)

  # establish unique counties
  orig <- unique(blocks)

  ndists <- max(plans[, 1])
  n_units <- length(plans[, 1])

  # initializing the vector containing the number of splits
  splits_vec <- matrix(nrow = n_units, ncol = N)

  # count splits
  for(n in 1:N){

    n_overlap <- rep(0, n_units)

    for(i in 1:ndists){

      if(length(unique(blocks[which(plans[, n] == i)])) > 1) {

        n_overlap[which(plans[, n] == i)] <- 1

      }

    }

    splits_vec[, n] <- n_overlap

  }

  return(splits_vec)

}

