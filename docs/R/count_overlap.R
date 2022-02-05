#' Count Number of Block Overlaps
#'
#' @param plans matrix of redistricting plans
#' @param blocks vector of block assignments (key)
#' @param ndists number of districts
#'
#' @return a vector with the number of block overlaps per plan
#'
#' @concept getdata
#'
#' @export
#'

count_overlap <- function(plans, blocks){

  # find number of plans in matrix
  N <- ncol(plans)

  # initializing the vector containing the number of splits
  splits_vec <- vector(mode = "list", length = N)

  # establish unique counties
  orig <- unique(blocks)

  ndists <- max(plans[, 1])

  # count splits
  for(n in 1:N){

    n_overlap <- 0.0
    for(i in 1:ndists){

      n_overlap <- n_overlap + as.integer(length(unique(blocks[which(plans[, n] == i)])) > 1)

    }

    splits_vec[n] <- as.integer(n_overlap)

  }

  return(unlist(splits_vec))

}

