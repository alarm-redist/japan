#' Calculate Ippyo-no-kakusa of simulation output(maximum weight disparity in voter power)
#'
#' @param redist_simulation_output a data table of the `redist` simulation function output (`redist_smc` etc.)
#'
#' @return a table (n_sims x 5) with the ippyo no kakusa parameters for the simulated plan
#'
#' @concept calculate
#'
#' @export
#'

simulation_weight_disparity_table <- function(redist_simulation_output){

  # set number of district
  n_dist <- nrow(redist_simulation_output %>%
                   dplyr::filter(draw == 1))

  # Number of iterations for the simulation
  n_sims <- nrow(redist_simulation_output)/n_dist

  # reset the data frame
  max_to_min <- vector(length = n_sims)
  Gini <- vector(length = n_sims)
  LH <- vector(length = n_sims)
  HH <- vector(length = n_sims)
  draw <- unique(redist_simulation_output$draw)

  # Calculate statistics for each iterations
  for(k in 1:n_sims){

    # filter to simulation level
    simulation <- redist_simulation_output[(n_dist*(k-1) + 1):(n_dist*k), ]

    total_pop <- simulation$total_pop

    # save statistics in the k_th vector
    # Max to Min ratio
    max_to_min[k] <- max(total_pop)/min(total_pop)

    # Gini Coefficient
    Gini[k] <- reldist::gini(x = total_pop,
                             weights = rep(1, n_dist))

    # Loosemore Hanby index
    lh_total <- 0
    for (i in 1:n_dist){
      lh_total <- lh_total +
        (abs((total_pop[i] / sum(total_pop)) -
               (1 /n_dist))) / 2
    }
    LH[k] <- lh_total

    # Hirshman Herfindahl index
    HH[k] <- sum(total_pop^2)/sum(total_pop)^2

  }

  # Integrate those n_sims of vectors to table
  simulation_weight_disparity_table <- dplyr::tibble(
    `max_to_min` = max_to_min,
    `Gini` = Gini,
    `LH` = LH,
    `HH` = HH,
    `draw` = draw
  )

  # n_sims x 5 table
  return(simulation_weight_disparity_table)

}
