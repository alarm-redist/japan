#' Calculate Ippyo-no-kakusa of simulation output(maximum weight disparity in voter power)
#'
#' @param redist_simulation_output a data table of the `redist` simulation function output (`redist_smc` etc.)
#'
#' @return a table (n_sims x 4) with the ippyo no kakusa parameters for the simulated plan
#'
#' @concept calculate
#'

simulation_weight_disparity_table <- function(redist_simulation_output){

  # set up data frame by creating empty vector
  max_to_min <- NA
  Gini <- NA
  LH <- NA
  HH <- NA
  simulation_weight_disparity_table <- NA

  # Number of iterations
  n_sims <- max(as.numeric(redist_simulation_output$draw))

  # Calculate statistics for each iterations
  for(k in 1:n_sims){

    # filter to simulation level
    output <- redist_simulation_output %>%
      dplyr::filter(draw == k) %>%
      # make it numeric
      dplyr::summarise(total_pop = as.numeric(total_pop))

    # save statistics in the k_th vector
    # Max to Min ratio
    max_to_min[k] <- max(output)/min(output)

    # Gini Coefficient
    Gini[k] <- reldist::gini(x = output$total_pop,
                             weights = rep(1, nrow(output)))

    # Loosemore Hanby index
    for (i in 1:nrow(output)){
      LH[k] <-  (abs((output$total_pop[i] / sum(output$total_pop)) - (1 / nrow(output)))) / 2
    }

    # Hirshman Herfindahl index
    HH[k] <- sum(output$total_pop^2)/sum(output$total_pop)^2
  }

  # Integrate those n_sims of vectors to table
  simulation_weight_disparity_table <- dplyr::tibble(
    `max_to_min` = max_to_min,
    `Gini` = Gini,
    `LH` = LH,
    `HH` = HH
  )

  # n_sims x 4 table
  return(simulation_weight_disparity_table)
}
