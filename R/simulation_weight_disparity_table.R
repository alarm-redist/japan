#' Calculate Ippyo-no-kakusa of simulation output(maximum weight disparity in voter power)
#'
#' @param redist_simulation_output a data table of the `redist` simulation function output (`redist_smc` etc.)
#'
#' @return a table (n_sims x 4) with the ippyo no kakusa parameters for the simulated plan
#'
#' @concept calculate
#'

simulation_weight_disparity_table <- function(redist_simulation_output){

  # set number of district
  n_dist <- max(as.numeric(redist_simulation_output$district))

  ### initial draw for `redist_mergesplit` ###
  if(redist_simulation_output$draw[1] == "<init>"){
    # subset to initial draw
    init <- redist_simulation_output %>%
      dplyr::filter(draw == "<init>") %>%
      dplyr::summarise(total_pop = as.numeric(total_pop)) %>%
      tibble::deframe()

    # Max to Min ratio
    max_to_min <- NA
    max_to_min <- max(init)/min(init)

    # Gini Coefficient
    Gini <- NA
    Gini <- reldist::gini(x = init,
                          weights = rep(1, length(init)))

    # Loosemore Hanby index
    LH <- NA
    for (i in 1:length(init)){
      LH <-  (abs((init[i] / sum(init)) - (1 / length(init)))) / 2
    }

    # Hirshman Herfindahl index
    HH <- NA
    HH <- sum(init^2)/sum(init)^2

    # create table output
    init_weight_disparity_table <- NA
    init_weight_disparity_table <- dplyr::tibble(
      `max_to_min` = max_to_min,
      `Gini` = Gini,
      `LH` = LH,
      `HH` = HH
    )
  }

  ### output of the simulations ###
  # remove initial draw
  simulation <- redist_simulation_output %>%
    dplyr::filter(draw != "<init>")

  # reset the data frame
  max_to_min <- NA
  Gini <- NA
  LH <- NA
  HH <- NA
  simulation_weight_disparity_table <- NA

  # Number of iterations for the simulation
  n_sims <- nrow(simulation)/n_dist

  # Calculate statistics for each iterations
  for(k in 1:n_sims){

    # filter to simulation level
    simulation <- redist_simulation_output %>%
      dplyr::filter(draw == k) %>%
      # make it numeric
      dplyr::summarise(total_pop = as.numeric(total_pop))

    # save statistics in the k_th vector
    # Max to Min ratio
    max_to_min[k] <- max(simulation)/min(simulation)

    # Gini Coefficient
    Gini[k] <- reldist::gini(x = simulation$total_pop,
                             weights = rep(1, nrow(simulation)))

    # Loosemore Hanby index
    for (i in 1:nrow(simulation)){
      LH[k] <-  (abs((simulation$total_pop[i] / sum(simulation$total_pop)) - (1 / nrow(simulation)))) / 2
    }

    # Hirshman Herfindahl index
    HH[k] <- sum(simulation$total_pop^2)/sum(simulation$total_pop)^2
  }

  # Integrate those n_sims of vectors to table
  simulation_weight_disparity_table <- dplyr::tibble(
    `max_to_min` = max_to_min,
    `Gini` = Gini,
    `LH` = LH,
    `HH` = HH
  )

  ### Combine if there is `<init>` ###
  ifelse(redist_simulation_output$draw[1] == "<init>",
         return <- rbind(init_weight_disparity_table,
                         simulation_weight_disparity_table),
         return <- simulation_weight_disparity_table)

  # n_sims x 4 table
  return(return)
}
