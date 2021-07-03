#' Calculate Ippyo-no-kakusa of simulation output(maximum weight disparity in voter power)
#'
#' @param redist_simulation_output a data table of the `redist` simulation function output (`redist_smc` etc.)
#'
#' @return a table (n_sims x 2) with the ippyo no kakusa parameters for the simulated plan
#'
#' @concept calculate
#'
#' @export
#'

#' Calculate Ippyo-no-kakusa of simulation output(maximum weight disparity in voter power)
#'
#' @param redist_simulation_output a data table of the `redist` simulation function output (`redist_smc` etc.)
#'
#' @return a table (n_sims x 4) with the ippyo no kakusa parameters for the simulated plan
#'
#' @concept calculate
#'
#' @export
#'

simulation_maxmin_LH_table <- function(redist_simulation_output){

  # set number of district
  n_dist <- nrow(redist_simulation_output %>% dplyr::filter(draw == 1))

  ### initial draw for `redist_mergesplit` ###
  if(redist_simulation_output$draw[1] == "<init>"){
    # subset to initial draw
    init <- redist_simulation_output %>%
      dplyr::filter(draw == "<init>") %>%
      tibble::deframe()

    # Max to Min ratio
    max_to_min <- NA
    max_to_min <- max(init)/min(init)

    # Loosemore Hanby index
    LH <- NA
    for (i in 1:length(init)){
      LH <-  (abs((init[i] / sum(init)) - (1 / length(init)))) / 2
    }

    # create table output
    init_weight_disparity_table <- NA
    init_weight_disparity_table <- dplyr::tibble(
      `max_to_min` = max_to_min,
      `LH` = LH
    )
  }

  ### output of the simulations ###
  # remove initial draw
  simulation <- redist_simulation_output %>%
    dplyr::filter(draw != "<init>")

  # Number of iterations for the simulation
  n_sims <- nrow(simulation)/n_dist

  # reset the data frame
  max_to_min <- vector(length = n_sims)
  LH <- vector(length = n_sims)
  simulation_weight_disparity_table <- NA


  # Calculate statistics for each iterations
  for(k in 1:n_sims){

    # filter to simulation level
    simulation <- redist_simulation_output %>%
      dplyr::filter(draw == k)

    total_pop <- simulation$total_pop

    # save statistics in the k_th vector
    # Max to Min ratio
    max_to_min[k] <- max(total_pop)/min(total_pop)

    # Loosemore Hanby index
    lh_total <- 0
    for (i in 1:n_dist){
      lh_total <- lh_total +
        (abs((total_pop[i] / sum(total_pop)) -
               (1 /n_dist))) / 2
    }
    LH[k] <- lh_total
<<<<<<< HEAD
  }
=======

  }

>>>>>>> 7b5a2efa9bdea8ff5b90a829b53200d98be90b76
  # Integrate those n_sims of vectors to table
  simulation_weight_disparity_table <- dplyr::tibble(
    `max_to_min` = max_to_min,
    `LH` = LH
  )

  ### Combine if there is `<init>` ###
  ifelse(redist_simulation_output$draw[1] == "<init>",
         return <- rbind(init_weight_disparity_table,
                         simulation_weight_disparity_table),
         return <- simulation_weight_disparity_table)

  # n_sims x 2 table
  return(return)

}

