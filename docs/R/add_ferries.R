#' Adding ferry-related adjacencies
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#'
#' @return array with two columns, containing start port location to end port location for boats
#'
#' @concept organizedata
#'
#' @export

add_ferries <- function(pref){

  # find numeric prefix for prefecture
  pref_num <- floor(pref$code[1]/1000)

  # import route and port data
  load(file = "./data-raw/route_data.rda")
  load(file = "./data-raw/port_data.rda")

  # re-projecting port and route data to match pref's CRS.
  sf::st_crs(route_data) <- sf::st_crs(pref)
  sf::st_crs(port_data) <- sf::st_crs(pref)

  # selecting columns of route data (start port, stop port)
  route_data <- route_data %>% dplyr::select(N09_011, N09_014)
  names(route_data) <- c("start", "stop", "geometry")

  # filter out the routes that are intra-prefectural
  pref_internal <- route_data %>% dplyr::filter(substr(route_data$start, 1, 2) == as.character(pref_num)
                                                & substr(route_data$stop, 1, 2) == as.character(pref_num), )
  if(nrow(pref_internal) == 0) {stop("No ferries in prefecture.")}

  # find the ports located within prefecture of choice
  pref_ports <- port_data %>% dplyr::filter(substr(N09_001, 1, 2) == as.character(pref_num), )
  if(nrow(pref_ports) == 0) {stop("No ferries in prefecture.")}

  pref_boundary <- sf::st_boundary(pref)

  # create "closest" column that finds the municipality/district in which the port is located
  pref_ports$closest <- flatten(nngeo:::st_nn(pref_ports, pref_boundary))

  # sort the ports in order
  for (x in 1:length(pref_internal$start)) {

    array <- as.numeric(c(pref_ports$closest[which(pref_ports$N09_001 == pref_internal$start[x])],
                          pref_ports$closest[which(pref_ports$N09_001 == pref_internal$stop[x])]))

    pref_internal$start[x] <- min(array)
    pref_internal$stop[x] <- max(array)

  }

  # isolate unique routes
  pref_internal <- unique(cbind(as.numeric(pref_internal$start), as.numeric(pref_internal$stop)))

  # create data frame
  pref_internal <- as.data.frame(pref_internal)

  # delete routes where start/stop municipalities are the same
  pref_internal <- pref_internal[which(pref_internal[, 1] != pref_internal[, 2]), ]

  # filter out the routes that connect isolated areas (islands) to the mainland
  # obtain adjacency list
  prefadj <- redist::redist.adjacency(pref)

  # check the number of adjacencies per municipality
  edges <- rep(0,time = length(prefadj))
  for(i in 1:length(prefadj)){
    edges[i] <- length(prefadj[[i]])
  }

  # check which municipalities are isolated
  edges_mun <- as.data.frame(edges)
  isolated_mun_index <- which(edges_mun$edges == 0)

  # filter out routes that connect isolated municipalities
  pref_internal_isolated <- pref_internal %>%
      dplyr::filter(V1 %in% isolated_mun_index | V2 %in% isolated_mun_index)

  # return result
  return(pref_internal_isolated)

}

