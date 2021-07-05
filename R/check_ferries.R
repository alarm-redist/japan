#' Count Number of Municipality Splits
#'
#' @param pref_code number between 1~47 assigned to prefecture
#'
#' @return boolean of whether the prefecture has ferries
#'
#' @concept getdata
#'
#' @export
#'

check_ferries <- function(pref_code){

  pref_code <- sprintf("%02d", pref_code)

  data("route_data")
  data("port_data")

  # selecting columns of route data (start port, stop port)
  route_data <- route_data %>% dplyr::select(N09_011, N09_014)
  names(route_data) <- c("start", "stop", "geometry")

  # filter out the routes that are intra-prefectural
  pref_internal <- route_data %>% dplyr::filter(substr(route_data$start, 1, 2) == pref_code
                                              & substr(route_data$stop, 1, 2) == pref_code, )

  pref_ports <- port_data %>% dplyr::filter(substr(N09_001, 1, 2) == pref_code, )

  return(nrow(pref_internal) != 0 & nrow(pref_ports) != 0)

}

