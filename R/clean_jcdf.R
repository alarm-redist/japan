#' Download JCDF (Japan Census Data File)
#'
#' @param path string containing path to the shapefile census data
#'
#' @return a sf object with cleaned, collated version of the census data
#'
#' @concept getdata
#'

clean_jcdf <- function(path){

  # download the raw JCDF data
  pref_raw <- st_read(path)

  # filter out water surfaces and extraneous port data
  pref <- pref_raw %>%
      filter(pref_raw$HCODE != 8154, )  %>%
      group_by(PREF, CITY, KIHON1) %>%
      summarize(geometry = st_union(geometry), JINKO = sum(JINKO))

  # reformatting the types of the JCDF data
  pref$PREF <- as.numeric(pref$PREF)
  pref$CITY <- as.numeric(pref$CITY)

  # creates municipality codes, of the form XXYYY
  pref$REGION <- (1000*pref$PREF + pref$CITY)

  pref <- pref[, -c(1, 2)]
  names(pref) <- c("town", "geometry", "pop", "code")

  # deleting redundant municipality-wide shapes
  pref <- pref[which(pref$town != "0000"), ]

  return(pref)

}
