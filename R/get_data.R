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
  pref_name <- pref_raw %>% 
      filter(nagasaki_raw$HCODE != 8154, )  %>%
      group_by(PREF, CITY, KIHON1) %>% 
      summarize(geometry = st_union(geometry), JINKO = sum(JINKO))
  
  # reformatting the types of the JCDF data
  pref_name$PREF <- as.numeric(pref_name$PREF)
  pref_name$CITY <- as.numeric(pref_name$CITY)
  
  # creates municipality codes, of the form XXYYY
  pref_name$REGION <- (1000*pref_name$PREF + pref_name$CITY) 
  
  pref_name <- pref_name[, -c(1, 2)]
  names(pref_name) <- c("town", "geometry", "pop", "code")
  
  # deleting redundant municipality-wide shapes
  pref_name <- pref_name[which(pref_name$town != "0000"), ]
  
  return(pref_name)
  
}
