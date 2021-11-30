#' Download 2000 Prefectural Shapefile Data 
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile data downloaded in data-raw/ directory and resulting shp
#'
#' @concept downloaddata
#'
#' @export
#'


download_old_shp <- function(pref_code){
  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)
  
  # check if data-raw/ folder exists in working directory
  if(!file.exists("data-raw")){
    
    # if not, create data-raw folder
    dir.create("data-raw")
    
  }
  
  # download the files
  download.file(paste('https://github.com/reiy24/jcdf_data/releases/download/06292021/N03-001001_' , 
                      pref_code, '_GML.zip', sep = ''), 'data-raw/old_shp_data.zip')
  
  # unzip the downloaded zip file
  unzip("data-raw/old_shp_data.zip", exdir = "data-raw")
  
  # remove the zip file after zip is decompressed
  file.remove("data-raw/old_shp_data.zip")
  
  # return the shp file
  pref_raw <- sf::st_read(paste("data-raw/N03-001001_", as.character(pref_code), '-g_AdministrativeBoundary.shp', sep = ''),
                          options = "ENCODING = CP932")
  return(pref_raw)
}
