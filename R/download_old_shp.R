#' Download 2000 Prefectural Shapefile Data 
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile data downloaded in data/ directory and resulting shp
#'
#' @concept downloaddata
#'
#' @export
#'


download_old_shp <- function(pref_code){
  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)
  
  # check if data/ folder exists in working directory
  if(!file.exists("data")){
    
    # if not, create data folder
    dir.create("data")
    
  }
  
  # download the files
  download.file(paste('https://github.com/reiy24/jcdf_data/releases/download/06292021/N03-001001_' , 
                      pref_code, '_GML.zip', sep = ''), 'data/old_shp_data.zip')
  
  # unzip the downloaded zip file
  unzip("data/old_shp_data.zip", exdir = "data")
  
  # remove the zip file after zip is decompressed
  file.remove("data/old_shp_data.zip")
  
  # return the shp file
  pref_raw <- sf::st_read(paste("data/N03-001001_", as.character(pref_code), '-g_AdministrativeBoundary.shp', sep = ''),
                          options = "ENCODING = CP932")
  return(pref_raw)
}
