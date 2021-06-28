#' Download Prefectural Shapefile Data
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile data downloaded in data/ directory and resulting shp
#'
#' @concept downloaddata
#'
#' @export
#'

download_shp <- function(pref_code){

  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)

  # check if data/ folder exists in working directory
  if(!file.exists("~/data")){

    # if not, create data folder
    dir.create("~/data")

  }

  # download the files from the Japanese Government Statistics Portal (e-Stat)
  download.file(paste('https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A002005212015&code=',
                      as.character(pref_code), '&coordSys=1&format=shape&downloadType=5', sep = ''), '~/data/shp_data.zip')

  # unzip the downloaded zip file
  unzip("~/data/shp_data.zip", exdir = "~/data")

  # remove the zip file after zip is decompressed
  file.remove("~/data/shp_data.zip")

  # return the shp file
  pref_raw <- sf::st_read(paste("~/data/h27ka", as.character(pref_code), '.shp', sep = ''))
  return(pref_raw)

}
