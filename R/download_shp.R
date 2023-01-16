#' Download Prefectural Shapefile Data
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile data downloaded in data-raw/ directory and resulting shp
#'
#' @concept downloaddata
#'
#' @export
#'

download_shp <- function(pref_code){

  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)

  # check if data-raw/ folder exists in working directory
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # download the files from the Japanese Government Statistics Portal (e-Stat)
  # This shapefile is from 2020 Census (Reiwa 2)
  download.file(paste('https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A002005212020&code=',
                      as.character(pref_code), '&coordSys=1&format=shape&downloadType=5', sep = ''), 'data-raw/shp_data.zip')

  # unzip the downloaded zip file
  unzip("data-raw/shp_data.zip", exdir = "data-raw/")

  # remove the zip file after zip is decompressed
  file.remove("data-raw/shp_data.zip")

  # return the shp file
  pref_raw <- sf::st_read(paste("data-raw/r2ka", as.character(pref_code), '.shp', sep = ''))
  return(pref_raw)

}
