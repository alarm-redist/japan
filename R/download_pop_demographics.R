#' Download Prefectural Population Demographics Data
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return demographic data downloaded in data/ directory and resulting csv
#'
#' @concept downloaddata
#'

download_pop_demographics <- function(pref_code){

  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)

  # check if data/ folder exists in working directory
  if(!file.exists("data")){

    # if not, create data folder
    dir.create("data")

  }

  # download the files from the Japanese Government Statistics Portal (e-Stat)
  fn = paste('data/003_', as.character(pref_code), '.csv', sep = '')

  download.file(paste('https://github.com/reiy24/jcdf_data/releases/download/06232021/',
                      fn, sep = ''),
                paste('data/', fn, sep = ''))

  # return array
  age_pops <- read.csv(file = paste('data/', fn, sep = ''), fileEncoding = "Shift-JIS", skip = 5)
  return(age_pops)

}
