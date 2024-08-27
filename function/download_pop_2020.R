#' Download 2020 Population Data
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile csv downloaded in data-raw/ directory and resulting csv
#'
#' @concept downloaddata
#'
#' @export
#'

download_pop_2020 <- function(pref_code){
  pref_code <- as.numeric(pref_code)

  # check if data/ folder exists in working directory
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # download csv file
  n <- as.character(3094 + 15* (pref_code - 1))

  download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=00003216',
                      n, '&fileKind=1', sep = ''),
                paste('data-raw/', pref_code, '_2020_pop.csv', sep = ''))

  # read csv
  pref_pop_2020 <- read_csv(paste('data-raw/', pref_code, '_2020_pop.csv', sep = ''),
                            locale = locale(encoding = "cp932"),
                            show_col_types = FALSE, col_names = FALSE,
                            skip = 5)

  return(pref_pop_2020)
}
