#' Download 2019 House of Councillors Elections Data (Proportional Representation)
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile xlsx downloaded in data-raw/ directory and resulting xlsx
#'
#' @concept downloaddata
#'
#' @export
#'

download_2019_HoC_PR <- function(pref_code){

  # check if `data-raw `folder exists
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # Convert to numeric
  pref_code <- as.numeric(pref_code)

  # check if data exists
  if(file.exists(paste0('data-raw/', pref_code, '_2019_HoC_PR.xlsx')) == TRUE){

    # read file
    pref_2019_HoC_PR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2019_HoC_PR.xlsx'),
                                           skip = 5)

  }else{
    # if not, download file

    # edit prefecture code
    n <- 7556 + (pref_code - 1)*3

    # download file
    download.file(paste0('https://www.soumu.go.jp/main_content/00063' ,
                         n, '.xlsx'),
                  paste0('data-raw/', pref_code, '_2019_HoC_PR.xlsx'))

    # read file
    pref_2019_HoC_PR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2019_HoC_PR.xlsx'),
                                           skip = 5)
  }

  # return file
  return(pref_2019_HoC_PR)
}
