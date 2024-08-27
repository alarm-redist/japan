#' Download 2022 House of Councillors Elections Data (Proportional Representation)
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile xlsx downloaded in data-raw/ directory and resulting xlsx
#'
#' @concept downloaddata
#'
#' @export
#'

download_2022_HoC_PR <- function(pref_code){

  # check if `data-raw `folder exists
  if(!file.exists("data-raw")){

    # if not, create data folder
    dir.create("data-raw")

  }

  # Format prefecture code
  pref_code <- as.numeric(pref_code)
  pref_code_pad <- str_pad(pref_code, 2, pad = "0")

  # check if data exists
  if(file.exists(paste0('data-raw/', pref_code, '_2022_HoC_PR.xlsx')) == TRUE){

    # read file
    pref_2022_HoC_PR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2022_HoC_PR.xlsx'),
                                           skip = 5)

  }else{
    # if not, download file

    # obtain links to xlsx files
    links <-
      rvest::read_html(paste0("https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/sangiin26_8_",
                              pref_code_pad, ".html")) %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      grep("xlsx", ., value = TRUE)

    # select link to relevant file
    link_PR_party <- links[2]

    # download file
    download.file(paste0('https://www.soumu.go.jp/' ,
                         link_PR_party),
                  paste0('data-raw/', pref_code, '_2022_HoC_PR.xlsx'))

    # read file
    pref_2022_HoC_PR <- readxl::read_excel(paste0('data-raw/', pref_code, '_2022_HoC_PR.xlsx'),
                                           skip = 5)
  }

  # return file
  return(pref_2022_HoC_PR)
}
