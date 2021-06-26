#' Download Prefectural Population Demographics Data
#'
#' @param type character vector for the type of data either 'total' or 'foreigner'.
#'
#' @return data frame with the 2020 census excel file.
#'
#' @concept downloaddata
#'
#' @export
#'

download_2020_census <- function(type){

  # check if data/ folder exists in working directory
  if(!file.exists("data/2020census")){

    # if not, create data folder
    dir.create("data/2020census")

  }

  if(type == "total"){
    # download the files from the Japanese Government Statistics Portal (e-Stat)
    ## total population
    download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032092710&fileKind=0",
                  "data/2020census/a01.xlsx")
    # return the data frame
    census_2020 <- readxl::read_excel("data/2020census/a01.xlsx")

  } else if(type == "foreigner"){
    ## foreigners
    download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032092574&fileKind=0",
                  "data/2020census/z01.xlsx")
    # return the data frame
    census_2020 <- readxl::read_excel("data/2020census/z01.xlsx")

  } else{
    census_2020 <- stop("Specify data type either 'total' or 'foreigner'. ")
  }

  return(census_2020)
}
