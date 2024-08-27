#' Clean latest 2020 data frame
#'
#' @param pref_code prefecture code
#'
#' @return a data frame object of cleaned 2020 census data
#'
#' @concept getdata
#'
#' @export
#'

clean_2020_census <- function(pref_code){

  #prefecture code
  pref_code <- as.numeric(pref_code)

  #check if data/ folder exists in working directory
  if(!file.exists("data-raw/2020census")){

    # if not, create data folder
    dir.create("data-raw/2020census")

  }

  #check if 2020 census data exists in working directory
  if(!file.exists("data-raw/2020census/b02_05.xlsx")){

    #if it does not exist, download excel spreadsheet from the Japanese Government Statistics Portal
    #spreadsheet includes data for total population as well as population of Japanese nationals
    download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032142408&fileKind=0",
                  "data-raw/2020census/b02_05.xlsx")


  }

  #read the excel spreadsheet
  census2020_raw <- readxl::read_excel("data-raw/2020census/b02_05.xlsx")

  #clean dataframe
  cencus2020 <- census2020_raw %>%
    # remove non-data columns
    dplyr::slice(-(1:12)) %>%
    #select relevant columns
    dplyr::select(c(1, 2, 3, 5, 8, 10))

  #rename columns
  names(cencus2020)[1] <- "total_vs_Japanese"
  names(cencus2020)[2] <- "total_vs_by_gender"
  names(cencus2020)[3] <- "type_of_municipality"
  names(cencus2020)[4] <- "pre_gappei_code"
  names(cencus2020)[5] <- "code"
  names(cencus2020)[6] <- "pop"

  #filter out data on the size of the population of Japanese nationals
  cencus2020 <- cencus2020 %>%
    filter(total_vs_Japanese == "1_うち日本人") %>%
    filter(total_vs_by_gender == "0_総数")

  #prepare data for output
  cencus2020 <- cencus2020 %>%
    dplyr::select(type_of_municipality, pre_gappei_code, code, pop) %>%
    dplyr::summarise(type_of_municipality = type_of_municipality,
                     pre_gappei_code = as.numeric(pre_gappei_code),
                     code = as.numeric(code),
                     pop = as.numeric(pop)) %>%
    #filter out the data for the designated prefecture
    dplyr::filter(code > pref_code*1000 & code < (pref_code +1) *1000)

  return(cencus2020)

}
