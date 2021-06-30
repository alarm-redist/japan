#' Download 2015 Population Data per Each Old Municipality
#'
#' @param pref_code administrative code for prefecture of choice (eg. Hokkaido: 01, Okinawa: 47)
#'
#' @return shapefile csv downloaded in data/ directory and resulting csv
#'
#' @concept downloaddata
#'
#' @export
#'

download_2015pop_old <- function(pref_code){
  pref_code <- as.numeric(pref_code)
  
  # check if data/ folder exists in working directory
  if(!file.exists("data")){
    
    # if not, create data folder
    dir.create("data")
    
  }
  
  # download the files
  if(pref_code < 16){
    #Hokkaido(pref_code: 01) to Nigata(pref_code:15)
    n <- 73281 + (pref_code - 1)*65
    n <- as.character(n)
    download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                        n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
    
  }else{
    if(pref_code == 16){
      #Toyama(pref_code: 16) is an exception
      n <- 72179 + pref_code
      n <- as.character(n)
      download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                          n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
      
    }else{
      if(pref_code > 16 & pref_code < 31){
        #Ishikawa (pref_code:17) to Wakayama (pref_code:30)
        n <- 72300 + (pref_code - 17)*65
        n <- as.character(n)
        download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                            n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
        
      }else{
        if(pref_code == 31){
          #Tottori (pref_code: 31) is an exception
          n <- 69936 + pref_code
          n <- as.character(n)
          download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                              n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
         
          }else{
          if(pref_code > 31 & pref_code < 47){
            #Shimane (pref_code: 32) to Kagoshima (pref_code: 46)
            n <- 71152 + (pref_code - 32)*65
            n <- as.character(n)
            download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                                n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
            
          }else{
            #Okinawa(pref_code: 47) is an exception
            n <- 72080 + pref_code
            n <- as.character(n)
            download.file(paste('https://www.e-stat.go.jp/stat-search/file-download?statInfId=0000314' , 
                                n, '&fileKind=1', sep = ''), 'data/2015pop_old.csv')
          }
        }
      }
    }
  }
  
  # read csv
  pop_by_old_boundary <- read.csv("data/2015pop_old.csv", fileEncoding = "shift-jis")
  
  return(pop_by_old_boundary)
}
