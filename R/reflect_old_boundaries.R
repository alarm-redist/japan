#' Download 2015 Population Data per Each Old Municipality
#'
#' @param pref  sf object of cleaned census data for a prefecture
#' @param old_boundary  sf object of administrative boundaries as of the year 2000
#' @param pop_by_old_boundary  sf object of population per each former-city
#' @param old_code  the codes of former-cities that were later merged together
#' @param new_code  the code of the new city that was created as a result of gappei
#'
#' @return shapefile csv downloaded in data/ directory and resulting csv
#'
#' @concept downloaddata
#'
#' @export
#'

###########Notes(to be deleted once we establish workflow)#########################
#<How to Match Old Municipalities with New Merged Ones (after Gappei)>
#0. Run clean_jcdf on 2015(2020) census. Ex: toyama <- clean_jcdf(toyama_raw)
#1. Run merge_small on 2015(2020) census. Ex: toyama <- merge_small(toyama, split_codes = c(16201))
#2. Download the old shapefile that reflect the boundaries as of 2000 using download_old_shp().
#   Ex: toyama_old_boundary <- download_old_shp(16)
#   equivalent to : old_boundary <- sf::st_read("data/N03-001001_16-g_AdministrativeBoundary.shp")
#3. Download the 2015 Census data that shows the population as of 2015 per each old municipality
#   Ex: toyama_2015pop_old <- download_2015pop_old(16)
#   equivalent to : pop_by_old_boundary <- read.csv("data/001_16.csv", fileEncoding = "shift-jis")
#4.Specify the codes of the old municipalities that presently belong to a *single* municipality.
#Use https://www.e-stat.go.jp/municipalities/cities to check the codes of the old municipalities.
####Run step 4 as many times as you need to, depending on how many municipalities you wish to split.
#5. Run merge_gun


reflect_old_boundaries <- function(pref, old_boundary, pop_by_old_boundary, old_code, new_code){

  #code of old municipalities: convert input into character; make sure that code is 5 digits
  if(nchar(old_code[1]) == 4){
    old_code_char <- paste0(0, as.character(old_code))
  }else{
    old_code_char <- as.character(old_code)
  }

  #code of new municipalities: convert input into character; make sure that code is 5 digits
  if(nchar(new_code) == 4){
    new_code_char <- paste0(0, as.character(new_code))
  }else{
    new_code_char <- as.character(new_code)
  }

  #Match CRS
  sf::st_crs(old_boundary) <- sf::st_crs(pref)

  #filter out the municipalities that will not be split
  #(i.e. no need to take into account old boundaries)
  post_gappei_except_for_designated_city <- pref %>%
      dplyr::filter(code != as.numeric(new_code))

  #Clean the data on old boundaries
  pre_gappei_geom <- old_boundary %>%
    dplyr::filter(N03_007 %in% old_code_char) %>%
    dplyr::select(N03_004, N03_007, geometry)
  names(pre_gappei_geom) <- c("municipality", "pre_gappei_code", "geometry")
  pre_gappei_geom <- sf::st_make_valid(pre_gappei_geom)
  pre_gappei_geom <- pre_gappei_geom %>%
    dplyr::group_by(pre_gappei_code) %>%
    dplyr::summarise(geometry =  sf::st_union(geometry))

  #get rid of unnecessary rows and columns
  cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
    slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
    select(X, X.1, X.4, X.5)
  #column X: municipality code; X.1:shikibetsu code(9 corresponds to the old municipalities)
  #column X.4: name of old municipality; X.5: population as of 2015

  #select the data we are interested in
  pop_data <- cleaned_pop_by_old_boundary %>%
    filter(X %in% new_code_char) %>%
    filter(X.1 %in% 9) %>%
    #"9" corresponds to the old municipalities (cf.shikibetsu code)
    select(X, X.4, X.5)
  names(pop_data) <- c("code", "municipality", "pop")
  #obtain the last three digits of the old municipality codes
  pop_data <- pop_data %>%
    separate(municipality, into = c("a", "pre_code", "b"), sep = " ") %>%
    select(code, pre_code, pop)
  pop_data$code <- as.numeric(pop_data$code)


  #generate functioning municipality codes
  #(i.e. add the first two digits)
  pref_10<- pref$code[1] %/% 1000 #prefecture_code
  a <- lapply(pref_10,  paste0, pop_data$pre_code)
  b <- as.data.frame(a, col.names = "pre_gappei_code")

  #final version of 2015(2020) population based on old boundaries
  pop_data <- bind_cols(b, pop_data)
  pop_data = subset(pop_data, select = -c(pre_code))
  #make old code into character vector
  if(nchar(pop_data$pre_gappei_code[1]) == 4){
    pop_data$pre_gappei_code <- as.character(paste0(0, as.character(pop_data$pre_gappei_code)))
  }else{
    pop_data$pre_gappei_code  <- as.character(pop_data$pre_gappei_code )
  }

  #match with dataframes based on municipality code
  old_joined <- merge(pre_gappei_geom, pop_data, by = "pre_gappei_code")

  #prepare to merge together with original dataframe
  old_joined$pre_gappei_code <- as.numeric(old_joined$pre_gappei_code)
  old_joined$pop <- as.numeric(old_joined$pop)
  if("gun_code" %in% colnames(post_gappei_except_for_designated_city)){
    old_joined$gun_code <- old_joined$pre_gappei_code
  }
  if("koiki_code" %in% colnames(post_gappei_except_for_designated_city)){
      old_joined$koiki_code <- rep(pref$koiki_code[which(pref$code == old_joined$code[1])],
                                   times = length(old_joined$code))
  }
  old_joined <- as_tibble(old_joined)

  if("pre_gappei_code" %in% colnames(post_gappei_except_for_designated_city)){
    post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$pre_gappei_code
  }else{
    post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$code
  }

  #merge with the data that excludes the designated city
  merged <- dplyr::bind_rows(old_joined, post_gappei_except_for_designated_city)

  merged <- sf::st_as_sf(merged)

  return(merged)
}




