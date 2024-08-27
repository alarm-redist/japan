#' Assign codes for "Gun"
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#'
#' @return an sf object with a `gun_code` column
#'
#' @concept assign codes
#'
#' @export
#'

merge_gun <- function(pref){

  # Determine the prefecture code
  pref_10 <- pref$code[1] %/% 1000 # 2-digit prefecture code
  pref_code <- pref_10*1000 # 5-digit prefecture code

  if(pref_code %in% c(1000, 47000)){
    print("This function does not work for Hokkaido and Okinawa.")

  }else{

    # Obtain the last 3 digits of the municipality codes
    pref_gun_code <- pref %>%
      mutate(gun_code = code %% pref_code)

    for(i in 1:length(pref_gun_code$gun_code)){
      if(pref_gun_code$gun_code[i] < 300){
        # Don't assign `gun_code` to municipalities that do not belong to "gun"
        pref_gun_code$gun_code[i] <-  pref_10*10000 + i
      }

      else{
        # Assign codes to municipalities that belong to "gun"
        pref_gun_code$gun_code[i] <-  cut(pref_gun_code$gun_code[i],
                                          breaks = c(300, 320, 340, 360, 380,
                                                     400, 420, 440, 460, 480,
                                                     500, 520, 540, 560, 580,
                                                     600, 620, 640, 660, 680,
                                                     700, 720, 740, 760, 780,
                                                     800))
      }
    }

    # Relabel the `gun_code`
    for(i in 1:20){
      pref_gun_code$gun_code[pref_gun_code$gun_code == i] <- pref_code + 300 + 20*(i-1)
    }

    # Return sf object
    pref_gun_code <- sf::st_as_sf(pref_gun_code)

    return(pref_gun_code)
  }
}

