#' Merge "Gun" of Choice
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param exception an array of codes for municipalities to keep separate
#'
#' @return an sf object with the chosen municipalities grouped together as "gun" in the `gun_code` column
#'
#' @concept getdata
#'
#' @export
#'

merge_gun <- function(pref, exception = NULL){

  #Determine the prefecture code (Ex: Kyoto: 26)
  pref_10 <- pref$code[1] %/% 1000 #2-digit prefecture code
  pref_code <- pref_10*1000 #5-digits prefecture code


  if(pref_code == 1000){
    ##This function does not work for Hokkaido and Okinawa.
    print("Use merge_gun_hokkaido().")

  }else{
    if(pref_code == 47000){
      print("Use merge_gun_okinawa()." )

    }else{
      if(missing(exception)){
        #If no exceptions are set, all the municipalities that belong to a "gun" are grouped together

        #obtain the last 3 digits of the municipality codes
        pref_gun <- pref %>%
          mutate(municipality_code = code) %>%
          mutate(gun_code = code)


        for(i in 1:length(pref_gun$municipality_code)){
          if(pref_gun$municipality_code[i] < 300){
            #the municipality codes of all the municipalities that do not belong to "gun" are kept the same
            pref_gun$gun_code[i] <-  pref_gun$municipality_code[i]
          }

          else{
            #group together the municipalities that belong to a "gun"
            pref_gun$gun_code[i] <-  cut(pref_gun$municipality_code[i],
                                 breaks = c(300, 320, 340, 360, 380,
                                            400, 420, 440, 460, 480,
                                            500, 520, 540, 560, 580,
                                            600, 620, 640, 660, 680,
                                            700, 720, 740, 760, 780, 800))
          }
        }


        #relabel the municipality codes of the municipalities that belong to "gun"
        for(i in 1:20){
          pref_gun$gun_code[pref_gun$gun_code == i] <- gun_code + 300 + 20*(i-1)
        }


        #delete the unncessary column
        pref_gun <- subset(pref_gun, select = - municipality_code)
        # return the results
        return(pref_gun)
      }


      else{
        #Create a dataframe without any data
        pref_interm <- pref %>%
          mutate(gun_code = code)
        pref_separate <- pref_interm[ !(pref_interm$code %in% pref_interm$code), ]

        #Create a data frame that contains all the municipalities that belong to a "gun" that the user does not wish to group together
        for(i in 1:length(exception)){
          mod <- exception %% pref_code
          div_twenty <- mod %/% 20
          separate_gun_code <- pref_code + div_twenty * 20
          pref_to_separate <- pref_interm %>%
            filter(between(code, separate_gun_code[i], separate_gun_code[i] + 19))
          pref_separate <- dplyr::bind_rows(pref_to_separate, pref_separate)

        }

        #Create a data frame that includes the municipalities that need to be grouped together
        pref_to_group <- setdiff(pref_interm, pref_separate)
        pref_to_group <- pref_to_group %>%
          mutate(municipality_code = code %% pref_code)

        #the municipality codes of all the municipalities that do not belong to "gun" are kept the same
        for(i in 1:length(pref_to_group$municipality_code)){
          if(pref_to_group$municipality_code[i] < 300){
            pref_to_group$gun_code[i] <-  pref_to_group$gun_code[i]

          }

          #group together the municipalities that belong to a "gun"
          else{
            pref_to_group$gun_code[i] <-  cut(pref_to_group$municipality_code[i],
                                          breaks = c(300, 320, 340, 360, 380,
                                                     400, 420, 440, 460, 480,
                                                     500, 520, 540, 560, 580,
                                                     600, 620, 640, 660, 680,
                                                     700, 720, 740, 760, 780, 800))
          }
        }


        #relabel the municipality codes of the municipalities that belong to "gun"
        for(i in 1:20){
          pref_to_group$gun_code[pref_to_group$gun_code == i] <- pref_code + 300 + 20*(i-1)
        }


        #delete the unnecessary column
        pref_to_group = subset(pref_to_group, select = - municipality_code)

        #return results
        pref_gun <- dplyr::bind_rows(pref_to_group, pref_separate)
        return(pref_gun)
      }
    }
  }
}

