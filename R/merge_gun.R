#Note: This function groups together the municipalities that belong to a "gun"
#and makes it so that each "gun" is treated as if it were a single municipality.
#In cases where you would rather keep the municipalities that belong to a certain "gun" separate,
#enter 'one' of the municipality codes of the municipalities that belong to that "gun."
#The function will automatically figure out the codes of the other municipalities
#that belong to that designated "gun."

#use this function after merge_small()

#Ex: kyoto <- merge_small(kyoto, intact_codes = c(26101, 26102, 26103, 26104, 26105, 26106, 26107, 26108, 26109, 26110, 26111))
#kyoto1 <- merge_gun(kyoto)
#kyoto2 <- merge_gun(kyoto, exception =  26364)
#kyoto3 <- merge_gun(kyoto, exception = c(26364, 26343))


merge_gun <- function(pref, exception = NULL){
  #Determine the prefecture code (Ex: Kyoto: 26)
  pref_10 <- pref$code[1] %/% 1000 #2 digits
  pref_code <- pref_10*1000 #5 digits
  ########################This function does not work for Hokkaido and Okinawa.#############
  if(pref_code == 1000){
    print("Use merge_gun_hokkaido().")
  }else{
    if(pref_code == 47000){
      print("Use merge_gun_okinawa()." )
    }else{#################For all prefectures other than Hokkaido and Okinawa##########
      if(missing(exception)){
        ##########Scenario 1: No exceptions####################
        pref <- pref %>%
          mutate(gun_code = code %% pref_code)
        #the last 3 digits of the codes of all the "gun" are over 300 (Ex: 26343, 26344, etc.)
        for(i in 1:length(pref$gun_code)){
          if(pref$gun_code[i] < 300){
            pref$code[i] <-  pref$code[i]
            #the municipality codes of all the municipalities that do not belong to "gun"
            #are kept the same
          }
          else{
            pref$code[i] <-  cut(pref$gun_code[i],
                                 breaks = c(300, 320, 340, 360, 380,
                                            400, 420, 440, 460, 480,
                                            500, 520, 540, 560, 580,
                                            600, 620, 640, 660, 680,
                                            700, 720, 740, 760, 780, 800))
            #the municipality codes of the municipalities that belong to "gun"
            #need to be relabeled
          }
        }
        #relabeling the municipalitiy codes of the municipalities that belong to "gun"
        for(i in 1:20){
          pref$code[pref$code == i] <- pref_code + 300 + 20*(i-1)
        }
        #delete the unncessary column
        pref = subset(pref, select = - gun_code)
        #group together the gun and treat them as if they were municipalities
        bound <- pref %>%
          group_by(code) %>%
          summarise(geometry = sf::st_union(geometry), pop = sum(pop))
        bound <- st_as_sf(bound)
        return(bound)################
      }
      else{
        ##########Scenario 2: Exceptions exist#################
        #pref_separate does not include any data at this moment
        #this is only to create a dataframe without any data
        pref_interm <- pref
        pref_separate <- pref_interm[ !(pref_interm$code %in% pref$code), ]

        for(i in 1:length(exception)){
          mod <- exception %% pref_code
          div_twenty <- mod %/% 20
          separate_gun_code <- pref_code + div_twenty * 20
          pref_to_separate <- pref %>%
            filter(between(code, separate_gun_code[i], separate_gun_code[i] + 19))
          pref_separate <- dplyr::bind_rows(pref_to_separate, pref_separate)
          #pref_separate is a dataframe that contains all the municipalities that belong to a "gun"
          #that the user does not wish to group together
        }

        pref_to_group <- setdiff(pref, pref_separate)
        #pref_to_group includes the municipalities that need to be grouped together
        ######Same as Scenario 1###############
        pref_to_group <- pref_to_group %>%
          mutate(gun_code = code %% pref_code)
        #the last 3 digits of the codes of all the "gun" are over 300 (Ex: 26343, 26344, etc.)
        for(i in 1:length(pref_to_group$gun_code)){
          if(pref_to_group$gun_code[i] < 300){
            pref_to_group$code[i] <-  pref_to_group$code[i]
            #the municipality codes of all the municipalities that do not belong to "gun"
            #are kept the same
          }
          else{
            pref_to_group$code[i] <-  cut(pref_to_group$gun_code[i],
                                          breaks = c(300, 320, 340, 360, 380,
                                                     400, 420, 440, 460, 480,
                                                     500, 520, 540, 560, 580,
                                                     600, 620, 640, 660, 680,
                                                     700, 720, 740, 760, 780, 800))
            #the municipality codes of the municipalities that belong to "gun"
            #need to be relabeled
          }
        }
        #relabeling the municipalitiy codes of the municipalities that belong to "gun"
        for(i in 1:20){
          pref_to_group$code[pref_to_group$code == i] <- pref_code + 300 + 20*(i-1)
        }
        #delete the unncessary column
        pref_to_group = subset(pref_to_group, select = - gun_code)
        #group together the gun and treat them as if they were municipalities
        pref_grouped <- pref_to_group %>%
          group_by(code) %>%
          summarise(geometry = sf::st_union(geometry), pop = sum(pop))
        ######Same as Scenario 1###############
        bound <- dplyr::bind_rows(pref_grouped, pref_separate)
        bound <- st_as_sf(bound)
        return(bound)
      }
    }
  }
}

