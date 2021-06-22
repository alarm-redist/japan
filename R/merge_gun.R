#Note: This function groups together the municipalities that belong to a "gun"
#and makes it so that each "gun" is treated as if it were a single municipality.
#In cases where you do not wish would rather keep the municipalities that belong to 
#a "gun" separate,
#enter 'one' of the municipality code of the municipality that belong to that "gun."
#The function will automatically figure out the codes of the other municipalities 
#that belong to that "gun."
#use this function after merge_small()

#Ex: kyoto <- merge_small(kyoto, intact_codes = c(26101, 26102, 26103, 26104, 26105, 26106, 26107, 26108, 26109, 26110, 26111))
#kyoto <- merge_gun(kyoto)

merge_gun <- function(pref){
  #Determine the prefecture code (Ex: Kyoto: 26)
  pref_10<- pref$code[1] %/% 1000 #2 digits
  pref_code <- pref_10*1000 #5 digits
  
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
    summarise(geometry = st_union(geometry), pop = sum(pop))
  
  return(bound)
}

