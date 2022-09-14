new_districts <- read.csv(here::here("analyses/13_tokyo/new_enacted_plan/13_tokyo_new_districts.csv"))

new_enacted <- bind_rows(
  pref_mutual %>%
    select(mun_code.x, sub_code, pop, geometry, sub_name) %>%
    rename(code = mun_code.x) %>%
    mutate(code = as.numeric(code)),

  pref_freeze

) %>%
  arrange(code, sub_code) %>%
  sf::st_as_sf()

new_enacted <- merge_gun(new_enacted)

# Choose 郡 to merge
gun_codes <- unique(new_enacted$gun_code[which(new_enacted$gun_code >= (new_enacted$code[1]%/%1000)*1000+300)])
gun_codes <- setdiff(gun_codes, gun_exception) # Filter out exceptions

# Set aside non-郡 municipalities
pref_non_gun <- dplyr::filter(new_enacted, gun_code %in% gun_codes == FALSE)

# Merge together 郡
pref_gun <- NULL
for(i in 1:length(gun_codes)){
  # filter out gun
  gun <- new_enacted %>%
    dplyr::filter(gun_code == gun_codes[i])

  # merge together gun
  gun$code <- gun_codes[i]
  gun <- gun %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

  # merge back together
  gun$sub_code <- 0
  gun$gun_code <- gun_codes[i]
  pref_gun <- dplyr::bind_rows(pref_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
new_enacted <- dplyr::bind_rows(pref_non_gun, pref_gun)

# add new districts
new_enacted <- new_enacted %>%
  left_join(new_districts, by = "code")


special_wards <- new_enacted %>%
  filter(code %in% c(13101:13123,
                     13360, 13380, 13400, 13420))

special_wards <- special_wards %>%
  mutate(new_dist =
           ifelse(code == 13111,
             # 大田区大森東特別出張所管内
             # 大田区大森西特別出張所管内
             # 大田区入新井特別出張所管内
             # 大田区馬込特別出張所管内
             # 大田区池上特別出張所管内
             # 大田区新井宿特別出張所管内
             # 大田区久が原特別出張所管内 (池上三丁目に属する区域に 限 る 。)
             # 大田区糀谷特別出張所管内
             # 大田区羽田特別出張所管内
             # 大田区六郷特別出張所管内
             #大田区矢口特別出張所管内 (矢口二丁目(一番、十三番、 十四番、二十七番及び二十八 番に限る。)及び矢口三丁目 (一番及び八番に限る。)に 属する区域に限る。)
             # 大田区蒲田西特別出張所管内
             # 大田区蒲田東特別出張所管内
             ifelse(sub_code %in% c(10, 20, 30, 40, 50, 80, 630,
                                     70, 60, 600, 100, 590, 580, 90,
                                     110, 120, 130, 140, 150, 160, 170,
                                     360, 370, 380, 390, 400, 410, 420, 430,
                                     450, 440, 470, 460, 480, 500,
                                     530, 520, 540, 510, 550, 560, 570),
                     4,
                     26),
             ifelse(code == 13112,
                     # 世田谷区池尻まちづくりセン ター管内
                     # 世田谷区太子堂まちづくりセ ンター管内
                     # 世田谷区若林まちづくりセン ター管内
                     # 世田谷区上町まちづくりセン ター管内
                     # 世田谷区下馬まちづくりセンター管内
                     # 世田谷区上馬まちづくりセン ター管内
                     # 世田谷区代沢まちづくりセン ター管内
                     # 世田谷区奥沢まちづくりセン ター管内
                     # 世田谷区九品仏まちづくりセ ンター管内
                     # 世田谷区等々力まちづくりセ ンター管内
                     # 世田谷区上野毛まちづくりセ ンター管内
                     # 世田谷区用賀まちづくりセン ター管内
                     # 世田谷区二子玉川まちづくり センター管内
                     # 世田谷区深沢まちづくりセン ター管内
                    ifelse(sub_code %in% c(10, 20, 30, 40, 50,
                                             60, 70, 80, 120, 130,
                                             140, 150, 190, 270, 260, 280,
                                             290, 300, 310, 320, 330, 340,
                                             350, 360, 390, 380, 370,
                                             400, 420, 410, 430
                                             ),
                             5,
                             6),
                    ifelse(code == 13115,
                             # 下高井戸一丁目、下高井戸二 丁目、下高井戸三丁目、下高 井戸四丁目、下高井戸五丁目、
                             # 永福一丁目(二番から四十四 番までに限る。)、永福二丁 目、永福三丁目、永福四丁目、
                             # 浜田山一丁目、浜田山二丁目、 浜田山三丁目、浜田山四丁目、
                             # 大宮二丁目(五番から十八番 までに限る。)、
                             # 高円寺南二 丁目、高円寺南三丁目、高円 寺南四丁目、
                             # 高円寺北二丁目、 高円寺北三丁目、高円寺北四丁目、
                             # 阿佐谷南一丁目、阿佐 谷南二丁目、阿佐谷南三丁目、
                             # 阿佐谷北一丁目、阿佐谷北二 丁目、阿佐谷北三丁目、阿佐 谷北四丁目、阿佐谷北五丁目、 阿佐谷北六丁目、
                             # 天沼一丁目、 天沼二丁目、天沼三丁目、
                             # 本 天沼一丁目、本天沼二丁目、 本天沼三丁目、
                             # 成田西一丁目、 成田西二丁目、成田西三丁目、 成田西四丁目、
                             # 成田東一丁目、 成田東二丁目、成田東三丁目、 成田東四丁目、成田東五丁目、
                             # 荻窪一丁目、荻窪二丁目、荻 窪三丁目、荻窪四丁目、荻窪 五丁目、
                             # 南荻窪一丁目、南荻 窪二丁目、南荻窪三丁目、南 荻窪四丁目、
                             # 上荻一丁目、上 荻二丁目、上荻三丁目、上荻 四丁目、
                             # 西荻南一丁目、西荻 南二丁目、西荻南三丁目、西 荻南四丁目、
                             # 西荻北一丁目、 西荻北二丁目、西荻北三丁目、 西荻北四丁目、西荻北五丁目、
                             # 今川一丁目、今川二丁目、今 川三丁目、今川四丁目、
                             # 清水 一丁目、清水二丁目、清水三 丁目、
                             # 桃井一丁目、桃井二丁 目、桃井三丁目、桃井四丁目、
                             # 井草一丁目、井草二丁目、井 草三丁目、井草四丁目、井草 五丁目、
                             # 下井草一丁目、下井 草二丁目、下井草三丁目、下 井草四丁目、下井草五丁目、
                             # 上井草一丁目、上井草二丁目、 上井草三丁目、上井草四丁目、
                             # 善福寺一丁目、善福寺二丁目、 善福寺三丁目、善福寺四丁目、
                             # 松庵一丁目、松庵二丁目、松 庵三丁目、
                             # 宮前一丁目、宮前二丁目、宮前三丁目、宮前四 丁目、宮前五丁目、
                             # 久我山一 丁目、久我山二丁目、久我山 三丁目、久我山四丁目、久我 山五丁目、
                             # 高井戸東一丁目、 高井戸東二丁目、高井戸東三 丁目、高井戸東四丁目、
                             # 高井 戸西一丁目、高井戸西二丁目 高井戸西三丁目、
                             # 上高井戸一 丁目、上高井戸二丁目、上高 井戸三丁目
                           ifelse(sub_code %in% c(30, 40, 50, 90, 110, 120, 130, 140,
                                                     150, 160, 170, 180, 190, 200,
                                                     210, 220, 230, 240, 250, 260, 270,
                                                     280, 290, 300, 310, 320, 330, 340, 350, 360),
                                     8,
                                     27),
                           ifelse(code == 13119,
                                  # 本庁管内
                                  # 板橋一丁目、板橋二丁目、 板橋三丁目、板橋四丁目、
                                  # 加賀一丁目、加賀二丁目、
                                  # 大山東町、大山金井町、
                                  # 熊 野町、中丸町、南町、稲荷 台、仲宿、氷川町、栄町、
                                  # 大山町、大山西町、幸町、 中板橋、仲町、
                                  # 弥生町、本 町、大和町、双葉町、富士 見町、
                                  # 大谷口上町、大谷口 北町、大谷口一丁目、大谷 口二丁目、
                                  # 向原一丁目、向 原二丁目、向原三丁目、
                                  # 小 茂根一丁目、小茂根二丁目、 小茂根三丁目、小茂根四丁 目、小茂根五丁目、
                                  # 常盤台 一丁目、常盤台二丁目、常 盤台三丁目、常盤台四丁目、
                                  # 南常盤台一丁目、南常盤台 二丁目、
                                  # 東新町一丁目、東 新町二丁目、
                                  # 上板橋一丁目、 上板橋二丁目、上板橋三丁 目、
                                  # 清水町、蓮沼町、大原 町、泉町、宮本町、
                                  # 志村一 丁目、志村二丁目、志村三 丁目、
                                  # 坂下一丁目(一番か ら二十六番まで及び二十八 番に限る。)、
                                  # 東坂下一丁 目、小豆沢一丁目、小豆沢 二丁目、小豆沢三丁目、小豆沢四丁目、
                                  # 西台一丁目、 西台二丁目、西台三丁目、 西台四丁目、
                                  # 中台一丁目、 中台二丁目、中台三丁目、
                                  # 若木一丁目、若木二丁目、 若木三丁目、
                                  # 前野町一丁目、 前野町二丁目、前野町三丁 目、前野町四丁目、前野町 五丁目、前野町六丁目、
                                  # 三 園二丁目、東山町、
                                  # 桜川一 丁目、桜川二丁目、桜川三 丁目
                                  # 東京都板橋区赤塚支所管内
                                  ifelse(sub_code %in% c(seq(from = 10, to = 420, by = 10),
                                                  450, 500, 530, 540,
                                                  460, 470, 510, 480, 520),
                                  11,
                                  12),
                                  ifelse(code == 13120,
                                          # 貫井四丁目(二十八番、二十 九番四号、二十九番八号から 二十九番二十二号まで、三十 番九号、三十番十号、四十四 番から四十六番まで、四十七 番十八号から四十七番四十八 号まで及び四十七番五十号か ら四十七番五十二号までに限 る。)、
                                          # 高松六丁目、
                                          # 土支田 一丁目、土支田二丁目、土支 田三丁目、土支田四丁目、
                                          # 富 士見台一丁目、富士見台二丁 目、富士見台三丁目(二十番 六号から二十番十号まで、三 十八番から四十六番まで、四 十七番五号から四十七番七号 まで、五十五番六号から五十 五番十七号まで及び五十六番 から六十三番までに限る。)、 富士見台四丁目、
                                          # 南田中一丁 目、南田中二丁目、南田中三 丁目、南田中四丁目、南田中 五丁目、
                                          # 高野台一丁目、高野 台二丁目、高野台三丁目、高 野台四丁目、高野台五丁目、
                                          # 谷原二丁目、谷原三丁目、谷 原四丁目、谷原五丁目、谷原六丁目、
                                          # 三原台一丁目、三原 台二丁目、三原台三丁目、
                                          # 石 神井町一丁目、石神井町二丁 目、石神井町三丁目、石神井 町四丁目、石神井町五丁目、 石神井町六丁目、石神井町七 丁目、石神井町八丁目、
                                          # 石神 井台一丁目、石神井台二丁目、 石神井台三丁目、石神井台四 丁目、石神井台五丁目、石神 井台六丁目、石神井台七丁目、 石神井台八丁目、
                                          # 下石神井一 丁目、下石神井二丁目、下石 神井三丁目、下石神井四丁目、 下石神井五丁目、下石神井六 丁目、
                                          # 東大泉一丁目、東大泉 二丁目、東大泉三丁目、東大 泉四丁目、東大泉五丁目、東 大泉六丁目、東大泉七丁目、
                                          # 西大泉町、
                                          # 西大泉一丁目、西 大泉二丁目、西大泉三丁目、 西大泉四丁目、西大泉五丁目、 西大泉六丁目、
                                          # 南大泉一丁目、 南大泉二丁目、南大泉三丁目、 南大泉四丁目、南大泉五丁目、 南大泉六丁目、
                                          # 大泉町一丁目、 大泉町二丁目、大泉町三丁目、 大泉町四丁目、大泉町五丁目、 大泉町六丁目、
                                          # 大泉学園町一 丁目、大泉学園町二丁目、大 泉学園町三丁目、大泉学園町 四丁目、大泉学園町五丁目、 大泉学園町六丁目、大泉学園 町七丁目、大泉学園町八丁目、 大泉学園町九丁目、
                                          # 関町北一 丁目、関町北二丁目、関町北 三丁目、関町北四丁目、関町 北五丁目、
                                          # 関町南一丁目、関 町南二丁目、関町南三丁目、 関町南四丁目、
                                          # 上石神井南町、
                                          # 立野町、
                                          # 上石神井一丁目、上 石神井二丁目、上石神井三丁 目、上石神井四丁目、
                                          # 関町東 一丁目、関町東二丁目
                                         ifelse(sub_code %in% c(210, 260, 270, 280, 290, 300, 310, 320, 330,
                                                  360, 410, 420, 430, 440, 450, 460,
                                                  390, 400, 350, 370, 340, 380),
                                                9,
                                                28),
                                         ifelse(code == 13121,
                                            # 青井一丁目、青井二丁目、青 井三丁目、青井四丁目、青井 五丁目、青井六丁目、
                                            # 足立一 丁目、足立二丁目、足立三丁 目、足立四丁目、
                                            # 綾瀬一丁目、 綾瀬二丁目、綾瀬三丁目、綾 瀬四丁目、綾瀬五丁目、綾瀬 六丁目、綾瀬七丁目、
                                            # 梅島一 丁目、梅島二丁目、梅島三丁 目、
                                            # 梅田一丁目、梅田二丁目、 梅田三丁目、梅田四丁目、梅 田五丁目、梅田六丁目、梅田 七丁目、梅田八丁目、
                                            # 大谷田 一丁目、大谷田二丁目、大谷 田三丁目、大谷田四丁目、大 谷田五丁目、
                                            # 加平一丁目、加 平二丁目、加平三丁目、
                                            # 北加 平町、
                                            # 栗原一丁目、栗原二丁 目、
                                            # 弘道一丁目、弘道二丁目、
                                            # 佐野一丁目、佐野二丁目、
                                            # 島 根一丁目、島根二丁目、島根三丁目、島根四丁目、
                                            # 神明一 丁目、神明二丁目、神明三丁 目、
                                            # 神明南一丁目、神明南二 丁目、
                                            # 関原一丁目、関原二丁 目、関原三丁目、
                                            # 千住一丁目 千住二丁目、千住三丁目、千 住四丁目、千住五丁目、
                                            # 千住 曙町、
                                            # 千住旭町、
                                            # 千住東一丁 目、千住東二丁目、
                                            # 千住大川 町、
                                            # 千住河原町、
                                            # 千住寿町、
                                            # 千住桜木一丁目、千住桜木二 丁目、
                                            # 千住関屋町、
                                            # 千住龍田 町、
                                            # 千住中居町、
                                            # 千住仲町、
                                            # 千住橋戸町、
                                            # 千住緑町一丁目 千住緑町二丁目、千住緑町三 丁目、
                                            # 千住宮元町、
                                            # 千住元町
                                            # 千住柳町、
                                            # 竹の塚一丁目、竹 の塚二丁目、竹の塚三丁目、 竹の塚四丁目、竹の塚五丁目 竹の塚六丁目、竹の塚七丁目
                                            # 辰沼一丁目、辰沼二丁目、
                                            # 中 央本町一丁目、中央本町二丁 目、中央本町三丁目、中央本 町四丁目、中央本町五丁目、
                                            # 東和一丁目、東和二丁目、東 和三丁目、東和四丁目、東和 五丁目、
                                            # 中川一丁目、中川二 丁目、中川三丁目、中川四丁 目、中川五丁目、
                                            # 西綾瀬一丁 目、西綾瀬二丁目、西綾瀬三 丁目、西綾瀬四丁目、
                                            # 西新井 栄町一丁目、西新井栄町二丁 目、
                                            # 西加平一丁目、西加平二 丁目、
                                            # 西保木間一丁目、西保 木間二丁目、西保木間三丁目 西保木間四丁目、
                                            # 花畑一丁目 花畑二丁目、花畑三丁目、花 畑四丁目、花畑五丁目、花畑 六丁目、花畑七丁目、花畑八 丁目、
                                            # 東綾瀬一丁目、東綾瀬二丁目、東綾瀬三丁目、
                                            # 東保 木間一丁目、東保木間二丁目、
                                            # 東六月町、
                                            # 一ツ家一丁目、一 ツ家二丁目、一ツ家三丁目、 一ツ家四丁目、
                                            # 日ノ出町、
                                            # 平 野一丁目、平野二丁目、平野 三丁目、
                                            # 保木間一丁目、保木 間二丁目、保木間三丁目、保 木間四丁目、保木間五丁目、
                                            # 保塚町、
                                            # 南花畑一丁目、南花 畑二丁目、南花畑三丁目、南 花畑四丁目、南花畑五丁目、
                                            # 六木一丁目、六木二丁目、六 木三丁目、六木四丁目、
                                            # 谷中 一丁目、谷中二丁目、谷中三 丁目、谷中四丁目、谷中五丁 目、
                                            # 柳原一丁目、柳原二丁目、
                                            # 六月一丁目、六月二丁目、六 月三丁目、
                                            # 六町一丁目、六町 二丁目、六町三丁目、六町四 丁目
                                            ifelse(sub_code %in% c(10, 20, 30, 120, 110,
                                                      140, 180, 190, 200,
                                                      240, 250, 280, 290, 300,
                                                      320, 330, 340, 350, 360, 370,
                                                      380, 390, 400, 410, 420, 430,
                                                      440, 450, 460, 470, 480, 490,
                                                      500, 510, 520, 540, 570, 580,
                                                      630, 610, 660, 670, 680, 730,
                                                      750, 710, 720, 740, 760, 770,
                                                      790, 810, 870, 890, 900, 910),
                                                   13,
                                                   29),
                                         ifelse(code == 13123,
                                            # 本庁管内
                                            # 中央四丁目、
                                            # 松島一丁目、 松島二丁目、松島三丁目、 松島四丁目、
                                            # 東小松川一丁 目、東小松川二丁目、東小 松川三丁目、東小松川四丁 目、
                                            # 西小松川町、
                                            # 興宮町、
                                            # 上一色一丁目、上一色二丁 目、上一色三丁目、
                                            # 本一色 一丁目、本一色二丁目、本 一色三丁目
                                            # 江戸川区小松川事務所管内
                                            # 江戸川区小岩事務所管内
                                            ifelse(sub_code %in% c(40, 60, 70, 280, 290, 300,
                                                                   11, 12, 20,
                                                                   200, 210, 220, 230),
                                                   14,
                                                   16),
                                            ifelse(code %in% c(13360, 13380, 13400, 13420),
                                                   3,
                                                   new_dist))
                                         )
                                  )
                           )
                    )
             )
           )
  )

### Tama ###
tama <- filter(new_enacted, code %in% c(13101:13123, 13360, 13380, 13400, 13420) == FALSE)
tama$new_dist[tama$code == 13300] <- 25

# 下柚木、下柚木二丁目、下柚 木三丁目、上柚木、上柚木二 丁目、上柚木三丁目、
# 中山(五 百十九番地、五百二十三番地 から五百二十六番地まで、
# 八 百十九番地から八百三十番地 まで、八百四十二番地、
# 八百 七十五番地から八百七十八番 地まで、八百八十番地から千百四十八番地まで、
# 千百五十 六番地、千二百十九番地及び 千二百二十一番地を除く。)、
# 越野、南陽台一丁目、南陽台 二丁目、南陽台三丁目、
# 堀之 内、堀之内二丁目、堀之内三 丁目、東中野、大塚、鹿島、
# 松が谷、
# 鑓水(三百三十九番 地から三百四十五番地まで、 三百六十四番地から三百七十 一番地まで及び三百九十六番 地を除く。)、
# 鑓水二丁目、 南大沢一丁目、南大沢二丁目、 南大沢三丁目、南大沢四丁目、 南大沢五丁目、
# 松木、別所一 丁目、別所二丁目

tama <- tama %>%
  mutate(new_dist =
           if_else(code == 13201,
                   if_else(sub_code %in% c(450, 460, 470, 510, 550,
                                       560, 570, 580, 590, 600,
                                       480, 500, 520, 540) == TRUE,
                           21,
                           24),
                   new_dist)
         )




### PLOT ###
#######Special wards###############
# Make adjacency list
special_wardsadj <- redist::redist.adjacency(special_wards)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(special_wards)
  special_wardsadj <- geomander::add_edge(special_wardsadj,
                                          ferries[, 1],
                                          ferries[, 2],
                                          zero = TRUE)
}

# Suggest connection between disconnected groups
suggest_special_wards <-  geomander::suggest_component_connection(shp = special_wards,
                                                                  adj = special_wardsadj)
special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        suggest_special_wards$x,
                                        suggest_special_wards$y,
                                        zero = TRUE)

# Repair adjacencies
special_wards_add_edge <-
  matrix(c(
    #大田区東海-大田区平和島
    which(special_wards$code == 13111 & special_wards$sub_code == 580),
    which(special_wards$code == 13111 & special_wards$sub_code == 90),
    #大田区東海-大田区京浜島
    which(special_wards$code == 13111 & special_wards$sub_code == 580),
    which(special_wards$code == 13111 & special_wards$sub_code == 600),
    #大田区昭和島-大田区大森東
    which(special_wards$code == 13111 & special_wards$sub_code == 100),
    which(special_wards$code == 13111 & special_wards$sub_code == 20),
    #品川区八潮-品川区東八潮
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 270),
    #品川区八潮-品川区東大井
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 160),
    #品川区八潮-品川区東品川5丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[2],
    #品川区東品川1・3・4丁目 - 品川区東品川2丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[1],
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[3],
    #品川区東品川2丁目 - 品川区東品川5丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[3],
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[2],
    #港区海岸-港区台場
    which(special_wards$code == 13103 & special_wards$sub_code == 20),
    which(special_wards$code == 13103 & special_wards$sub_code == 300)[1],
    #中央区佃・月島・勝鬨-中央区の大部分の地域
    which(special_wards$code == 13102)[1],
    which(special_wards$code == 13102)[2],
    #中央区浜離宮庭園の各地域
    which(special_wards$code == 13102)[1],
    which(special_wards$code == 13102)[4]
  ), ncol = 2, byrow = TRUE)

#Add edges
special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        special_wards_add_edge[,1],
                                        special_wards_add_edge[,2])
# Set Colors for Plot
new_special_wards_map <- special_wards %>%
  redist_map(existing_plan = new_dist,
             adj = special_wardsadj) %>%
  mutate(color = redist:::color_graph(.$adj, as.integer(.$new_dist))) %>%
  filter(code %in% c(13360, 13380, 13400, 13420) == FALSE)


## Color Palette
PAL <- c('#6D9537', '#364B7F', '#DCAD35', '#9A9BB9', '#2A4E45', '#7F4E28')

# Plot Map
ggplot() +
  geom_sf(data = new_special_wards_map, aes(fill = factor(color)), color = NA) +
  scale_fill_manual(values = PAL, guide = "none") +

  geom_sf(data = boundary_special_wards, show.legend = "line",
          fill = NA, lwd = 0.4) +
  scale_color_manual(values = c("#373C38")) +

  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())



### Plot tama##
# Make adjacency list
tamaadj <- redist::redist.adjacency(tama)

# No-ferry-related adjacencies to add in Tama
# Define pref_map object
# Set Colors for Plot
new_tama_map <- tama %>%
  redist_map(existing_plan = new_dist,
             adj = tamaadj) %>%
  mutate(color = redist:::color_graph(.$adj, as.integer(.$new_dist)))

# Plot Map
ggplot() +
  geom_sf(data = new_tama_map, aes(fill = factor(color)), color = NA) +
  scale_fill_manual(values = PAL, guide = "none") +

  geom_sf(data = boundary_tama, show.legend = "line",
          fill = NA, lwd = c(0.4, 1.2)) +
  scale_color_manual(values = c("#373C38", "#D0C1BA50")) +

  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names), color = "white", size = 3,
               nudge_x = 0, # adjust the position of the labels
               nudge_y = -2000, # adjust the position of the labels
               family = "HiraginoSans-W3") +

  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())
