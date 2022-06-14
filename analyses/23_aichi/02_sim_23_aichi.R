###############################################################################
# Simulations for `23_aichi`
# © ALARM Project, June 2021
###############################################################################

# Obtain codes of 郡 to merge
pref <- merge_gun(pref)
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
# Filter out exceptions
gun_codes <- setdiff(gun_codes, gun_exception)

# Set aside non-郡 municipalities
pref_non_gun <- dplyr::filter(pref, gun_code %in% gun_codes == FALSE)

# Merge together 郡
pref_gun <- NULL
for(i in 1:length(gun_codes)){
  # filter out gun
  gun <- pref %>%
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
pref <- dplyr::bind_rows(pref_non_gun, pref_gun)

# Converet MULTIPOLYGON to several POLYGONs
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)

new_rows[1, ]$pop <- pref[1, ]$pop

pref_sep <- new_rows

# to calculate area size, switch off the `geometry (s2)`
sf_use_s2(FALSE)
for (i in 2:nrow(pref))
{
  new_rows <- data.frame(code = pref[i, ]$code,
                         sub_code = pref[i, ]$sub_code,
                         geometry = sf::st_cast(pref[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = pref[i, ]$gun_code
  )

  # order by size of the area
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    dplyr::select(-area)

  # assign population to the largest area
  new_rows[1, ]$pop <- pref[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

pref <- sf::st_as_sf(pref_sep)

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref)
    prefadj <- geomander::add_edge(prefadj,
                                   ferries[, 1],
                                   ferries[, 2],
                                   zero = TRUE)
}

# Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# Repair adjacencies 1
pref_add_edge_1 <-
  matrix(c(
    #西尾市東幡豆町(本土)-西尾市東幡豆町前島
    which(pref$code == 23213 & pref$sub_code == 7520)[1],
    which(pref$code == 23213 & pref$sub_code == 7520)[2],
    #西尾市東幡豆町(本土)-西尾市東幡豆町沖島
    which(pref$code == 23213 & pref$sub_code == 7520)[1],
    which(pref$code == 23213 & pref$sub_code == 7520)[3],
    #知多郡-知多郡島嶼部
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[4],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[7],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[8],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[9],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[10]
), ncol = 2, byrow = TRUE)

#Add edges 1
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge_1[,1],
                               pref_add_edge_1[,2])

# Repair adjacencies 2
pref_add_edge_2 <-
  matrix(c(
    #南陽町大字七島新田-港区当知町草野
    which(pref$code == 23111 & pref$sub_code == 2050),
    which(pref$code == 23111 & pref$sub_code == 1290)[1],

    #港区中川本町-港区厚田前新田
    which(pref$code == 23111 & pref$sub_code == 1360),
    which(pref$code == 23111 & pref$sub_code == 50),

    #港区中川本町-港区河口町
    which(pref$code == 23111 & pref$sub_code == 1360),
    which(pref$code == 23111 & pref$sub_code == 380),

    #港区新船町-港区新川町
    which(pref$code == 23111 & pref$sub_code == 950),
    which(pref$code == 23111 & pref$sub_code == 1240),

    #中川区福船町-中川区福川町
    which(pref$code == 23110 & pref$sub_code == 1990),
    which(pref$code == 23110 & pref$sub_code == 1940),

    #中川区清船町-中川区清川町
    which(pref$code == 23110 & pref$sub_code == 520),
    which(pref$code == 23110 & pref$sub_code == 510),

    #中川区富船町-中川区富川町
    which(pref$code == 23110 & pref$sub_code == 1420),
    which(pref$code == 23110 & pref$sub_code == 1250),

    #中川区舟戸町-中川区広川町
    which(pref$code == 23110 & pref$sub_code == 2050),
    which(pref$code == 23110 & pref$sub_code == 1910),

    #中川区月島町-中川区広住町
    which(pref$code == 23110 & pref$sub_code == 1080),
    which(pref$code == 23110 & pref$sub_code == 1920),

    #中川区運河町-中川区運河町
    which(pref$code == 23110 & pref$sub_code == 250)[1],
    which(pref$code == 23110 & pref$sub_code == 250)[2],

    #港区千鳥-港区東築地町
    which(pref$code == 23111 & pref$sub_code == 1200),
    which(pref$code == 23111 & pref$sub_code == 1660),

    #港区作倉町-港区木場町
    which(pref$code == 23111 & pref$sub_code == 700),
    which(pref$code == 23111 & pref$sub_code == 450),

    #港区千年-南区三条
    which(pref$code == 23111 & pref$sub_code == 1180),
    which(pref$code == 23112 & pref$sub_code == 390),

    #熱田区千年-熱田区大瀬子町
    which(pref$code == 23109 & pref$sub_code == 520),
    which(pref$code == 23109 & pref$sub_code == 110),

    #熱田区千年-熱田区白鳥町
    which(pref$code == 23109 & pref$sub_code == 520),
    which(pref$code == 23109 & pref$sub_code == 310),

    #熱田区熱田西町-熱田区白鳥
    which(pref$code == 23109 & pref$sub_code == 40),
    which(pref$code == 23109 & pref$sub_code == 320),

    #熱田区熱田西町-熱田区旗屋
    which(pref$code == 23109 & pref$sub_code == 40),
    which(pref$code == 23109 & pref$sub_code == 670),

    #熱田区川並町-熱田区尾頭町
    which(pref$code == 23109 & pref$sub_code == 170),
    which(pref$code == 23109 & pref$sub_code == 120),

    #中川区柳川町-熱田区新尾頭
    which(pref$code == 23110 & pref$sub_code == 2420),
    which(pref$code == 23109 & pref$sub_code == 350),

    #中川区尾頭橋-中区正木
    which(pref$code == 23110 & pref$sub_code == 350),
    which(pref$code == 23106 & pref$sub_code == 490),

    #中川区山王-中区正木
    which(pref$code == 23110 & pref$sub_code == 680),
    which(pref$code == 23106 & pref$sub_code == 490),

    #中川区松重町-中区松原
    which(pref$code == 23110 & pref$sub_code == 2170),
    which(pref$code == 23106 & pref$sub_code == 530),

    #中村区名駅南-中区大須
    which(pref$code == 23105 & pref$sub_code == 1240),
    which(pref$code == 23106 & pref$sub_code == 70),

    #中村区名駅南-中区栄
    which(pref$code == 23105 & pref$sub_code == 1240),
    which(pref$code == 23106 & pref$sub_code == 190),

    #中村区名駅-中区錦
    which(pref$code == 23105 & pref$sub_code == 1190),
    which(pref$code == 23106 & pref$sub_code == 390),

    #中村区那古野-中区丸の内
    which(pref$code == 23105 & pref$sub_code == 820),
    which(pref$code == 23106 & pref$sub_code == 560),

    #西区那古野-中区丸の内
    which(pref$code == 23104 & pref$sub_code == 860),
    which(pref$code == 23106 & pref$sub_code == 560),

    #西区幅下-中区三の丸
    which(pref$code == 23104 & pref$sub_code == 990),
    which(pref$code == 23106 & pref$sub_code == 240),

    #南区内田橋-熱田区伝馬
    which(pref$code == 23112 & pref$sub_code == 60),
    which(pref$code == 23109 & pref$sub_code == 550),

    #熱田区神宮-瑞穂区桃園町
    which(pref$code == 23109 & pref$sub_code == 380),
    which(pref$code == 23108 & pref$sub_code == 1020),

    #熱田区花表町-瑞穂区新開町
    which(pref$code == 23109 & pref$sub_code == 730),
    which(pref$code == 23108 & pref$sub_code == 430),

    #熱田区三本松町-瑞穂区牛巻町
    which(pref$code == 23109 & pref$sub_code == 300),
    which(pref$code == 23108 & pref$sub_code == 100),

    #熱田区六野-瑞穂区二野町
    which(pref$code == 23109 & pref$sub_code == 770),
    which(pref$code == 23108 & pref$sub_code == 810),

    #熱田区六野-瑞穂区須田町
    which(pref$code == 23109 & pref$sub_code == 770),
    which(pref$code == 23108 & pref$sub_code == 450),

    #熱田区池内町-昭和区高辻町
    which(pref$code == 23109 & pref$sub_code == 60),
    which(pref$code == 23107 & pref$sub_code == 420),

    #熱田区桜田町-昭和区福江
    which(pref$code == 23109 & pref$sub_code == 250),
    which(pref$code == 23107 & pref$sub_code == 770),

    #中区大井町-中区千代田
    which(pref$code == 23106 & pref$sub_code == 60),
    which(pref$code == 23106 & pref$sub_code == 340),

    #中区富士見町-中区千代田
    which(pref$code == 23106 & pref$sub_code == 440),
    which(pref$code == 23106 & pref$sub_code == 340),

    #中区上前津-中区千代田
    which(pref$code == 23106 & pref$sub_code == 170),
    which(pref$code == 23106 & pref$sub_code == 340),

    #港区龍宮町-港区大江町
    which(pref$code == 23111 & pref$sub_code == 2020),
    which(pref$code == 23111 & pref$sub_code == 270),

    #南区豊田-南区加福本通
    which(pref$code == 23112 & pref$sub_code == 830),
    which(pref$code == 23112 & pref$sub_code == 210),

    #南区忠次-南区東又兵ヱ町
    which(pref$code == 23112 & pref$sub_code == 1480),
    which(pref$code == 23112 & pref$sub_code == 1040),

    #南区忠次-南区荒浜町
    which(pref$code == 23112 & pref$sub_code == 1480),
    which(pref$code == 23112 & pref$sub_code == 30),

    #港区昭和町-港区大江町
    which(pref$code == 23111 & pref$sub_code == 860),
    which(pref$code == 23111 & pref$sub_code == 270),

    #港区船見町-東海市新宝町
    which(pref$code == 23111 & pref$sub_code == 1820),
    which(pref$code == 23222 & pref$sub_code == 10),

    #南区柴田本通-東海市名和町
    which(pref$code == 23112 & pref$sub_code == 490),
    which(pref$code == 23222 & pref$sub_code == 30)[1],

    #南区天白町-緑区大高町
    which(pref$code == 23112 & pref$sub_code == 730),
    which(pref$code == 23114 & pref$sub_code == 4010)[1],

    #南区元鳴尾町-緑区鳴海町
    which(pref$code == 23112 & pref$sub_code == 1320),
    which(pref$code == 23114 & pref$sub_code == 2010)[1]

  ), ncol = 2, byrow = TRUE)

#Add edges 2
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge_2[,1],
                               pref_add_edge_2[,2])

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 4, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 4, admin = pref_map$code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  runs = 2L,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.06)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_pref))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(pref, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    ".Rds",
                    sep = ""))

saveRDS(prefadj, paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_adj.Rds",
                       sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(pref_map, paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map.rds",
                          sep = ""),
          compress = "xz")

saveRDS(sim_smc_pref, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims * 2),
                            ".Rds",
                            sep = ""))
