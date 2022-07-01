###############################################################################
# Simulations for `28_hyogo`
# © ALARM Project, July 2022
###############################################################################

# Assign 郡 codes
pref <- merge_gun(pref)

# Choose 郡 to merge
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
gun_codes <- setdiff(gun_codes, gun_exception) # Filter out exceptions

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
  gun$sub_code <- NA
  gun$gun_code <- gun_codes[i]
  pref_gun <- dplyr::bind_rows(pref_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
pref <- dplyr::bind_rows(pref_non_gun, pref_gun)

# Convert multi-polygons into polygons
sf_use_s2(FALSE)
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)

new_rows <- new_rows %>%
  dplyr::mutate(area = sf::st_area(geometry)) %>%
  dplyr::arrange(desc(area)) %>%
  dplyr::select(-area)

new_rows[1, ]$pop <- pref[1, ]$pop

# Keep 東灘区 (23101) as a seperate dataframe since there are 2 rows
first_rows <- new_rows[2,]
pref_sep <- new_rows[1,]

# to calculate area size, switch off `geometry (s2)`
for (i in 2:nrow(pref))
{
  new_rows <- data.frame(code = pref[i, ]$code,
                         sub_code = pref[i, ]$sub_code,
                         geometry = sf::st_cast(pref[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = pref[i, ]$gun_code
  )

  # order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    dplyr::select(-area)

  # assign population to the largest area
  new_rows[1, ]$pop <- pref[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

# Merge 東灘区 back into pref_sep
pref_sep <- dplyr::bind_rows(pref_sep, first_rows) %>%
  dplyr::arrange(code, sub_code)

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

# Optional: Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)

# remove incorrect suggestion
incorrect <- c(which(pref$code == 28202)[2],
               which(pref$code == 28206)[2],
               which(pref$code == 28380)[3])
suggest <- suggest %>%
  filter(x %in% incorrect == FALSE & y %in% incorrect == FALSE)

prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
# prefadj <- geomander::add_edge(prefadj,
# which(pref$code == xxxxx & pref$sub_code == "xxxx"),
# which(pref$code == xxxxx & pref$sub_code == "xxxx"))

# 姫路市飾磨区須加-姫路市旧家島町
pref_add_edge_1 <-
  matrix(c(
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[1],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[3],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[4],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[5],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[6],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[7],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[9],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[10],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[11],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[12],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[13],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[14],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[15],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[16],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[17],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[18],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[19],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[20],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[21],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[22],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[23],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[24],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[25],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[26],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[27],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[28],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[29],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[30],
    which(pref$code == 28201 & pref$sub_code == 4620)[2],
    which(pref$code == 28201 & pref$sub_code == 6940)[31]
  ), ncol = 2, byrow = TRUE)

# Add edges 1
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge_1[,1],
                               pref_add_edge_1[,2])

pref_add_edge_2 <-
  matrix(c(
    #尼崎市末広町-尼崎市
    which(pref$code == 28202)[1],
    which(pref$code == 28202)[2],

    #芦屋市陽光町-芦屋市
    which(pref$code == 28206)[2],
    which(pref$code == 28206)[1],

    #淡路市-神戸市垂水区
    which(pref$code == 28226),
    which(pref$code == 28108),

    #淡路市-明石市
    which(pref$code == 28226),
    which(pref$code == 28203),

    #播磨町新島-播磨町
    which(pref$code == 28380)[2],
    which(pref$code == 28380)[3],

    #たつの市島嶼部-たつの市
    which(pref$code == 28229)[1],
    which(pref$code == 28229)[2],
    which(pref$code == 28229)[1],
    which(pref$code == 28229)[4],

    #相生市島嶼部-相生市
    which(pref$code == 28208)[1],
    which(pref$code == 28208)[2],
    which(pref$code == 28208)[1],
    which(pref$code == 28208)[3],

    #赤穂市島嶼部-赤穂市
    which(pref$code == 28212)[1],
    which(pref$code == 28212)[2],
    which(pref$code == 28212)[1],
    which(pref$code == 28212)[3]

  ), ncol = 2, byrow = TRUE)

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
constr = redist::add_constr_splits(constr, strength = 3, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 2, admin = pref_map$code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  runs = 4L,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.05)

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
                            as.character(nsims * 4),
                            ".Rds",
                            sep = ""))
