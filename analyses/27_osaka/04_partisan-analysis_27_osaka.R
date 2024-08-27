###############################################################################
# Partisan Analysis for `27_osaka`
# © ALARM Project, March 2023
###############################################################################

# Load data
pref_map <- readRDS(here(paste("data-out/map/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_lh_2022_map.rds",
                               sep = "")))

sim_smc_pref_sample <- readRDS(here(paste("data-out/plans/",
                                          as.character(pref_code),
                                          "_",
                                          as.character(pref_name),
                                          "_lh_2022_plans.rds",
                                          sep = "")))

# Population Deviation
redist.plot.hist(sim_smc_pref_sample, qty = plan_dev, bins = 10) +
  labs(x = "Population Deviation", y = "Percentage of Plans") +
  theme_bw()

# Compactness
redist.plot.hist(sim_smc_pref_sample, qty = comp_edge, bins = 10) +
  labs(x = "Fraction of Edges Kept", y = "Percentage of Plans") +
  theme_bw()


###############################################
# Analysis of Ishin for Osaka
# In Osaka, Ishin is the leading party.
# Also, Ishin avoided to send the candidate in the district where Komei has candidate.
# Therefore, the `ruling_coalition` and other default grouping are not accurate.

enac_sum <- sim_smc_pref_sample %>%
  filter(draw == "lh_2022") %>%
  # TODO: match with what gets plotted
  mutate(
    dist_lab = paste0(str_pad(.$district, width = 2, pad = '0')),
    ishin_share_rank = rank(ishin_share), # ascending order
    ldp_share_rank = rank(ldp_share),
    ldp_v_ishin_rank = rank(ldp_v_ishin),
    komei_share_rank = rank(komei_share),
    compact_rank = rank(comp_polsby),
  )

redist.plot.distr_qtys(sim_smc_pref_sample, comp_polsby,
                       geom = "boxplot") +
  geom_text(data = enac_sum,
            aes(x = compact_rank,
                label = dist_lab),
            vjust = 3,
            y = Inf,
            size = 2.5,
            fontface = "bold",
            lineheight = 0.8,
            alpha = 0.8,
            color = "red")

redist.plot.distr_qtys(sim_smc_pref_sample, ldp_share,
                       geom = "boxplot")+
  geom_text(data = enac_sum,
            aes(x = ldp_share_rank,
                label = dist_lab),
            vjust = 3,
            y = Inf,
            size = 2.5,
            fontface = "bold",
            lineheight = 0.8,
            alpha = 0.8,
            color = "red")


redist.plot.distr_qtys(sim_smc_pref_sample, ldp_v_ishin,
                       geom = "boxplot")+
  geom_text(data = enac_sum,
            aes(x = ldp_v_ishin_rank,
                label = dist_lab),
            vjust = 3,
            y = Inf,
            size = 2.5,
            fontface = "bold",
            lineheight = 0.8,
            alpha = 0.8,
            color = "red")

redist.plot.distr_qtys(sim_smc_pref_sample, ishin_share,
                       geom = "boxplot")+
  geom_text(data = enac_sum,
            aes(x = ishin_share_rank,
                label = dist_lab),
            vjust = 3,
            y = Inf,
            size = 2.5,
            fontface = "bold",
            lineheight = 0.8,
            alpha = 0.8,
            color = "red")

redist.plot.distr_qtys(sim_smc_pref_sample, komei_share,
                       geom = "boxplot")+
  geom_text(data = enac_sum,
            aes(x = komei_share_rank,
                label = dist_lab),
            vjust = 3,
            y = Inf,
            size = 2.5,
            fontface = "bold",
            lineheight = 0.8,
            alpha = 0.8,
            color = "red")
