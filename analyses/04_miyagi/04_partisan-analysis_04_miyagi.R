###############################################################################
# Partisan Analysis for `04_miyagi`
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

# Election results by district:
# Ruling coalition vote share
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                       color_thresh = 0.5)
# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_share,
                       geom = "boxplot")

# LDP vote share vs Komei vote share
# Scatter Plot
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_share)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share)
# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_share,
                               color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324")

# Election results by district:
# Ruling coalition vs opposition coalition that excludes the DPP
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                       color_thresh = 0.5)
# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                       geom = "boxplot")

# LDP vote share vs Komei vote share
# Scatter Plot
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_coalition_4)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share)
# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_coalition_4,
                               color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324")

# Election results by district:
# Ruling coalition vs all major opposition parties
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                       color_thresh = 0.5)
# Boxplot
redist.plot.distr_qtys(sim_smc_pref_sample, ruling_v_opp_all,
                       geom = "boxplot")

# LDP vote share vs Komei vote share
# Scatter Plot
sim_smc_pref_sample %>%
  group_by(draw) %>%
  mutate(dist_by_ruling_share = row_number(ruling_v_opp_all)) %>%
  redist.plot.scatter(x = ldp_share,
                      y = komei_share) +
  facet_wrap(~dist_by_ruling_share)
# Dot-plots by Ordered Districts
redist.plot.distr.custom.color(sim_smc_pref_sample, ruling_v_opp_all,
                               color_var = ldp_v_komei) +
  scale_colour_gradient(low = "#f55881", high = "#3CA324")
