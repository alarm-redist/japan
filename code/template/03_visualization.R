###############################################################################
# Data visualization for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
a_koiki <- c()
b_koiki <- c()
c_koiki <- c()
koiki <- c(a_koiki, b_koiki, c_koiki)

for (i in 0:1)
{
  sim_smc_pref_n <- readRDS(paste("data-out/plans/",
                                  as.character(pref_code),
                                  "_",
                                  as.character(pref_name),
                                  "_",
                                  as.character(sim_type),
                                  "_",
                                  as.character(nsims),
                                  "_",
                                  as.character(i),
                                  ".Rds",
                                  sep = ""), refhook = NULL)
  pref_smc_plans_n <- redist::get_plans_matrix(sim_smc_pref_n)

  assign(paste("pref_smc_plans_", i, sep = ""), pref_smc_plans_n)

}


