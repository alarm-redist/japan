#' Estimate Baseline Votes Per Party
#'
#' @param pref_2019_HoC_PR_cleaned cleaned 2019 House of Councillors Elections Data
#' @param pref_2022_HoC_PR_cleaned cleaned 2022 House of Councillors Elections Data
#'
#' @return a data frame object with an estimate of the basline votes per party
#'
#' @concept cleandata
#'
#' @export
#'
#'

clean_pref_HoC_PR <- function(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned){

  # Remove space from `mun_name`
  pref_2019_HoC_PR_cleaned$mun_name <- gsub(' ','', pref_2019_HoC_PR_cleaned$mun_name) # 半角 (normal) space
  pref_2019_HoC_PR_cleaned$mun_name <- gsub('　','', pref_2019_HoC_PR_cleaned$mun_name) # 全角 (Japanese) space
  pref_2022_HoC_PR_cleaned$mun_name <- gsub(' ','', pref_2022_HoC_PR_cleaned$mun_name) # 半角 (normal) space
  pref_2022_HoC_PR_cleaned$mun_name <- gsub('　','', pref_2022_HoC_PR_cleaned$mun_name) # 全角 (Japanese) space

  # Merge data
  pref_join_HoC_PR_cleaned <-
    full_join(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned, by = "mun_name")

  # Estimate baseline vote
  pref_HoC_PR <- pref_join_HoC_PR_cleaned %>%
    # Add up votes for non-major parties
    mutate(nv_other_2019 = sum(nv_olive_2019, nv_hrp_2019, nv_labor_2019,
                               nv_ces_2019,
                               na.rm = TRUE),
           nv_other_2022 = sum(nv_hrp_2022, nv_gobo_2022, nv_sansei_2022,
                               nv_jfp_2022, nv_kunimori_2022, nv_shimpu_2022,
                               na.rm = TRUE)) %>%
    # Mean vote share * Geometric mean of turnout
    mutate(nv_ldp = 1/2 * (nv_ldp_2019/nv_total_2019 + nv_ldp_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_cdp = 1/2 * (nv_cdp_2019/nv_total_2019 + nv_cdp_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_ishin = 1/2 * (nv_ishin_2019/nv_total_2019 + nv_ishin_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_komei = 1/2 * (nv_komei_2019/nv_total_2019 + nv_komei_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_jcp = 1/2 * (nv_jcp_2019/nv_total_2019 + nv_jcp_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_dpp = 1/2 * (nv_dpp_2019/nv_total_2019 + nv_dpp_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_reiwa = 1/2 * (nv_reiwa_2019/nv_total_2019 + nv_reiwa_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_sdp = 1/2 * (nv_sdp_2019/nv_total_2019 + nv_sdp_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_nhk = 1/2 * (nv_nhk_2019/nv_total_2019 + nv_nhk_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_other = 1/2 * (nv_other_2019/nv_total_2019 + nv_other_2022/nv_total_2022) *
             sqrt(nv_total_2019*nv_total_2022),
           nv_total = sum(nv_ldp, nv_cdp, nv_ishin, nv_komei, nv_jcp,
                          nv_dpp, nv_reiwa, nv_sdp, nv_nhk, nv_other)) %>%

    # Select columns
    select(mun_name, nv_ldp, nv_cdp, nv_ishin, nv_komei, nv_jcp,
           nv_dpp, nv_reiwa, nv_sdp, nv_nhk, nv_other, nv_total)

  # Return
  return(pref_HoC_PR)
}
