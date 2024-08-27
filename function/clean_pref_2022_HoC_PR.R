#' Clean 2022 House of Councillors Elections Data
#'
#' @param pref_2022_HoC_PR raw 2022 House of Councillors Elections Data
#'
#' @return a data frame object of cleaned 2022 House of Councillors Elections Data
#'
#' @concept cleandata
#'
#' @export
#'

clean_pref_2022_HoC_PR <- function(pref_2022_HoC_PR){

  pref_2022_HoC_PR_cleaned <- pref_2022_HoC_PR %>%
    # Select relevant rows and columns
    slice(5:(n()-1)) %>%
    select(c(1, seq(from = 2, to = 46, by = 3))) %>%
    # Rename columns
    rename(mun_name = "...1",
           nv_hrp_2022 = "幸福実現党",
           nv_ishin_2022 = "日本維新の会",
           nv_reiwa_2022 = "れいわ新選組",
           nv_komei_2022 = "公明党",
           nv_gobo_2022 = "ごぼうの党",
           nv_cdp_2022 = "立憲民主党",
           nv_dpp_2022 = "国民民主党",
           nv_sansei_2022 = "参政党",
           nv_jfp_2022 = "日本第一党",
           nv_jcp_2022 = "日本共産党",
           nv_kunimori_2022 = "新党くにもり",
           nv_ldp_2022 = "自由民主党",
           nv_sdp_2022 = "社会民主党",
           nv_nhk_2022 = "ＮＨＫ党",
           nv_shimpu_2022 = "維新政党・新風"
    ) %>%
    # Convert to numeric
    mutate_at(vars(matches("nv")), as.numeric) %>%
    # Calculate total number of votes
    rowwise() %>%
    mutate(nv_total_2022 = sum(c_across(-"mun_name"), na.rm = TRUE))

  # Return data
  return(pref_2022_HoC_PR_cleaned)
}
