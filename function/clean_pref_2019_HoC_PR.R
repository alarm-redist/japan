#' Clean 2019 House of Councillors Elections Data
#'
#' @param pref_2019_HoC_PR raw 2019 House of Councillors Elections Data
#'
#' @return a data frame object of cleaned 2019 House of Councillors Elections Data
#'
#' @concept cleandata
#'
#' @export
#'

clean_pref_2019_HoC_PR <- function(pref_2019_HoC_PR){

  pref_2019_HoC_PR_cleaned <- pref_2019_HoC_PR %>%
    # Select relevant rows and columns
    slice(5:(n()-1)) %>%
    select(c(1, seq(from = 2, to = 40, by = 3))) %>%
    # Rename columns
    rename(mun_name = "...1",
           nv_jcp_2019 = "日本共産党",
           nv_ldp_2019 = "自由民主党",
           nv_olive_2019 = "オリーブの木",
           nv_sdp_2019 = "社会民主党",
           nv_komei_2019 = "公明党",
           nv_dpp_2019 = "国民民主党",
           nv_ishin_2019 = "日本維新の会",
           nv_hrp_2019 = "幸福実現党",
           nv_cdp_2019 = "立憲民主党",
           nv_labor_2019 = "労働の解放をめざす労働者党",
           nv_nhk_2019 = "ＮＨＫから国民を守る党",
           nv_ces_2019 = "安楽死制度を考える会",
           nv_reiwa_2019 = "れいわ新選組"
    ) %>%
    # Convert to numeric
    mutate_at(vars(matches("nv")), as.numeric) %>%
    # Calculate total number of votes
    rowwise() %>%
    mutate(nv_total_2019 = sum(c_across(-"mun_name"), na.rm = TRUE))

  # Return data
  return(pref_2019_HoC_PR_cleaned)
}
