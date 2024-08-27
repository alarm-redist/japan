#' Mutate partisan metrics columns for Japan
#'
#' @param plans a `redist_plans` object with partisan data
#'                  nv_ldp, nv_cdp, nv_ishin, nv_komei, nv_jcp,
#'                  nv_dpp, nv_reiwa, nv_sdp, nv_nhk, nv_other
#' @param  pref_map a `redist_map` object
#'
#' @returns `redist_plans` object with partisan statistics
#' @export
#'
#'
partisan_metrics_japan <- function(plans, pref_map) {

  perim_path <- here::here("temp/perim.rds")

  if (requireNamespace('redistmetrics', quietly = TRUE)) {
      perim_df <- redistmetrics::prep_perims(pref_map, perim_path = perim_path)
    } else {
      perim_df <- redist.prep.polsbypopper(pref_map, perim_path = perim_path)
    }

  plans_partisan <- plans %>%
    dplyr::mutate(

      # Maximum Deviation from Population Parity
      plan_dev = redist::plan_parity(pref_map),

      # Compactness: FracKept
      comp_edge = redist::distr_compactness(pref_map),

      # Compactness: Polsby-Popper Compactness
      comp_polsby = redist::distr_compactness(pref_map,
                                              measure = "PolsbyPopper",
                                              perim_df = perim_df),

      # Number of LDP voters
      nv_ldp = tally_var(pref_map, nv_ldp),

      # Number of Komei voters
      nv_komei = tally_var(pref_map, nv_komei),

      # Number of LDP + Komei voters
      nv_ruling = tally_var(pref_map, nv_ldp + nv_komei),

      # Number of CDP voters
      nv_cdp = tally_var(pref_map, nv_cdp),

      # Number of voters: opposition coalition (excludes DPP)
      nv_opp_coalition_4 = tally_var(pref_map,
                                     nv_cdp + nv_jcp + nv_reiwa + nv_sdp),

      # Number of voters: opposition coalition (includes DPP)
      nv_opp_coalition_5 = tally_var(pref_map,
                                     nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                       nv_dpp),

      # Number of voters: all major opposition parties
      nv_opp_all = tally_var(pref_map,
                             nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                               nv_dpp + nv_ishin),

      # LDP vote share
      ldp_share = redist::group_frac(pref_map,
                                     nv_ldp,
                                     nv_total),

      # Komei vote share
      komei_share = redist::group_frac(pref_map,
                                       nv_komei,
                                       nv_total),

      # Ruling party vote share
      ruling_share = redist::group_frac(pref_map,
                                        nv_ldp + nv_komei,
                                        nv_total),

      # LDP vote share vs Komei
      ldp_v_komei =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp,
                                 rvote = nv_komei),

      # LDP vote share vs CDP
      ldp_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp,
                                 rvote = nv_cdp),

      # Ruling vote share vs CDP
      ruling_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp),

      # Ruling coalition vote share (vs Opposition coalition that excludes the DPP)
      ruling_v_opp_coalition_4 =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp),

      # Ruling coalition vote share (vs Opposition coalition that includes the DPP)
      ruling_v_opp_coalition_5 =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp),

      # Ruling coalition vote share (vs All major opposition parties)
      ruling_v_opp_all =
        redist::partisan_metrics(pref_map,
                                 "DVS",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp + nv_ishin),

      # Expected number of LDP seats (LDP v CDP)
      seats_ldp_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "DSeats",
                                 dvote = nv_ldp,
                                 rvote = nv_cdp),

      # Expected number of ruling coalition seats (Ruling coalition v CDP)
      seats_ruling_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "DSeats",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp),

      # Expected number of ruling coalition seats
      # (Ruling coalition v Opposition coalition that excludes the DPP)
      seats_ruling_v_opp_coalition_4 =
        redist::partisan_metrics(pref_map,
                                 "DSeats",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp),

      # Expected number of ruling coalition seats
      # (Ruling coalition v Opposition coalition that includes the DPP)
      seats_ruling_v_opp_coalition_5 =
        redist::partisan_metrics(pref_map,
                                 "DSeats",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp),

      # Expected number of ruling coalition seats
      # (Ruling coalition v Opposition coalition that includes the DPP)
      seats_ruling_v_opp_all =
        redist::partisan_metrics(pref_map,
                                 "DSeats",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp + nv_ishin),

      # Efficiency gap: LDP v CDP
      egap_ldp_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "EffGap",
                                 dvote = nv_ldp,
                                 rvote = nv_cdp),

      # Efficiency gap: Ruling coalition v CDP
      egap_ruling_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "EffGap",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp),

      # Efficiency gap
      # (Ruling coalition v Opposition coalition that excludes the DPP)
      egap_ruling_v_opp_coalition_4 =
        redist::partisan_metrics(pref_map,
                                 "EffGap",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp),

      # Efficiency Gap
      # (Ruling coalition v Opposition coalition that includes the DPP)
      egap_ruling_v_opp_coalition_5 =
        redist::partisan_metrics(pref_map,
                                 "EffGap",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp),

      # Expected number of ruling coalition seats
      # (Ruling coalition v Opposition coalition that includes the DPP)
      egap_ruling_v_opp_all =
        redist::partisan_metrics(pref_map,
                                 "EffGap",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp + nv_ishin),

      # Partisan bias: LDP v CDP
      pbias_ldp_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "Bias",
                                 dvote = nv_ldp,
                                 rvote = nv_cdp),

      # Partisan Bias: Ruling coalition v CDP
      pbias_ruling_v_cdp =
        redist::partisan_metrics(pref_map,
                                 "Bias",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp),

      # Partisan Bias
      # (Ruling coalition v Opposition coalition that excludes the DPP)
      pbias_ruling_v_opp_coalition_4 =
        redist::partisan_metrics(pref_map,
                                 "Bias",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp),

      # Partisan Bias
      # (Ruling coalition v Opposition coalition that includes the DPP)
      pbias_ruling_v_opp_coalition_5 =
        redist::partisan_metrics(pref_map,
                                 "Bias",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp),

      # Expected number of ruling coalition seats
      # (Ruling coalition v Opposition coalition that includes the DPP)
      pbias_ruling_v_opp_all =
        redist::partisan_metrics(pref_map,
                                 "Bias",
                                 dvote = nv_ldp + nv_komei,
                                 rvote = nv_cdp + nv_jcp + nv_reiwa + nv_sdp +
                                   nv_dpp + nv_ishin),
      )

  return(plans_partisan)
}
