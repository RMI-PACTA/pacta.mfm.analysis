#' @export
prep_exposures_scorecard <- function(results_portfolio,
                                     overview_portfolio,
                                     scenario_selected = "1.5C-Unif") {
  # input data
  data <- results_portfolio

  # infer start year
  start_year <- min(data$year, na.rm = TRUE)

  # filter data
  data <- data %>%
    dplyr::filter(
      year == .env$start_year,
      .data$scenario == .env$scenario_selected
    )

  # calculate current exposures and wrangle for score card
  data_out <- data %>%
    wrangle_data_exposures_scorecard(overview_portfolio)

  data_out
}

wrangle_data_exposures_scorecard <- function(data,
                                             overview_portfolio) {
  overview_portfolio <- overview_portfolio %>%
    dplyr::mutate(total_portfolio = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    dplyr::group_by(.data$asset_type) %>%
    dplyr::mutate(total_asset_value = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    dplyr::select("asset_type", "total_portfolio", "total_asset_value") %>%
    dplyr::mutate(asset_type = tolower(.data$asset_type)) %>%
    dplyr::distinct()

  data <- data %>%
    dplyr::rename(asset_type = "asset_class") %>%
    dplyr::inner_join(
      base::get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::mutate(
      sector_or_tech = dplyr::case_when(
        .data$ald_sector == "coal" ~ "coal",
        .data$technology == "coalcap" ~ "coal",
        .data$technology %in% c("oil", "oilcap") ~ "oil",
        .data$technology %in% c("gas", "gascap") ~ "gas",
        .data$technology == "renewablescap" ~ "renewables_power",
        .data$technology == "nuclearcap" ~ "nuclear_power",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::left_join(overview_portfolio, by = "asset_type") %>%
    dplyr::filter(!is.na(.data$sector_or_tech)) %>%
    dplyr::select(c("asset_type", "sector_or_tech", "plan_carsten", "total_portfolio", "total_asset_value")) %>%
    dplyr::mutate(total_amount_by_asset_type = .data$plan_carsten * .data$total_asset_value) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$sector_or_tech) %>%
    dplyr::mutate(total_amount = sum(.data$total_amount_by_asset_type, na.rm = TRUE)) %>%
    dplyr::select(c("sector_or_tech", "total_amount", "total_portfolio")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(exposure_perc_aum = .data$total_amount / .data$total_portfolio) %>%
    dplyr::select(c("sector_or_tech", "exposure_perc_aum")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      sector_or_tech = factor(
        .data$sector_or_tech,
        levels = rev(c("coal", "oil", "gas", "renewables_power", "nuclear_power"))
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = "sector_or_tech",
      names_prefix = "exposure_swiss_score_card_",
      values_from = "exposure_perc_aum"
    )


  data <- data %>%
    mutate(exposure_swiss_score_card_coal = ifelse("exposure_swiss_score_card_coal" %in% colnames(data), exposure_swiss_score_card_coal, 0),
           exposure_swiss_score_card_oil = ifelse("exposure_swiss_score_card_oil" %in% colnames(data), exposure_swiss_score_card_oil, 0),
           exposure_swiss_score_card_gas = ifelse("exposure_swiss_score_card_gas" %in% colnames(data), exposure_swiss_score_card_gas, 0),
           exposure_swiss_score_card_renewables_power = ifelse("exposure_swiss_score_card_renewables_power" %in% colnames(data), exposure_swiss_score_card_renewables_power, 0),
           exposure_swiss_score_card_nuclear_power = ifelse("exposure_swiss_score_card_nuclear_power" %in% colnames(data), exposure_swiss_score_card_nuclear_power, 0))
}
