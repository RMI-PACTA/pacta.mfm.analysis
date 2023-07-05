#' Prepare data input for plotting aggregate climate scores
#'
#' Prepare data input for plotting aggregate climate scores based
#' on PACTA for investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param overview Data frame that contains overview
#' @param scenario_source Character. Must be a
#'   `scenario_source` featured in the `scenario_thresholds` data set.
#' @param remaining_carbon_budgets Data-frame that contains the remaining carbon
#' budget per sector and scenario source.
#'
#' @return data.frame
#' @export
prep_scores <- function(results_portfolio,
                        overview = overview,
                        scenario_source = "GECO2021",
                        remaining_carbon_budgets,
                        scenario_thresholds) {

  # infer start_year
  start_year <- min(results_portfolio$year, na.rm = TRUE)

  scenarios <- scenario_thresholds %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source) %>%
    dplyr::pull(.data$scenario)

  # prepare data for sector aggregation
  data_aggregate_scores <- results_portfolio %>%
    wrangle_input_data_aggregate_scores(scenarios = scenarios)

  # roadmap sectors
  data_aggregate_scores_tech <- data_aggregate_scores %>%
    dplyr::filter(.data$ald_sector %in% c("automotive", "coal", "oil", "gas", "power"))

  # calculate technology level alignment
  data_aggregate_scores_tech <- data_aggregate_scores_tech %>%
    calculate_technology_alignment(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # calculate roadmap sectors aggregate score
  sector_aggregate_scores_tech <- data_aggregate_scores_tech %>%
    calculate_sector_aggregate_scores()

  # emission intensity sectors
  data_aggregate_scores_emissions <- data_aggregate_scores %>%
    dplyr::filter(.data$ald_sector %in% c("aviation", "cement", "steel", "shipping"))

  # calculate emission intensity sectors aggregate score
  sector_aggregate_scores_emissions <- data_aggregate_scores_emissions %>%
    calculate_emissions_sectors_aggregate_scores(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # combine scores of roadmap and emission intensity sectors
  sector_aggregate_scores <- sector_aggregate_scores_tech %>%
    dplyr::bind_rows(sector_aggregate_scores_emissions)

  portfolio_aggregate_scores <- sector_aggregate_scores %>%
    calculate_portfolio_aggregate_scores(
      remaining_carbon_budgets = remaining_carbon_budgets
    )

  aggregate_portfolio_score <- portfolio_aggregate_scores %>%
    calculate_aggregate_portfolio_score(overview = overview)

  # combine scores in single data frame
  output_scores <- sector_aggregate_scores %>%
    dplyr::bind_rows(portfolio_aggregate_scores) %>%
    dplyr::select(-"sector_exposure") %>%
    dplyr::bind_rows(aggregate_portfolio_score)

  # apply scenario based grades
  data_out <- output_scores %>%
    calculate_aggregate_scores_with_scenarios(
      scenario_thresholds = scenario_thresholds
    ) %>%
    dplyr::select(
      c("asset_class", "scope", "entity", "sector", "score")
    )

  data_out
}

wrangle_input_data_aggregate_scores <- function(data,
                                                scenarios) {
  # filter data
  data <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios)

  # map sectors to p4b style
  data <- data %>%
    dplyr::inner_join(
      base::get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c("sector_p4b", "technology_p4b"))

  # Oil & Gas are treated as separate sectors in the aggregate score.
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$technology == "oil" ~ "oil",
        .data$technology == "gas" ~ "gas",
        TRUE ~ .data$ald_sector
      )
    )

  return(data)
}

calculate_technology_alignment <- function(data,
                                           start_year,
                                           time_horizon) {
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      scen_alloc_wt_tech_prod_t0 = dplyr::first(.data$scen_alloc_wt_tech_prod),
      scen_alloc_wt_tech_prod_t5 = dplyr::last(.data$scen_alloc_wt_tech_prod),
      plan_alloc_wt_tech_prod_t5 = dplyr::last(.data$plan_alloc_wt_tech_prod),
      exposure_t0 = dplyr::first(.data$plan_carsten)
    ) %>%
    dplyr::select(-c(scen_alloc_wt_tech_prod)) %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::mutate(
      tech_trajectory_alignment = dplyr::if_else(
        .data$green_or_brown == "green",
        (.data$plan_alloc_wt_tech_prod - .data$scen_alloc_wt_tech_prod_t5) /
          .data$scen_alloc_wt_tech_prod_t5,
        (.data$scen_alloc_wt_tech_prod_t5 - .data$plan_alloc_wt_tech_prod) /
          .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}

calculate_sector_aggregate_scores <- function(data) {
  # create helper variables for the weighting factors
  # account for special handling of CoalCap, RenewablesCap, ICE and Electric
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      tech_allocation_weight = dplyr::if_else(
        # TODO: add an input that defines these technologies
        .data$technology %in% techs_edge_case_aggregate_score_lookup &
          .data$plan_alloc_wt_tech_prod_t5 > .data$scen_alloc_wt_tech_prod_t5,
        .data$plan_alloc_wt_tech_prod_t5,
        .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::mutate(
      scenario_change = abs(.data$scen_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0),
      production_change = abs(.data$plan_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0)
    ) %>%
    dplyr::mutate(
      scenario_change = dplyr::if_else(
        .data$technology %in% techs_edge_case_aggregate_score_lookup &
          .data$production_change > .data$scenario_change,
        .data$production_change,
        .data$scenario_change
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("production_change"))

  # calculate sector score & sector exposure
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$tech_trajectory_alignment,
        w = .data$tech_allocation_weight * .data$scenario_change
      ),
      sector_exposure = sum(.data$exposure_t0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(score)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scope = "sector")

  return(data)
}

calculate_emissions_sectors_aggregate_scores <- function(data,
                                                         start_year,
                                                         time_horizon) {
  data_out <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(sector_exposure = dplyr::first(.data$plan_sec_carsten)) %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::mutate(
      score = (.data$scen_sec_emissions_factor - .data$plan_sec_emissions_factor) /
        .data$scen_sec_emissions_factor
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$score, .data$sector_exposure
    ) %>%
    dplyr::mutate(scope = "sector")

  return(data_out)
}

calculate_portfolio_aggregate_scores <- function(data,
                                                 remaining_carbon_budgets) {
  data <- data %>%
    dplyr::inner_join(
      remaining_carbon_budgets,
      by = c("scenario_source", "ald_sector")
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$score,
#        w = .data$remaining_carbon_budget * .data$sector_exposure
        w = .data$sector_exposure
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scope = "portfolio",
      ald_sector = NA_character_
    )

  return(data)
}

calculate_aggregate_scores_with_scenarios <- function(data,
                                                      scenario_thresholds) {
  # add threshold scenarios
  data <- data %>%
    dplyr::select(
      c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "ald_sector", "scenario_source", "scenario", "score"
      )
    ) %>%
    dplyr::inner_join(
      scenario_thresholds,
      by = c("scenario_source", "scenario")
    ) %>%
    dplyr::select(-c("scenario", "scenario_source"))

  data <- data %>%
    tidyr::pivot_wider(
      id_cols = c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "ald_sector"
      ),
      names_from = "threshold",
      values_from = "score"
    ) %>%
    dplyr::rename(sector = "ald_sector")

  # calculate grades based on scores and scenario thresholds
  data <- data %>%
    dplyr::mutate(
      score = dplyr::case_when(
        .data$high >= 0.15 ~ "A+",
        .data$high >= 0 ~ "A",
        .data$high < 0 & .data$mid >= 0.15 ~ "B",
        .data$mid >= 0 ~ "C",
        .data$mid < 0 & .data$low >= 0 ~ "D",
        .data$low < 0 ~ "E",
        TRUE ~ NA_character_,
      )
    )

  data <- data %>%
    dplyr::select(
      c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "sector", "score", "high"
      )
    ) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$scope,
      .data$sector, .data$entity_name, .data$entity_type, .data$entity, .data$high
    )

  return(data)
}


#' @export
prep_data_executive_summary_mfm <- function(start_year,
                                            scenario_source,
                                            scenario_geography,
                                            equity_market,
                                            portfolio_allocation_method_equity,
                                            portfolio_allocation_method_bonds,
                                            green_techs,
                                            equity_results_portfolio,
                                            bonds_results_portfolio) {
  equity_results_portfolio <- equity_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_equity
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    )

  # add asset class, entity type and grenn/brown, combine data sets
  # ... portfolios
  equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      asset_class = "equity"
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      asset_class = "bonds"
    )

  results_portfolio <- equity_results_portfolio %>%
    dplyr::bind_rows(bonds_results_portfolio) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      ),
      entity_name = "portfolio",
      entity_type = "this_portfolio",
      entity = "this_portfolio"
    )

  return(results_portfolio)
}

apply_general_filters <- function(data,
                                  scenario_source,
                                  scenario_geography,
                                  equity_market,
                                  start_year,
                                  allocation_type) {
  data <- data %>%
    dplyr::filter(
      .data$scenario_source %in% .env$scenario_source,
      .data$scenario_geography %in% .env$scenario_geography,
      .data$equity_market == .env$equity_market,
      dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon_lookup),
      .data$allocation == .env$allocation_type
    )

  return(data)
}


calculate_aggregate_portfolio_score <- function(data,
                                                overview) {
  overview <- overview %>%
    dplyr::filter(.data$financial_sector != "Other") %>%
    dplyr::group_by(.data$asset_type) %>%
    dplyr::mutate(asset_type_sector_exposure = sum(.data$valid_value_usd, na.rm = TRUE)) %>%
    dplyr::mutate(
      asset_class = dplyr::case_when(
        .data$asset_type == "Equity" ~ "equity",
        .data$asset_type == "Bonds" ~ "bonds",
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("asset_class", "asset_type_sector_exposure") %>%
    dplyr::distinct()

  data <- data %>%
    dplyr::left_join(overview, by = "asset_class") %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$score,
        w = .data$asset_type_sector_exposure
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(asset_class = "portfolio")

  return(data)
}



#' @export
prep_scores_NZE <- function(results_portfolio,
                        overview = overview,
                        scenario = "NZE_2050",
                        remaining_carbon_budgets_nze) {

  # infer start_year
  start_year <- min(results_portfolio$year, na.rm = TRUE)

  # prepare data for sector aggregation
  data_aggregate_scores <- results_portfolio %>%
    wrangle_input_data_aggregate_scores(scenarios = scenario)

  # roadmap sectors
  data_aggregate_scores_tech <- data_aggregate_scores %>%
    dplyr::filter(.data$ald_sector %in% c("automotive", "coal", "oil", "gas", "power"))

  # calculate technology level alignment
  data_aggregate_scores_tech <- data_aggregate_scores_tech %>%
    calculate_technology_alignment(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # calculate roadmap sectors aggregate score
  sector_aggregate_scores_tech <- data_aggregate_scores_tech %>%
    calculate_sector_aggregate_scores()

  # emission intensity sectors
  data_aggregate_scores_emissions <- data_aggregate_scores %>%
    dplyr::filter(.data$ald_sector %in% c("aviation", "cement", "steel", "shipping"))

  # calculate emission intensity sectors aggregate score
  sector_aggregate_scores_emissions <- data_aggregate_scores_emissions %>%
    calculate_emissions_sectors_aggregate_scores(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # combine scores of roadmap and emission intensity sectors
  sector_aggregate_scores <- sector_aggregate_scores_tech %>%
    dplyr::bind_rows(sector_aggregate_scores_emissions)

  portfolio_aggregate_scores <- sector_aggregate_scores %>%
    calculate_portfolio_aggregate_scores(
      remaining_carbon_budgets = remaining_carbon_budgets_nze
    )

  aggregate_portfolio_score <- portfolio_aggregate_scores %>%
    calculate_aggregate_portfolio_score(overview = overview)

  # combine scores in single data frame
  output_scores <- sector_aggregate_scores %>%
    dplyr::bind_rows(portfolio_aggregate_scores) %>%
    dplyr::select(-"sector_exposure") %>%
    dplyr::bind_rows(aggregate_portfolio_score)

  # apply scenario based grades
  data_out <- output_scores %>%
   calculate_aggregate_scores_with_scenarios_nze(
     scenario_thresholds = scenario_thresholds
   ) %>%
   dplyr::select(
    c("asset_class", "scope", "entity", "sector", "score", "high")
  )

  data_out
}



calculate_aggregate_scores_with_scenarios_nze <- function(data,
                                                      scenario_thresholds) {
  # add threshold scenarios
  data <- data %>%
    dplyr::select(
      c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "ald_sector", "scenario_source", "scenario", "score"
      )
    ) %>%
    dplyr::inner_join(
      scenario_thresholds,
      by = c("scenario_source", "scenario")
    ) %>%
    dplyr::select(-c("scenario", "scenario_source"))

  data <- data %>%
    tidyr::pivot_wider(
      id_cols = c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "ald_sector"
      ),
      names_from = "threshold",
      values_from = "score"
    ) %>%
    dplyr::rename(sector = "ald_sector")

  # calculate grades based on scores and scenario thresholds
  data <- data %>%
    dplyr::mutate(
      score = dplyr::case_when(
        .data$high >= 0.15 ~ "A+",
        .data$high >= 0 ~ "A",
        .data$high >-0.1   ~ "B",
        .data$high > -0.2 ~ "C",
        .data$high > -0.4  ~ "D",
        .data$high > -0.6 ~ "E",
        !is.na(.data$high) ~ "F",
        TRUE ~ NA_character_,
      )
    )

  data <- data %>%
    dplyr::select(
      c(
        "investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "sector", "score", "high"
      )
    ) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$scope,
      .data$sector, .data$entity_name, .data$entity_type, .data$entity, .data$high
    )

  return(data)
}

