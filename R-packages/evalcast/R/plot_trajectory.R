#' Produce trajectory plots of forecasts and observed data
#'
#' @param list_of_predictions_cards collection of prediction cards as returned 
#'   by `get_predictions()`. Supports collections of multiple forecast dates and
#'   forecasters
#' @param point_fcast a character string, plot either the median (default) or
#'   a point forecast
#' @param first_day the earliest date to display
#' @param last_day the latest date to display. Defaults to the most recent data
#'   available (plus future, if forecasts are past today)
#' @param alpha Displayed ribbons are the 1-alpha coverage results
#' @param nrow number of rows in the facet display
#' @param ncol number of columns in the facet display
#' @param facets_per_page maximum facets per page
#' @param force_single_page overrides the above faceting options and puts all
#'   facets in one figure
#'
#' @return Produces a ggplot object
#' @export
#' @importFrom rlang .data
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom MMWRweek MMWRweek2Date MMWRweek
#'
#' @examples
#' sister_preds = get_predictions(
#'   baseline_forecaster, "sister",
#'   tibble::tibble(data_source="usa-facts", signal = "deaths_incidence_num",
#'     start_day=lubridate::ymd("2020-07-01")),
#'     lubridate::ymd("2020-09-01"),"epiweek", 1:4, "state", c("mi"))
#' baby_preds = get_predictions(
#'   baseline_forecaster, "baby",
#'   tibble::tibble(data_source="usa-facts", signal = "deaths_incidence_num",
#'     start_day=lubridate::ymd("2020-07-01")),
#'     lubridate::ymd("2020-09-08"),"epiweek", 1:4, "state", c("mi"))
#'  plot_trajectory(c(sister_preds, baby_preds), last_day="2020-10-01")
plot_trajectory <- function(list_of_predictions_cards,
                            point_fcast = c("median", "point"),
                            first_day = "2020-07-01",
                            last_day = Sys.Date(),
                            alpha = .2,
                            nrow = 6,
                            ncol = 4,
                            facets_per_page = 52,
                            force_single_page = FALSE){
  # make sure predictions cards are for the same forecasting task (except ahead, and forecast_date)
  response <- unique_attr(list_of_predictions_cards,"signals")
  incidence_period <- unique_attr(list_of_predictions_cards,"incidence_period")
  geo_type <- unique_attr(list_of_predictions_cards, "geo_type")
  geo_values <- unique_attr(list_of_predictions_cards, "geo_values")
  assertthat::assert_that(incidence_period %in% c("day","epiweek"))
  assertthat::assert_that(geo_type %in% c("county","state"))
  point_fcast <- match.arg(point_fcast)
  
  
  # predicted quantiles to plot
  plot_probs <- c(alpha / 2, 1 - alpha / 2)
  
  preds_df <- list_of_predictions_cards %>% 
    aggregate_cards() %>%
    mutate(prob_type = case_when(
      abs(probs - plot_probs[1]) <= 1e-8 ~ "lower",
      (point_fcast == "median") & (abs(probs - .5) < 1e-8) ~ "point",
      abs(probs - plot_probs[2]) <= 1e-8 ~ "upper",
      (point_fcast == "point") & is.na(probs) ~ "point")) %>%
    filter(!is.na(.data$prob_type))
  
  preds_df <- preds_df %>%
    mutate(
      target_period = if_else(
        .data$incidence_period == "day", 
        .data$forecast_date + .data$ahead, # if true
        MMWRweek2Date( # epiweek, 
          lubridate::year(.data$forecast_date), # grab the year
          MMWRweek(.data$forecast_date)$MMWRweek + .data$ahead - # increment epiweek
            as.numeric(wday(.data$forecast_date) < 3)) # but current week if S or M
      )) %>%
    dplyr::select(.data$location, .data$quantiles, .data$name_of_forecaster,
                  .data$prob_type, .data$target_period, 
                  .data$forecast_date) %>%
    pivot_wider(values_from = .data$quantiles, names_from = .data$prob_type) %>%
    rename(reference_period = target_period) %>% 
    mutate(location_abbr = grab_locations(location, geo_type)) %>% 
    filter(!is.na(location_abbr))
  
  
    
  
  # ground truth to plot
  if(incidence_period == "day") {
    response_df <- download_signal(data_source = response$data_source,
                                   signal = response$signal,
                                   start_day = first_day,
                                   end_day = last_day,
                                   geo_type = geo_type,
                                   geo_values = geo_values) %>%
      dplyr::select(.data$location, .data$time_value, .data$value) %>%
      rename(reference_period = .data$time_value) %>%
      filter(location %in% preds_df$location) 
  } else {
    # avoid summing over partial epiweeks
    date_range <- covidcast::covidcast_meta() %>% 
      filter(data_source == response$data_source & 
               signal == response$signal &
               geo_type == !!geo_type) %>%
      dplyr::select(min_time, max_time)
    
    sunday_following_first_day <- shift_day_to_following_xxxday(
      max(ymd(date_range$min_time, first_day)), xxx = 1)
    saturday_preceding_last_day <- shift_day_to_preceding_xxxday(
      min(ymd(date_range$max_time, last_day)), xxx = 7)
    assertthat::assert_that(
      sunday_following_first_day < saturday_preceding_last_day,
      msg = "Pick first and last day to span at least one full epiweek.")
    
    response_df <- download_signal(data_source = response$data_source,
                                   signal = response$signal,
                                   start_day = sunday_following_first_day,
                                   end_day = saturday_preceding_last_day,
                                   geo_type = geo_type,
                                   geo_values = geo_values) %>%
      dplyr::select(location, .data$time_value, .data$value) %>%
      sum_to_epiweek() %>%
      filter(location %in% preds_df$location)
  }
  
  response_df <- response_df %>% 
    mutate(location_abbr = grab_locations(location, geo_type)) %>%
    filter(!is.na(location_abbr))
  
    
  # Set up the page number by the number of facets
  nfacets = length(unique(preds_df$location_abbr)) 

  # Framework of the trajectory plot
  p <- ggplot(preds_df, aes(x = .data$reference_period)) +
    geom_line(aes(y = .data$point, col = .data$name_of_forecaster, 
                  group = .data$forecast_date), size = 1) + 
    geom_point(aes(y = .data$point, col = .data$name_of_forecaster, 
                   group = .data$forecast_date)) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper,
                    fill = .data$name_of_forecaster, 
                    group = .data$forecast_date),
                alpha = .1, colour = NA, show.legend = FALSE) + 
    geom_line(data=response_df, aes(y = .data$value), size = 1) + 
    scale_color_discrete(na.translate = FALSE) +
    labs(x = incidence_period,
         y = paste0(response$data_source,": ",response$signal),
         colour = "forecaster") +
    theme_bw() + 
    theme(axis.text = element_text(size = 8), 
          strip.text = element_text(size = 10,face = "bold"))
  
  if (force_single_page) {
    print(p + facet_wrap(~.data$location_abbr, scales = "free_y"))
  } else if (nfacets <= facets_per_page) {
    print(p + facet_wrap(~.data$location_abbr, scales = "free_y", 
                         ncol = ncol, nrow = nrow))
  } else {
    facet_page <- 0
    stop_flag <- TRUE
    while (stop_flag) {
      facet_page <- facet_page + 1
      fac_plt <- p + 
        ggforce::facet_wrap_paginate(~.data$location_abbr, scales = "free_y", 
                                     ncol = ncol, nrow = nrow, 
                                     page = facet_page)
      stop_flag <- ggforce::n_pages(fac_plt) > facet_page
      print(fac_plt)
    }
  }
}



sum_to_epiweek <- function(daily_df){
  daily_df %>%
    mutate(epiweek = MMWRweek(.data$time_value)$MMWRweek,
           epiyear = MMWRweek(.data$time_value)$MMWRyear) %>%
    select(-.data$time_value) %>%
    group_by(.data$location, .data$epiyear, .data$epiweek) %>%
    summarise(value = sum(.data$value)) %>%
    mutate(reference_period = MMWRweek2Date(epiyear, epiweek))
}

shift_day_to_preceding_xxxday <- function(day, xxx){
  ew_day <- MMWRweek(day)
  if (ew_day$MMWRday < xxx) {
    MMWRweek2Date(ew_day$MMWRyear, ew_day$MMWRweek, xxx) - 7
  } else {
    MMWRweek2Date(ew_day$MMWRyear, ew_day$MMWRweek, xxx)
  }
}

shift_day_to_following_xxxday <- function(day,xxx){
  ew_day <- MMWRweek(day)
  if(ew_day$MMWRday > xxx)
  {
    MMWRweek2Date(ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx) + 7
  } else {
    MMWRweek2Date(ew_day$MMWRyear, MMWRweek = ew_day$MMWRweek, MMWRday = xxx)
  }
}


grab_locations <- function(location, geo_type="state"){
  switch(geo_type,
         state = covidcast::fips_to_abbr(paste0(location,"000")),
         county = covidcast::fips_to_name(location)
  )
}


