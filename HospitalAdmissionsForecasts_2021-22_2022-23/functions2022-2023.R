###FluSight 2022-2023 Functions

#get data
  get_next_saturday <- function(date) {
    require(lubridate)
    date <- as.Date(date)
    ## calculate days until saturday (day 7)
    diff <- 7 - wday(date)
    ## add to given date
    new_date <- diff + date
    return(new_date)
  }
  
  get_next_tuesday <- function(date) {
    require(lubridate)
    date <- as.Date(date)
    ## calculate days until saturday (day 7)
    diff = ifelse(wday(date) <= (3), 3-wday(date), 3+7-wday(date))
    ## add to given date
    new_date <- diff + date
    return(new_date)
  }
  
  
  dat_for_scores_function <- function(all_dat, obs_data, includedf){
    
    all_dat = drop_na(all_dat) #additional confirmation that all quants are present
    
    dat_for_scores = all_dat %>% 
      mutate(target_end_date = as.Date(target_end_date),
             forecast_date = as.Date(forecast_date),
             value = value, 
             value = case_when(quantile==0.5 ~ round(value),
                               quantile<0.5 ~ floor(value),
                               quantile>0.5 ~ ceiling(value),
                               type=='point' ~ round(value)),
             submission_deadline =
               get_next_tuesday(as.Date(forecast_date))) %>%
      filter(! (type=="quantile" & is.na(quantile)),
             ! type == "point",
      ) %>% 
      left_join( obs_data %>% rename(report = value_inc), 
                 by=c("target_end_date", "location_name", "location"))%>%
      mutate(
        report = ifelse(grepl("inc flu hosp",target),
                        report)) %>% 
      rename(date = target_end_date) %>% 
      filter(!is.na(report))
    
    
    dat_for_scores = distinct(dat_for_scores) %>% 
      rename("prediction" = "value", "true_value" = "report") %>%
      filter(model %in% includedf$model)
    return(dat_for_scores)
  }

#WIS calculations
  # WIS_calculation <-function(score_df = dat_for_scores){
  #   
  #   dat_for_scores <- score_df %>% 
  #     mutate(alpha=ifelse(quantile < 0.500, quantile*2, 2*(1-quantile)), # Sets alpha levels for each interval 
  #            indicator = ifelse(quantile < 0.500 & report < value | quantile > 0.500 & report > value, 1, 0), #low indicator & high indicator
  #            pen = ifelse(indicator==1 & quantile < 0.500, (2/alpha)*(value-report), #low penalty
  #                         ifelse(indicator==1 & quantile > 0.500, (2/alpha)*(report-value), 0)) #high penalty
  #     ) 
  #   
  #   IS_per_alpha <- dat_for_scores %>% 
  #     mutate(alpha=as.factor(alpha)) %>%
  #     group_by(model, date, location_name, forecast_date, alpha) %>%
  #     summarize(value_diff=max(value)-min(value), #difference between forecasted value at max quantile and min quantile
  #               pen=sum(pen)) %>% #sum of pentaly for both quantiles within intervals
  #     ungroup() %>%
  #     mutate(alpha=as.numeric(as.character(alpha)),
  #            IS=(alpha/2)*(value_diff+pen)) %>% # Create interval scores 
  #     filter(alpha!="1.00")
  #   
  #   IS_sum <- IS_per_alpha %>%
  #     group_by(model, date, location_name, forecast_date) %>%
  #     summarize(IS_sum=sum(IS)) #Sum IS across all available intervals
  #   
  #   num_interval<-length(unique(IS_per_alpha$alpha))-1
  #   
  #   Q50 <-dat_for_scores %>%
  #     filter(quantile==0.500) %>%
  #     mutate(IS_Q50=(alpha/2)*abs(report-value)) #Previously = (alpha/2)*2*abs(report-value); multiplicative factor of 2 removed per hub update on 01/04 
  #   
  #   WIS <-IS_sum %>%
  #     left_join(., Q50, by=c("model", "date", "location_name", "forecast_date")) %>%
  #     mutate(WIS = (1/ (0.5 + num_interval))*(IS_Q50 + IS_sum), #add weights to the sum of all IS across the 11 intervals + the score at the 50% quantile; Previously = (1/ 1 + num_interval)*(IS_Q50 + IS_sum), but updated to per hub on 01/04
  #            forecast_date=as.Date(forecast_date)) %>% 
  #     dplyr::select(model,date, location_name, forecast_date, WIS) %>%
  #     as.data.frame(.)
  # }
  # 
  wis_all_function <- function(dat_for_scores){
    WIS_all  = dat_for_scores %>% scoringutils::score()
      # left_join(dat_for_scores %>%
      #            # filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
      #            filter(type == "quantile") %>%
      #             pivot_wider(names_from = c(type, quantile), values_from=value),
      #           WIS_calculation(dat_for_scores),
      #           by = c("model", "date", "location_name","forecast_date")) %>%
      # mutate(WIS_rel = ifelse(report==0, WIS/1, WIS/report),
      #        abs.error = abs(quantile_0.5 - report),
      #        perc.point.error = 100*abs.error/report,
      #        coverage.50 = ifelse(report >= quantile_0.25 & report <= quantile_0.75,T,F),
      #        coverage.95 = ifelse(report >= quantile_0.025 & report <= quantile_0.975,T,F)
      # ) %>%
      # rename(target_end_date = date)
    return(WIS_all)
  }

# #Season inc rankings
#   inc.rankings_all_func <- function(WIS_Season){
#     rankings =
#       make_WIS_ranking_baseline(WIS_Season) %>%
#       # select(-rank) %>%
#       filter(frac.forecasts.submitted >= 0.75) %>%
# 
#       mutate(model = model_display_names[as.character(model)],
#              mean.WIS = round(mean.WIS,2),
#              rel.WIS.skill = round(rel.WIS.skill,2),
#              MAE = round(MAE,2),
#              Percent.Cov.50 = round(100*Percent.Cov.50),
#              Percent.Cov.95 = round(100*Percent.Cov.95),
#              frac.forecasts.submitted = round(100*frac.forecasts.submitted),
#              frac.locations.submitted = round(100*frac.locations.submitted),
#              frac.locations.fully.forecasted = round(100*frac.locations.fully.forecasted),
#              frac.submitted.locations.fully.forecasted = round(100*frac.submitted.locations.fully.forecasted)) %>%
#       arrange(rel.WIS.skill)
#     return(rankings)
#   }
  
  # make_WIS_ranking_baseline = function(df){
  #   unique_models = unique(df$model)
  #   uniquetargets <- length(unique(df$target_end_date))
  #   uniquelocations <- length(unique(df$location_name))
  #   
  #   ranking_baseline <- data.frame(model = character(), mean.WIS = numeric(), rel.WIS.skill = numeric(), MAE = numeric(), 
  #                                  Percent.Cov.50 = numeric(),Percent.Cov.95 = numeric(), frac.forecasts.submitted = numeric(),
  #                                  frac.locations.submitted = numeric(), frac.locations.fully.forecasted = numeric(), 
  #                                  frac.submitted.locations.fully.forecasted = numeric())
  #   
  #   for (i in 1:length(unique_models)){
  #     step0 = df %>% filter(model == unique_models[i])
  #     
  #     locations.fully.submitted = step0 %>% group_by(location_name) %>% 
  #       summarise(subs = n()) %>% ungroup() %>% 
  #       #filter(subs >= 2*uniquetargets) %>% #possibly update if definition of fully submitted changes
  #       nrow()
  #     
  #     step1 = step0 %>% summarise(model = unique(model),
  #                                 mean.WIS = mean(WIS), 
  #                                 MAE = mean(abs.error, na.rm = TRUE), 
  #                                 Percent.Cov.50 = mean(coverage.50, na.rm = TRUE),
  #                                 Percent.Cov.95 = mean(coverage.95, na.rm = TRUE))
  #     
  #     step2 = left_join(x = filter(df, model == unique_models[i]), y = filter(df, model == "Flusight-baseline"), 
  #                       by = c("location", "target", "target_end_date")) %>% 
  #       summarise(model = unique(model.x),
  #                 rel.WIS.skill = mean(WIS.x)/mean(WIS.y))
  #     
  #     step3 = left_join(step1, step2, by = "model") %>% 
  #       mutate(frac.forecasts.submitted = 
  #                nrow(filter(df, model == unique_models[i]))/nrow(filter(df, model == "Flusight-baseline")),
  #              frac.locations.submitted = length(unique(step0$location_name))/uniquelocations,
  #              frac.locations.fully.forecasted = locations.fully.submitted/uniquelocations, 
  #              frac.submitted.locations.fully.forecasted = frac.locations.fully.forecasted/frac.locations.submitted
  #       ) %>% 
  #       select(model, mean.WIS, rel.WIS.skill, MAE, Percent.Cov.50, Percent.Cov.95, 
  #              frac.forecasts.submitted, frac.locations.submitted,
  #              frac.locations.fully.forecasted, frac.submitted.locations.fully.forecasted)
  #     
  #     ranking_baseline <- rbind(ranking_baseline, step3)
  #   }
  #   return(ranking_baseline)
  # }
  
  # make_WIS_ranking_location <- function(df){
  #   unique_models = unique(df$model)
  #   uniquetargets <- length(unique(df$target_end_date))
  #   uniquelocations <- length(unique(df$location_name))
  #   
  #   ranking_baseline <- data.frame(model = character(), location_name = character(), mean.WIS = numeric(), MAE = numeric(), 
  #                                  Percent.Cov.50 = numeric(),Percent.Cov.95 = numeric(), relative_WIS = numeric())
  #   
  #   for (i in 1:length(unique_models)){
  #     step0 = df %>% filter(model == unique_models[i])
  #     
  #     step1 = step0 %>% group_by(location_name) %>% summarise(model = unique(model),
  #                                                             mean.WIS = mean(WIS, na.rm = TRUE), 
  #                                                             MAE = mean(abs.error, na.rm = TRUE), 
  #                                                             Percent.Cov.50 = mean(coverage.50, na.rm = TRUE),
  #                                                             Percent.Cov.95 = mean(coverage.95, na.rm = TRUE))
  #     
  #     step2 = left_join(x = filter(df, model == unique_models[i]), y = filter(df, model == "Flusight-baseline"), 
  #                       by = c("location", "target", "target_end_date", "location_name")) %>% group_by(location_name) %>% 
  #       summarise(model = unique(model.x),
  #                 relative_WIS = mean(WIS.x)/mean(WIS.y))
  #     
  #     step3 = left_join(step1, step2, by = c("model", "location_name")) %>% 
  #       # mutate(frac.forecasts.submitted = 
  #       #          nrow(filter(df, model == unique_models[i]))/nrow(filter(df, model == "Flusight-baseline")),
  #       #        frac.locations.submitted = length(unique(step0$location_name))/uniquelocations,
  #       #        frac.locations.fully.forecasted = locations.fully.submitted/uniquelocations, 
  #       #        frac.submitted.locations.fully.forecasted = frac.locations.fully.forecasted/frac.locations.submitted
  #       #        ) %>% 
  #       select(model, location_name, mean.WIS, MAE, Percent.Cov.50, Percent.Cov.95, relative_WIS)
  #     
  #     ranking_baseline <- rbind(ranking_baseline, step3)
  #   }
  #   
  #   return(ranking_baseline)
  #   
  # }

#### this is a modified version of covidHubUtils::plot_forecast to better align the viz with the rest of the manuscript 
###added +theme_bw()
  plot_forecasts <- function (forecast_data, truth_data = NULL, hub = c("US", "ECDC", 
                                                                        "FluSight"), models = NULL, target_variable = NULL, locations = NULL, 
                              facet = NULL, facet_scales = "fixed", facet_nrow = NULL, 
                              facet_ncol = NULL, forecast_dates, intervals, horizon, truth_source, 
                              use_median_as_point = FALSE, plot_truth = TRUE, plot = TRUE, 
                              fill_by_model = FALSE, fill_transparency = 1, truth_as_of = NULL, 
                              top_layer = c("truth", "forecast"), title = "default", subtitle = "default", 
                              show_caption = TRUE) 
  {
    if (is.na(title)) {
      stop("Error in plot_forecasts: title argument interpretable as a character.")
    }
    if (is.na(subtitle)) {
      stop("Error in plot_forecasts: subtitle argument interpretable as a character.")
    }
    if (!missing(models)) {
      if (!all(models %in% forecast_data$model)) {
        stop("Error in plot_forecasts: Not all models are available in forecast data.")
      }
    }
    else {
      models <- unique(forecast_data$model)
    }
    if (missing(locations)) {
      locations <- unique(forecast_data$location)
    }
    else {
      locations <- name_to_fips(locations, hub)
    }
    hub <- match.arg(hub, choices = c("US", "ECDC", "FluSight"), 
                     several.ok = TRUE)
    if (hub[1] == "US") {
      valid_location_codes <- covidHubUtils::hub_locations$fips
      valid_target_variables <- c("cum death", "inc case", 
                                  "inc death", "inc hosp")
      valid_truth_sources <- c("JHU", "NYTimes", "HealthData")
    }
    else if (hub[1] == "ECDC") {
      valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
      valid_target_variables <- c("inc case", "inc death")
      valid_truth_sources <- c("JHU", "jhu", "ECDC", "ecdc")
    }
    else if (hub[1] == "FluSight") {
      valid_location_codes <- covidHubUtils::hub_locations_flusight$fips
      valid_target_variables <- c("inc flu hosp")
      valid_truth_sources <- c("HealthData")
    }
    if (!all(locations %in% forecast_data$location)) {
      stop("Error in plot_forecasts: Not all locations are available in forecast_data.")
    }
    locations <- match.arg(locations, choices = valid_location_codes, 
                           several.ok = TRUE)
    if (length(locations) > 1) {
      if (is.null(facet)) {
        stop("Error in plot_forecasts: Passed in multiple locations without a facet command")
      }
    }
    if (missing(target_variable)) {
      if (length(unique(forecast_data$target_variable)) == 
          1) {
        target_variable <- unique(forecast_data$target_variable)
      }
      else {
        stop("Error in plot_forecasts: Target variable unspecified and more than one target_variable in data.")
      }
    }
    else {
      if (!(target_variable %in% forecast_data$target_variable)) {
        stop("Error in plot_forecasts: Please provide a valid target variable.")
      }
      target_variable <- match.arg(target_variable, choices = valid_target_variables, 
                                   several.ok = FALSE)
    }
    if (!is.null(truth_data)) {
      columns_check <- all(c("model", "target_variable", "target_end_date", 
                             "location", "value") %in% colnames(truth_data))
      if (!columns_check) {
        stop("Error in plot_forecasts: Please provide columns model, \n           target_variable, target_end_date, location and value in truth_data.")
      }
      else {
        if (!all(truth_data$location %in% valid_location_codes)) {
          stop("Error in plot_forecasts: Please make sure all fips codes in location column are valid.")
        }
        if (!all(locations %in% truth_data$location)) {
          stop("Error in plot_forecasts: At least one forecasted location not available in truth_data.")
        }
        if (!(target_variable %in% truth_data$target_variable)) {
          stop("Error in plot_forecasts: Please provide a valid target variable.")
        }
      }
    }
    else {
      truth_source <- match.arg(truth_source, choices = valid_truth_sources, 
                                several.ok = FALSE)
      if (target_variable == "inc hosp" | target_variable == 
          "inc flu hosp") {
        if (truth_source != "HealthData") {
          stop("Error in plot_forecasts: Incident hopsitalization truth data is only available from HealthData.gov now.")
        }
      }
      else {
        if (truth_source == "HealthData") {
          stop("Error in plot_forecasts: This function does not support selected target_variable from HealthData.")
        }
      }
    }
    if (show_caption) {
      if (missing(truth_source)) {
        stop("Error in plot_forecasts: Please provide truth_source for caption.")
      }
    }
    if (missing(forecast_dates)) {
      forecast_dates <- unique(forecast_data$forecast_date)
    }
    else {
      forecast_dates <- as.Date(forecast_dates)
      if (!all(forecast_dates %in% forecast_data$forecast_date)) {
        stop("Error in plot_forecasts: Not all forecast_dates are available in forecast data.")
      }
    }
    if (missing(intervals)) {
      lower_bounds <- unique(forecast_data[forecast_data$type == 
                                             "quantile" & forecast_data$quantile < 0.5, ]$quantile)
      if ("NA" %in% lower_bounds) {
        intervals <- NULL
      }
      else {
        if (length(unique(models)) > 5) {
          intervals <- c(0.95)
        }
        else {
          intervals <- lapply(lower_bounds, function(l) {
            1 - as.numeric(2 * l)
          })
          if (all(c(0.5, 0.8, 0.95) %in% intervals)) {
            intervals <- c(0.5, 0.8, 0.95)
          }
        }
      }
    }
    if (!is.null(intervals) & length(unique(models)) > 5) {
      intervals <- c(0.95)
    }
    quantiles_to_plot <- unlist(lapply(intervals, function(interval) {
      c(0.5 - as.numeric(interval)/2, 0.5 + as.numeric(interval)/2)
    }))
    if (use_median_as_point) {
      if (0.5 %in% forecast_data$quantile) {
        quantiles_to_plot <- append(quantiles_to_plot, 0.5)
      }
      else {
        stop("Error in plot_forecasts: Median quantiles are not available in forecast_data.")
      }
    }
    if (fill_by_model) {
      if (length(unique(models)) <= 5) {
        color_families <- c("Blues", "Oranges", "Greens", 
                            "Purples", "Reds")
        if (use_median_as_point) {
          colourCount <- (length(quantiles_to_plot) - 1)/2 + 
            1
        }
        else {
          colourCount <- length(quantiles_to_plot)/2 + 
            1
        }
        model_colors <- purrr::map(color_families[1:length(unique(models))], 
                                   function(color_family) {
                                     getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, 
                                                                                                        color_family))
                                     if (colourCount < 4) {
                                       getPalette(4) %>% tail(colourCount)
                                     }
                                     else {
                                       getPalette(colourCount)
                                     }
                                   })
        ribbon_colors <- RColorBrewer::brewer.pal(max(4, 
                                                      colourCount), "Greys")
        ribbon_colors <- ribbon_colors[seq_len(length(ribbon_colors) - 
                                                 1)] %>% tail(colourCount)
        forecast_colors <- unlist(lapply(model_colors, tail, 
                                         n = 1))
        interval_colors <- unlist(lapply(model_colors, head, 
                                         n = colourCount - 1))
      }
      else {
        if (use_median_as_point) {
          colourCount <- (length(quantiles_to_plot) - 1)/2
        }
        else {
          colourCount <- length(quantiles_to_plot)/2
        }
        modelCount <- length(unique(models))
        getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, 
                                                                           "Set1"))
        model_colors <- getPalette(modelCount)
        ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[1:3]
        forecast_colors <- unlist(lapply(model_colors, tail, 
                                         n = 1))
        interval_colors <- unlist(lapply(model_colors, function(color) {
          colorspace::lighten(color, 0.2)
        }))
      }
    }
    else {
      if (use_median_as_point) {
        colourCount <- max((length(quantiles_to_plot) - 1)/2 + 
                             1, 2)
      }
      else {
        colourCount <- max(length(quantiles_to_plot)/2 + 
                             1, 2)
      }
      getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, 
                                                                         "Blues"))
      blues <- getPalette(colourCount)
      forecast_colors <- rep(tail(blues, 1), length(unique(models)))
      blues[1:2] <- colorspace::darken(blues[1:2], 0.1)
      interval_colors <- rep(blues[1:(length(blues) - 1)], 
                             length(unique(models)))
      ribbon_colors <- blues[1:(length(blues) - 1)]
    }
    if (!is.null(truth_as_of)) {
      warning("Warning in plot_forecasts: truth_as_of is not used to load versioned truth data.\n            Will be available soon.")
    }
    plot_data <- get_plot_forecast_data(forecast_data = forecast_data, 
                                        truth_data = truth_data, models_to_plot = models, forecast_dates_to_plot = as.Date(forecast_dates), 
                                        horizons_to_plot = horizon, quantiles_to_plot = quantiles_to_plot, 
                                        locations_to_plot = locations, plot_truth = plot_truth, 
                                        truth_source = truth_source, target_variable_to_plot = target_variable, 
                                        hub = hub)
    if (show_caption) {
      if (!is.null(truth_as_of)) {
        caption <- paste0("source: ", truth_source, " (observed data as of ", 
                          as.Date(truth_as_of), "), ", paste(models, collapse = ", "), 
                          " (forecasts)")
        caption <- paste(strwrap(caption, grDevices::dev.size("px")[1]), 
                         collapse = "\n")
      }
      else {
        caption <- paste0("source: ", truth_source, " (observed data), ", 
                          paste(models, collapse = ", "), " (forecasts)")
        caption <- paste(strwrap(caption, grDevices::dev.size("px")[1]), 
                         collapse = "\n")
      }
    }
    else {
      caption <- NULL
    }
    if (target_variable == "cum death") {
      full_target_variable <- "Cumulative Deaths"
    }
    else if (target_variable == "inc case") {
      full_target_variable <- "Incident Cases"
    }
    else if (target_variable == "inc death") {
      full_target_variable <- "Incident Deaths"
    }
    else if (target_variable == "inc hosp" | target_variable == 
             "inc flu hosp") {
      full_target_variable <- "Incident Hospitalizations"
    }
    plot_data_forecast <- plot_data %>% dplyr::filter(truth_forecast == 
                                                        "forecast")
    plot_data_truth <- plot_data %>% dplyr::filter(!is.na(point), 
                                                   truth_forecast == "truth") %>% dplyr::rename(truth_model = model) %>% 
      dplyr::select(-forecast_date)
    if (title == "default") {
      if (target_variable == "inc hosp" & hub[1] != "FluSight") {
        title <- paste0("Daily COVID-19 ", full_target_variable, 
                        ": observed and forecasted")
      }
      else if (hub[1] == "FluSight") {
        title <- paste0("Weekly Influenza ", full_target_variable, 
                        ": observed and forecasted")
      }
      else {
        title <- paste0("Weekly COVID-19 ", full_target_variable, 
                        ": observed and forecasted")
      }
    }
    if (title == "none") {
      title <- NULL
    }
    if (subtitle == "default") {
      subtitle <- paste0("Selected location(s): ", paste(unique(plot_data_forecast$location), 
                                                         collapse = ", "), "\nSelected forecast date(s): ", 
                         paste(unique(plot_data_forecast$forecast_date), collapse = ", "))
    }
    if (subtitle == "none") {
      subtitle <- NULL
    }
    graph <- ggplot2::ggplot(data = plot_data_forecast, ggplot2::aes(x = target_end_date))
    if (top_layer[1] == "truth") {
      if (!is.null(intervals)) {
        graph <- graph + 
          ggplot2::geom_ribbon(data = plot_data_forecast %>% 
                                 dplyr::filter(type == "quantile"), 
                               mapping = ggplot2::aes(ymin = lower, 
                                                      ymax = upper, 
                                                      group = interaction(`Prediction Interval`,  model, location, forecast_date), 
                                                      fill = interaction(`Prediction Interval`,  model)), 
                               alpha = fill_transparency, show.legend = FALSE) + 
          ggplot2::scale_fill_manual(name = "Prediction Interval", 
                                     values = interval_colors) + ggnewscale::new_scale_fill() + 
          ggplot2::geom_ribbon(data = plot_data_forecast %>% 
                                 dplyr::filter(type == "quantile"), 
                               mapping = ggplot2::aes(ymin = lower, 
                                                      ymax = upper, 
                                                      fill = `Prediction Interval`), 
                               alpha = 0) + 
          ggplot2::scale_fill_manual(name = "Prediction Interval", values = ribbon_colors) + 
          ggplot2::geom_line(data = plot_data_forecast %>% 
                               dplyr::filter(type == "quantile"), 
                             mapping = ggplot2::aes(y = upper,colour = model), 
                             alpha = 0) + 
          ggplot2::scale_color_manual(name = "Model",  values = forecast_colors) + 
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
      }
      graph <- graph + 
        ggplot2::geom_line(data = plot_data_forecast %>% 
                             dplyr::filter(!is.na(point)), 
                           mapping = ggplot2::aes(x = target_end_date, 
                                                  y = point, 
                                                  group = interaction(model, location, forecast_date), 
                                                  color = model)) + 
        ggplot2::geom_point(data = plot_data_forecast %>% 
                              dplyr::filter(!is.na(point)), 
                            mapping = ggplot2::aes(x = target_end_date,
                                                   y = point, 
                                                   color = model)) + 
        ggplot2::scale_color_manual(name = "Model", values = forecast_colors) + 
        ggnewscale::new_scale_color() + 
        ggplot2::geom_line(data = plot_data_truth %>% 
                             dplyr::filter(!is.na(point)), 
                           mapping = ggplot2::aes(x = target_end_date, 
                                                  y = point, 
                                                  color = truth_model)) + 
        ggplot2::geom_point(data = plot_data_truth %>% 
                              dplyr::filter(!is.na(point)), mapping = ggplot2::aes(x = target_end_date, 
                                                                                   y = point, color = truth_model)) + 
        ggplot2::scale_color_manual(name = "Truth", values = "black")
    }
    else if (top_layer[1] == "forecast") {
      graph <- graph + ggplot2::geom_line(data = plot_data_truth %>% 
                                            dplyr::filter(!is.na(point)), mapping = ggplot2::aes(x = target_end_date, 
                                                                                                 y = point, color = truth_model)) + 
        ggplot2::geom_point(data = plot_data_truth %>% 
                              dplyr::filter(!is.na(point)), 
                            mapping = ggplot2::aes(x = target_end_date,y = point, color = truth_model)) + 
        ggplot2::scale_color_manual(name = "Truth", values = "black") + 
        ggnewscale::new_scale_color()
      if (!is.null(intervals)) {
        graph <- graph + 
          ggplot2::geom_ribbon(data = plot_data_forecast %>% 
                                 dplyr::filter(type == "quantile"), 
                               mapping = ggplot2::aes(ymin = lower,  
                                                      ymax = upper, 
                                                      group = interaction(`Prediction Interval`, model, location, forecast_date), 
                                                      fill = interaction(`Prediction Interval`, model)), 
                               alpha = fill_transparency, show.legend = FALSE) + 
          ggplot2::scale_fill_manual(name = "Prediction Interval", 
                                     values = interval_colors) + ggnewscale::new_scale_fill() + 
          ggplot2::geom_ribbon(data = plot_data_forecast %>% 
                                 dplyr::filter(type == "quantile"), mapping = ggplot2::aes(ymin = lower, 
                                                                                           ymax = upper, fill = `Prediction Interval`), 
                               alpha = 0) + ggplot2::scale_fill_manual(name = "Prediction Interval", 
                                                                       values = ribbon_colors) + 
          ggplot2::geom_line(data = plot_data_forecast %>% 
                               dplyr::filter(type == "quantile"), mapping = ggplot2::aes(y = upper, colour = model), alpha = 0) + 
          ggplot2::scale_color_manual(name = "Model",  values = forecast_colors) + 
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
      }
      graph <- graph + 
        ggplot2::geom_line(data = plot_data_forecast %>% 
                             dplyr::filter(!is.na(point)), 
                           mapping = ggplot2::aes(x = target_end_date, y = point, group = interaction(model, location, forecast_date), color = model)) +
        ggplot2::geom_point(data = plot_data_forecast %>%  dplyr::filter(!is.na(point)), mapping = ggplot2::aes(x = target_end_date,                                                                                                                                                     y = point, color = model)) + 
        ggplot2::scale_color_manual(name = "Model",  values = forecast_colors)
    }
    if (!is.null(facet)) {
      graph <- graph + ggplot2::facet_wrap(facets = facet, 
                                           scales = facet_scales, nrow = facet_nrow, ncol = facet_ncol, 
                                           labeller = ggplot2::label_wrap_gen(multi_line = FALSE))
    }
    graph <- graph + ggplot2::scale_x_date(name = NULL, date_breaks = "1 month", 
                                           date_labels = "%b %d") + ggplot2::ylab(full_target_variable) + 
      ggplot2::labs(title = title, subtitle = subtitle, caption = caption) + 
      theme_bw() +
      theme(legend.position = "bottom")
    if (plot) {
      print(graph)
    }
    return(invisible(graph))
  }
  
  
  #Forecasts and observed 
  forecastsandobservedplt <- function(all_dat, obs_data, a= "a"){
    plot_data_forecast <- all_dat %>% filter(model == "Flusight-ensemble", location == "US", forecast_date >= (as.Date(min(all_dat$forecast_date)) + 7)
                                             ) %>% #target_end_date >= "2022-02-05"
      mutate(temporal_resolution = "wk",
             target_variable = "inc flu hosp",
             horizon = case_when(target == "1 wk ahead inc flu hosp" ~ "1", 
                                 target == "2 wk ahead inc flu hosp" ~ "2",
                                 target == "3 wk ahead inc flu hosp" ~ "3",
                                 target == "4 wk ahead inc flu hosp"~ "4", 
                                 TRUE ~ NA),
             full_location_name = location_name, 
             target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
             forecast_date = as.Date(forecast_date, format = "%Y-%m-%d")) %>% 
      filter(forecast_date %in% c(min(forecast_date) + 28*c(0:15)))
    
    obs <- obs_data %>% 
      filter(location_name == "National") %>% 
     # filter(target_end_date >= "2022-02-02") %>% 
      mutate(model = "Observed Data",
             target_variable = "inc flu hosp",
             location_name = case_when(location_name == "US" ~ "National", 
                                       TRUE ~ location_name), 
             full_location_name = location_name,
             value = value_inc, 
             target_end_date <- as.Date(target_end_date, format = "%Y-%m-%d")) %>% 
      select(target_end_date, location, location_name, value, model, target_variable, full_location_name, season)
    
    plttitle = " " 
    
    fig = plot_forecasts(plot_data_forecast,
                         truth_data = obs,
                         models = "Flusight-ensemble",
                         truth_source = "HealthData.gov",
                         facet = "season",
                         facet_scales = "free",
                         locations = c("US"),
                         use_median_as_point = T,
                         subtitle = "",
                         title = plttitle
                         )
    return(fig)
  }

#Coverage figures
  scores_tab_function <- function(inc.rankings_location,inc.rankings_all, WIS_Season){
    Scores_sum = inc.rankings_location %>% group_by(model) %>% summarise(
      model = model,
      #Absolute_WIS = mean(mean.WIS, na.rm = TRUE),
      #Relative_WIS = mean(is.finite(relative_WIS), na.rm = TRUE),
      below_baseline_pct = mean(below, na.rm = TRUE)*100
    ) %>% unique()
    
    Scores_sum = merge(inc.rankings_all, Scores_sum, by = "model") %>% select(model, wis, rel_wis, below_baseline_pct) 
    
    weekly_breakdown = WIS_Season %>% group_by(model) %>% summarise(
      model = model,
      One_week_Cov = mean(coverage.95[target == "1 wk ahead inc flu hosp"], na.rm = TRUE)*100,
      Two_week_Cov = mean(coverage.95[target == "2 wk ahead inc flu hosp"], na.rm = TRUE)*100,
      Three_week_Cov = mean(coverage.95[target == "3 wk ahead inc flu hosp"], na.rm = TRUE)*100,
      Four_week_Cov = mean(coverage.95[target == "4 wk ahead inc flu hosp"], na.rm = TRUE)*100
    ) %>% unique() %>% 
      ungroup()
    
    Scores_tab = merge(Scores_sum, weekly_breakdown, by = "model") %>% select(-wis) %>% rename(Relative_WIS = rel_wis)
    
    return(Scores_tab)
  }

#Coverage figures

  cov95_function <- function(WIS_Season, Scores_tab){
    coverage95_states_horizon = WIS_Season %>% filter(location_name != "National") %>% 
      #  filter(target == "1 wk ahead inc flu hosp") %>% 
      group_by(model, date, target) %>% 
      summarise(model = model,
                date = as.Date(date, format = "%Y-%m-%d"),
                target = target,
                coverage95 = mean(coverage.95,na.rm = TRUE)) %>% unique()
    
    cov95_breakdown <- coverage95_states_horizon %>% 
      group_by(model) %>% 
      summarise(model = model,
                One_week_Percent_above_90 = (length(coverage95[target == "1 wk ahead inc flu hosp" & coverage95 >= 0.9])/length(coverage95[target == "1 wk ahead inc flu hosp"]))*100,
                Two_week_Percent_above_90 = (length(coverage95[target == "2 wk ahead inc flu hosp" & coverage95 >= 0.9])/length(coverage95[target == "2 wk ahead inc flu hosp"]))*100,
                Three_week_Percent_above_90 = (length(coverage95[target == "3 wk ahead inc flu hosp" & coverage95 >= 0.9])/length(coverage95[target == "3 wk ahead inc flu hosp"]))*100,
                Four_week_Percent_above_90 = (length(coverage95[target == "4 wk ahead inc flu hosp" & coverage95 >= 0.9])/length(coverage95[target == "4 wk ahead inc flu hosp"]))*100,
      ) %>% unique()
    
    cov95_breakdown_all <- merge(Scores_tab, cov95_breakdown, by = "model")
    return(cov95_breakdown_all)
  }

#Backfill Epicurve

  backfilldata_func <- function(startdate = as.Date("2022-02-07"), enddate = as.Date("2022-07-18")){
    jsonpath = read.socrata(url = "https://healthdata.gov/resource/qqte-vkut.json")
    
    finalset = data.frame(state = character(), date = as.Date(character()), value = integer(), report_date = character(), 
                          epiweek = character(),epiyear = character())
    
    #loop to pull in covidcast api data and aggregate it weekly
    for(k in seq.Date(from = as.Date(startdate), to = as.Date(enddate), by = "1 week")){
      d = as.Date(k, origin = "1970-01-01")
      filename = case_when(length(jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12]) > 1 ~
                             jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][2],
                           TRUE ~  jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][1])
      
      df = read.csv(filename) %>% mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) -1)
      df1 = df %>% filter(as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) > as.Date(d-9), date < as.Date(d - 1), state != "AS", state != "VI") %>% 
        select(state, date, previous_day_admission_influenza_confirmed) %>%
        mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")), report_date = (as.Date((get_next_saturday(d) - 7), "%Y-%m-%d")), epiweek = epiweek(date), epiyear = epiyear(date), .keep = "all") %>% 
        rename(value = previous_day_admission_influenza_confirmed) %>% select(state, date, value, report_date, epiweek, epiyear)
      finalset = rbind(finalset, df1)
      
    }
    return(finalset)
  }
  
  
  
  nation_summary_func <- function(finalset){
    influenzadat <-  finalset %>% group_by(report_date, state) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup() 
    
    groupsum <- influenzadat %>% group_by(report_date) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>% mutate(state = "US") %>% select(report_date, state, value)
    
    forecastdata <- rbind(influenzadat, groupsum) 
    return(forecastdata)
  }

#backfill Differences
  diff_df_function <- function(truthtransmute, forecastdata){
    truthdat = truthtransmute %>% 
      mutate(state = case_when(location_name == "US" ~ "US", 
                               location_name == "District of Columbia" ~ "DC", 
                               location_name == "Puerto Rico" ~ "PR", 
                               location_name == "Virgin Islands" ~ "VI",
                               TRUE ~ state.abb[match(location_name, state.name)])) %>% 
      rename(date = wk_end_date, final_value = value, truth_date = report_date)
    
    fullset2 = left_join(forecastdata, truthdat, by = c("state" = "state", "report_date" = "date"))
    
    diffdf = fullset2 %>% arrange(report_date, location_name) %>% 
      group_by(location_name) %>%  
      mutate(final_perc_change = (final_value - value)/value,
             absolutediff = (final_value - value)) %>% ungroup()
    return(diffdf)
  }

#Backfill matrix plot 
  hosp_data_read_func <- function(from = as.Date("2022-10-17"), to = as.Date("2023-05-23"), tuesday = FALSE){
    jsonpath <- read.socrata(url = "https://healthdata.gov/resource/qqte-vkut.json")
    finalset <- data.frame(state = character(), date = as.Date(character()), value = integer(), report_date = character(), epiweek = character(),epiyear = character())
    
    #loop to pull in api data and aggregate it weekly
    for(k in seq.Date(from = from, to = to, by = "1 week")){
      d = as.Date(k, origin = "1970-01-01")
      tryCatch({
        filename = case_when(length(jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12]) > 1 ~
                               jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][2],
                             TRUE ~  jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][1])
        
        df = read.csv(filename) %>% mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) -1)
        if(tuesday == FALSE){
          df1 = df %>% filter(as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) > from-9, date < as.Date(d - 1), state != "AS", state != "VI") 
        } else { 
          df1 = df %>% filter(as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) > from-10, date < as.Date(d - 2), state != "AS", state != "VI")
        }
        df1 = df1 %>% 
          select(state, date, previous_day_admission_influenza_confirmed) %>%
          mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")), report_date = (as.Date(d, "%Y-%m-%d")), epiweek = epiweek(date), epiyear = epiyear(date), .keep = "all") %>%
          rename(value = previous_day_admission_influenza_confirmed) %>% select(state, date, value, report_date, epiweek, epiyear)
        finalset = rbind(finalset, df1)
      }, error = function(e){cat("Error:", conditionMessage(e), "\n")})
      
    }
    return(finalset)
  }
  