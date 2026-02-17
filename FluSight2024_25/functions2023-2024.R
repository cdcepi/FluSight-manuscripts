###FluSight 2023/24Functions


dat_for_scores_function <- function(all_dat, obs_data, includedf){
  
  all_dat = drop_na(all_dat) #additional confirmation that all quants are present
  
  dat_for_scores = all_dat %>% 
    mutate(target_end_date = as.Date(target_end_date),
           forecast_date = as.Date(forecast_date),
           value = value, 
           submission_deadline = #get next Tuesday submission date
             lubridate::ceiling_date(forecast_date, unit = "week", week_start = getOption("lubridate.week.start", 2))) %>% 
    filter(! (type=="quantile" & is.na(quantile)),
           ! type == "point",
    ) %>% 
    left_join( obs_data, 
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

dat_for_scores_function_log <- function(all_dat, obs_data, includedf){
  
  all_dat = drop_na(all_dat) #additional confirmation that all quants are present
  
  dat_for_scores = all_dat %>% 
    mutate(target_end_date = as.Date(target_end_date),
           forecast_date = as.Date(forecast_date),
           logvalue = logvalue, 
           submission_deadline = #get next Tuesday submission date
             lubridate::ceiling_date(forecast_date, unit = "week", week_start = getOption("lubridate.week.start", 2))) %>% 
    filter(! (type=="quantile" & is.na(quantile)),
           ! type == "point",
    ) %>% 
    left_join( obs_data, 
               by=c("target_end_date", "location_name", "location"))%>%
    mutate(
      logreport = ifelse(grepl("inc flu hosp",target),
                      logreport)) %>% 
    rename(date = target_end_date) %>% 
    filter(!is.na(logreport))
  
  
  dat_for_scores = distinct(dat_for_scores) %>% 
    rename("prediction" = "logvalue", "true_value" = "logreport") %>%
    filter(model %in% includedf$model)
  return(dat_for_scores)
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
      Zero_week_Cov = mean(coverage.95[target == "0 wk inc flu hosp"], na.rm = TRUE)*100,
      One_week_Cov = mean(coverage.95[target == "1 wk inc flu hosp"], na.rm = TRUE)*100,
      Two_week_Cov = mean(coverage.95[target == "2 wk inc flu hosp"], na.rm = TRUE)*100,
      Three_week_Cov = mean(coverage.95[target == "3 wk inc flu hosp"], na.rm = TRUE)*100
    ) %>% unique() %>% 
      ungroup()
    
    Scores_tab = merge(Scores_sum, weekly_breakdown, by = "model") %>% select(-wis) %>% rename(Relative_WIS = rel_wis)
    
    return(Scores_tab)
  }
  
