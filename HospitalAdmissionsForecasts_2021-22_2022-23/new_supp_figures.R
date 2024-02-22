forecastsandobservedplt <- function(all_dat, obs_data, a= "a"){
  plot_data_forecast <- all_dat %>% filter(model == "Flusight-ensemble", location == "US", forecast_date >= (as.Date(min(all_dat$forecast_date)) + 21)) %>% #target_end_date >= "2022-02-05"
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
    filter(forecast_date %in% c(min(forecast_date) + 28*c(0:10)))
  
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
    select(target_end_date, location, location_name, value, model, target_variable, full_location_name)
  
  plttitle = case_when(a == "a" ~ "2021-22", a == "b" ~ "2022-23")
  
  fig = plot_forecasts(plot_data_forecast,
                       truth_data = obs,
                       models = "Flusight-ensemble",
                       truth_source = "HealthData.gov",
                       locations = c("US"),
                       use_median_as_point = T,
                       subtitle = "",
                       title = plttitle)+ theme(text = element_text(size = 15))  
  return(fig)
}