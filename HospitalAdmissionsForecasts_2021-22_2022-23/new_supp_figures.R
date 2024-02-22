######### Setup
library(arrow)
library(tidyverse)
library(covidHubUtils)
library(ggridges)


userid <- Sys.info()["user"]

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",userid,"/Desktop/GitHub")


'%!in%' <- Negate('%in%') #previously %notin% possible #update

last.tuesday21 = as.Date("2022-06-21")
last.tuesday23 = as.Date("2023-04-11")

window.width = c(2, 4, 8)

eval.weeks = 8

#updated to reflect inclusion criteria for 21-22 of Feb 21, 2022 to June 20, 2022 and Oct 17, 2022 to May 15, 2023 for 22-23
weeks.to.eval21 = 
  seq(as.Date("2022-02-21"),
      as.Date("2022-06-20"),
      by=7) %>% 
  as.character()

weeks.to.eval23 = 
  seq(as.Date("2022-10-17"),
      as.Date("2023-04-10"),
      by=7) %>% 
  as.character()


manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
flusight_forecast_data <-paste0(githubpath, "/Flusight-forecast-data")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##################### all data to generate figures and tables
all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))

obs_data21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
obs_data23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date))



forecastsandobservedplt <- function(all_dat, obs_data, model, location, a= "a"){
  
  plttitle = case_when(a == "a" ~ "2021-22", a == "b" ~ "2022-23")
  
  pdf(paste0(filedirec,"Retro Plots/",
             "Season_Performance_by_Model", plttitle,".pdf"),  
      width=11, height=7.5, paper='USr')
  
  par(mfrow=c(2, 3),
      mar=c(1.5, 2, 2, 2.5), oma=c(3, 2, 2, 2), 
      mgp=c(0.5, 0.3, 0), tck=-0.01)
  
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


for (i in models){
  
for (k in locations){
  
  forecastsandobservedplt(all_dat21, obs_data21, i, k, a = "a")
  
}  
  
}

for (i in models){
  
  for (k in locations){
    
    forecastsandobservedplt(all_dat23, obs_data23, i, k, a = "b")
    
  }  
  
}
