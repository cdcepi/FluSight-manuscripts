library(arrow)
library(tidyverse)
library(covidHubUtils)
library(ggridges)
library(gridExtra)


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

dashpath <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##################### all data to generate figures and tables
all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))

obs_data21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
obs_data23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

models_21 <- unique(all_dat21$model)
models_23 <- unique(all_dat23$model)

locations_21 <- unique(c(grep("National", all_dat21$location_name, value = T)[1], 
                         sort(unique(all_dat21$location_name))))
locations_23 <- unique(c(grep("National", all_dat23$location_name, value = T)[1], 
                         sort(unique(all_dat23$location_name))))
models <- unique(c(models_21, models_23))
locations <- unique(c(locations_21, locations_23))

models <- c("Flusight-ensemble")
locations <- c("National", "Alabama")



pdf(paste0(dashpath,"/Retro Plots/",
           "Season_Performance_by_Model_", i, ".pdf"),  
    width=11, height=7.5, paper='USr')

par(mfrow=c(2, 3),
    mar=c(1.5, 2, 2, 2.5), oma=c(3, 2, 2, 2), 
    mgp=c(0.5, 0.3, 0), tck=-0.01)

for (i in models) {
  
  
  for(k in locations)
  {
    all_dat21$forecast_date <- as.Date(all_dat21$forecast_date)  
    
    
    plot_data_forecast_22 <- all_dat21 %>% filter(model == i, location_name == k, forecast_date >= (as.Date(min(all_dat21$forecast_date)) + 21)) 
    
    # if(nrow(plot_data_forecast22) == 0) {
    #   print(k)
    #   next
    # }
    
    plot_data_forecast_22 <- plot_data_forecast_22 %>% #target_end_date >= "2022-02-05"
      mutate(temporal_resolution = "wk",
             target_variable = "inc flu hosp",
             horizon = case_when(target == "1 wk ahead inc flu hosp" ~ "1", 
                                 target == "2 wk ahead inc flu hosp" ~ "2",
                                 target == "3 wk ahead inc flu hosp" ~ "3",
                                 target == "4 wk ahead inc flu hosp"~ "4"),
             #TRUE ~ NA),
             full_location_name = location_name, 
             target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
             forecast_date = as.Date(forecast_date, format = "%Y-%m-%d")) %>% 
      filter(forecast_date %in% c(min(forecast_date) + 28*c(0:10)))
    
    obs_22 <- obs_data21 %>% 
      filter(location_name == k) %>% 
      # filter(target_end_date >= "2022-02-02") %>% 
      mutate(model = "Observed Data",
             target_variable = "inc flu hosp",
             location_name = case_when(location_name == "US" ~ "National", 
                                       TRUE ~ location_name), 
             full_location_name = location_name,
             value = value_inc, 
             target_end_date <- as.Date(target_end_date, format = "%Y-%m-%d")) %>% 
      select(target_end_date, location, location_name, value, model, target_variable, full_location_name)
    
    # if(nrow(obs_22) == 0) {
    #   print(k)
    #   next
    # }
    
    
    fig_loc <- ifelse(k == "National", "US", k)
    
    
    if(nrow(obs_22) == 0) {
      print(this_location)
      next
    }
    
    if(nrow(plot_data_forecast_22) == 0) {
      print(this_location)
      next
    }
    
    all_dat23$forecast_date <- as.Date(all_dat23$forecast_date)  
    
    
    plot_data_forecast_23 <- all_dat23 %>% filter(model == i, location_name == k, forecast_date >= (as.Date(min(all_dat23$forecast_date)) + 21)) 
    
    # if(nrow(plot_data_forecast23) == 0) {
    #   print(k)
    #   next
    # }
    
    plot_data_forecast_23 <- plot_data_forecast_23 %>% #target_end_date >= "2022-02-05"
      mutate(temporal_resolution = "wk",
             target_variable = "inc flu hosp",
             horizon = case_when(target == "1 wk ahead inc flu hosp" ~ "1", 
                                 target == "2 wk ahead inc flu hosp" ~ "2",
                                 target == "3 wk ahead inc flu hosp" ~ "3",
                                 target == "4 wk ahead inc flu hosp"~ "4"),
             #TRUE ~ NA),
             full_location_name = location_name, 
             target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
             forecast_date = as.Date(forecast_date, format = "%Y-%m-%d")) %>% 
      filter(forecast_date %in% c(min(forecast_date) + 28*c(0:10)))
    
    obs_23 <- obs_data23 %>% 
      filter(location_name == k) %>% 
      # filter(target_end_date >= "2022-02-02") %>% 
      mutate(model = "Observed Data",
             target_variable = "inc flu hosp",
             location_name = case_when(location_name == "US" ~ "National", 
                                       TRUE ~ location_name), 
             full_location_name = location_name,
             value = value_inc, 
             target_end_date <- as.Date(target_end_date, format = "%Y-%m-%d")) %>% 
      select(target_end_date, location, location_name, value, model, target_variable, full_location_name)
    
    # if(nrow(obs_23) == 0) {
    #   print(k)
    #   next
    # }
    if(nrow(obs_23) == 0) {
      print(this_location)
      next
    }
    
    if(nrow(plot_data_forecast_23) == 0) {
      print(this_location)
      next
    }
    
    plot_forecasts(plot_data_forecast_22,
                   truth_data = obs_22,
                   models = i,
                   truth_source = "HealthData.gov",
                   locations = c(fig_loc),
                   use_median_as_point = T,
                   subtitle = "",
                   title = paste0(k, " 2021-2022")) + theme(text = element_text(size = 15))
    
    plot_forecasts(plot_data_forecast_23,
                   truth_data = obs_23,
                   models = i,
                   truth_source = "HealthData.gov",
                   locations = c(fig_loc),
                   use_median_as_point = T,
                   subtitle = "",
                   title = paste0("2022-2023")) + theme(text = element_text(size = 15))
    
  }
  
  while(!par('page')) plot.new()
}
dev.off()




  
# forecastsandobservedplt_all(all_dat21, obs_data21, mods = models_21, locs = locations_21, a = "a")
# 
# forecastsandobservedplt_all(all_dat23, obs_data23, mods = models_23, locs = locations_23, a = "b")
# 


























library(arrow)
library(tidyverse)
library(covidHubUtils)
library(ggridges)
library(gridExtra)


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

dashpath <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##################### all data to generate figures and tables
all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))

obs_data21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
obs_data23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

all_dat21$Season <- rep("2021-2022", nrow(all_dat21))
all_dat23$Season <- rep("2022-2023", nrow(all_dat23))
all_dat_all <- as.data.frame(rbind(all_dat21, all_dat23))

obs_data21$Season <- rep("2021-2022", nrow(obs_data21))
obs_data23$Season <- rep("2022-2023", nrow(obs_data23))
obs_data_all <- as.data.frame(rbind(obs_data21, obs_data23))

models_21 <- unique(all_dat21$model)
models_23 <- unique(all_dat23$model)

locations_21 <- unique(c(grep("National", all_dat21$location_name, value = T)[1], 
                         sort(unique(all_dat21$location_name))))
locations_23 <- unique(c(grep("National", all_dat23$location_name, value = T)[1], 
                         sort(unique(all_dat23$location_name))))
models <- unique(c(models_21, models_23))
locations <- unique(c(locations_21, locations_23))

models <- c("Flusight-ensemble", "Flusight-baseline")
locations <- c("US", "Alabama", "Alaska", "Arkansas", "Maine")

all_dat_all$forecast_date <- as.Date(all_dat_all$forecast_date)  


plot_data_forecast_22 <- all_dat_all %>% filter(forecast_date >= (as.Date(min(all_dat_all$forecast_date)) + 21)) 

# if(nrow(plot_data_forecast22) == 0) {
#   print(k)
#   next
# }

plot_data_forecast_22 <- plot_data_forecast_22 %>% #target_end_date >= "2022-02-05"
  mutate(temporal_resolution = "wk",
         target_variable = "inc flu hosp",
         horizon = case_when(target == "1 wk ahead inc flu hosp" ~ "1", 
                             target == "2 wk ahead inc flu hosp" ~ "2",
                             target == "3 wk ahead inc flu hosp" ~ "3",
                             target == "4 wk ahead inc flu hosp"~ "4"),
         #TRUE ~ NA),
         full_location_name = location_name, 
         target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
         forecast_date = as.Date(forecast_date, format = "%Y-%m-%d")) %>% 
  filter(forecast_date %in% c(min(forecast_date) + 28*c(0:10)))

obs_22 <- obs_data_all %>% 
  #  filter(location_name %in% locations) %>% 
  # filter(target_end_date >= "2022-02-02") %>% 
  mutate(model = "Observed Data",
         target_variable = "inc flu hosp",
         location_name = case_when(location_name == "US" ~ "National", 
                                   TRUE ~ location_name), 
         full_location_name = location_name,
         value = value_inc, 
         target_end_date <- as.Date(target_end_date, format = "%Y-%m-%d")) %>% 
  select(target_end_date, location, location_name, value, model, target_variable, full_location_name, Season)

for (i in models){
  
  pdf(paste0(dashpath,"/Retro Plots/",
             "Season_Performance_by_Model_", i, ".pdf"),  
      width=11, height=7.5, paper='USr')
  
  j = 1
  
  while (j <= length(locations)){
    
    plot_forecasts(plot_data_forecast_22,
                   truth_data = obs_22,
                   models = i,
                   truth_source = "HealthData.gov",
                   locations = ifelse(length(locations) >= j+3, c(locations[j:j+3]), c(locations[j:length(locations)])),
                   use_median_as_point = T,
                   subtitle = "",
                   title = paste0(i),
                   facet = ~location_name+Season, facet_ncol = 2, facet_nrow = 4, facet_scales = "free") + theme(text = element_text(size = 15))# + ggforce::facet_wrap_paginate(~location_name+Season, ncol = 2, nrow = 3, scales = "free")
    j = j+4
  }
}
dev.off()

#, facet_ncol = 2, facet_scales = "free"
