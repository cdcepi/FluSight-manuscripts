###PLEASE READ###

# This file can be used to generate all relevent data used in the generate_log_transformed_figures_and_tables_2021-2023.R script.
# The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run this script prior to using the generate_log_transformed_figures_and_tables_2021-2023.R script.
# All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. 
# Each subsequent section contains the data manipulation code related to the figure of the same name.


### Setup

library(tidyverse)
library(epiDisplay)
library(MMWRweek)
library(DT)
library(plotly)
library(gridExtra)
library(covidHubUtils)
library(ggridges)
library(viridis)
library(cowplot)
library(scales)
library(RSocrata)
library(arrow)

userid="nqr2"

'%!in%' <- Negate('%in%')

last.tuesday21 = as.Date("2022-06-21")
last.tuesday23 = as.Date("2023-05-16")

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",userid,"/Desktop/GitHub")
manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
flusight_forecast_data <-paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-forecast-data")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##### Generate Scores

all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% 
  mutate(target_end_date = as.Date(target_end_date), forecast_date = as.Date(forecast_date))
all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet"))  %>% 
  mutate(target_end_date = as.Date(target_end_date), forecast_date = as.Date(forecast_date))


##Observed Data
obs_data21 <- read.csv(paste0(manuscript_repo,"/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
obs_data23 <- read.csv(paste0(manuscript_repo,"/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date)) 

##### Inclusion Criteria
include21 <- all_dat21 %>%  filter(type == "quantile", location != "US") %>% 
  mutate(baseline_n_forecasts = sum(!is.na(value[model == "Flusight-baseline"]))) %>% 
  group_by(model) %>% 
  mutate(n_forecasts = sum(!is.na(value)),
         n_locations = length(unique(location))) %>%
  mutate(per_forecasts = round((n_forecasts/(baseline_n_forecasts))*100,2),
         per_locations = round((n_locations/length(unique(all_dat21$location)))*100,2)) %>% 
  filter(per_forecasts >= 75) %>%
  select(model,n_forecasts, per_forecasts, n_locations, per_locations) %>% 
  distinct()

include23 <- all_dat23 %>%  filter(type == "quantile", location != "US") %>% 
  mutate(baseline_n_forecasts = sum(!is.na(value[model == "Flusight-baseline"]))) %>% 
  group_by(model) %>% 
  mutate(n_forecasts = sum(!is.na(value)),
         n_locations = length(unique(location))) %>%
  mutate(per_forecasts = round((n_forecasts/(baseline_n_forecasts))*100,2),
         per_locations = round((n_locations/length(unique(all_dat23$location)))*100,2)) %>% 
  filter(per_forecasts >= 75) %>%
  select(model,n_forecasts, per_forecasts, n_locations, per_locations) %>% 
  distinct()

### Prep for scoring

dat_for_scores21 <- dat_for_scores_function(all_dat21, obs_data21, include21)
dat_for_scores23 <- dat_for_scores_function(all_dat23, obs_data23, include23)

dat_for_scores21ln <- dat_for_scores21 %>% mutate(prediction = log1p(prediction), true_value = log1p(true_value))
dat_for_scores23ln <- dat_for_scores23 %>% mutate(prediction = log1p(prediction), true_value = log1p(true_value))



##### WIS Calculations

raw_scores21ln <- dat_for_scores21ln %>% scoringutils::score()
raw_scores23ln <- dat_for_scores23ln %>% scoringutils::score()

wis_season_by_model_21ln <- raw_scores21ln %>%
  filter(location != "US", model %in% include21$model) %>% 
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  scoringutils::summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline", na.rm  = TRUE)%>%
  mutate(wis=round(interval_score,2),
    mae=round(ae_median,2),
    rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae) 

inc.rankings_all21ln <- include21 %>%
  left_join(wis_season_by_model_21ln, by="model")

#23
wis_season_by_model_23ln <- raw_scores23ln %>%
  filter(location != "US", model %in% include23$model) %>% 
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline", na.rm  = TRUE)%>%
  mutate(wis=round(interval_score,2),
    mae=round(ae_median,2),
    rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae) 

inc.rankings_all23ln <- include23 %>%
  left_join(wis_season_by_model_23ln, by="model")

#coverage in scoringutils isn't exactly what we've been doing so approximating our coverage with data from scoringutils functions 
WIS_all21ln <- raw_scores21ln %>% summarise_scores(na.rm = TRUE) %>% mutate(wis=round(interval_score,2))
dat_21ln <- dat_for_scores21ln %>% filter(location != "US") %>% pivot_wider(names_from = c(type, quantile), values_from=c(prediction, true_value)) %>% select(target, model, forecast_date, location, date, prediction_quantile_0.025, prediction_quantile_0.25, prediction_quantile_0.5, prediction_quantile_0.75, prediction_quantile_0.975, true_value_quantile_0.5)

WIS_all21ln <- merge(WIS_all21ln, dat_21ln, by = c("target", "model", "forecast_date", "location", "date")) %>% rename("report" = "true_value_quantile_0.5") %>% mutate(coverage.50 = ifelse(report >= prediction_quantile_0.25 & report <= prediction_quantile_0.75,T,F),  coverage.95 = ifelse(report >= prediction_quantile_0.025 & report <= prediction_quantile_0.975,T,F))

WIS_all23ln <- raw_scores23ln %>% summarise_scores(na.rm = TRUE) %>% mutate(wis=round(interval_score,2))
dat_23ln <- dat_for_scores23ln %>% filter(location != "US") %>% pivot_wider(names_from = c(type, quantile), values_from=c(prediction, true_value)) %>% select(target, model, forecast_date, location, date, prediction_quantile_0.025, prediction_quantile_0.25, prediction_quantile_0.5, prediction_quantile_0.75, prediction_quantile_0.975, true_value_quantile_0.5)

WIS_all23ln <- merge(WIS_all23ln, dat_23ln, by = c("target", "model", "forecast_date", "location", "date")) %>% rename("report" = "true_value_quantile_0.5") %>% mutate(coverage.50 = ifelse(report >= prediction_quantile_0.25 & report <= prediction_quantile_0.75,T,F),  coverage.95 = ifelse(report >= prediction_quantile_0.025 & report <= prediction_quantile_0.975,T,F))

# pull out data on forecasts 
WIS_alllocations21ln <- WIS_all21ln
WIS_alllocations23ln <- WIS_all23ln

WIS_all21ln = filter(WIS_all21ln, location_name != "National")
WIS_all23ln = filter(WIS_all23ln, location_name != "National")

WIS_Season21ln <- filter(WIS_all21ln, as.Date(forecast_date) >= as.Date("2022-02-19"), as.Date(forecast_date) <= as.Date(last.tuesday21+4)) %>% {unique(.)}
WIS_Season23ln <- filter(WIS_all23ln, as.Date(forecast_date) >= as.Date("2022-10-17"), as.Date(forecast_date) <= as.Date(last.tuesday23+4)) %>% {unique(.)}

and_coverage_21 <- WIS_Season21ln %>% group_by(model) %>% summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% distinct() %>% ungroup()

inc.rankings_all21ln <- inc.rankings_all21ln %>%  left_join(and_coverage_21, by = "model")

and_coverage_23 <- WIS_Season23ln %>% group_by(model) %>% summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% distinct() %>% ungroup()
inc.rankings_all23ln <- left_join(inc.rankings_all23ln, and_coverage_23, by = "model")

##### Season Rankings: Table S3

# inc.rankings_all21ln <- inc.rankings_all_func(WIS_Season21ln)
# inc.rankings_all23ln <- inc.rankings_all_func(WIS_Season23ln)

inc.rankings_all <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% arrange(season, rel_wis)

write.csv(inc.rankings_all, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc.rankings_all.csv"), row.names = FALSE)

### necessary analytics for below output
WIS_and_coverage_21ln <- WIS_Season21ln %>% group_by(model, location_name) %>% summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% distinct() %>% ungroup()

inc.rankings_location21ln <- raw_scores21ln %>% 
  filter(location != "US") %>% 
  summarise_scores(by = c("model", "location_name"), relative_skill = TRUE,  baseline = "Flusight-baseline", na.rm = TRUE) %>% 
  mutate(wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, location_name, wis,rel_wis, mae) %>% # mutate(below = ifelse(rel_wis < 1, 1, 0)) %>% 
  group_by(location_name) %>%
  mutate(baseline_wis = wis[model == "Flusight-baseline"]) %>%
  ungroup() %>%
  mutate(below = ifelse(wis < baseline_wis & model != "Flusight-baseline", 1, 0)) %>% 
  select(-baseline_wis) %>% 
  left_join(WIS_and_coverage_21ln, by = join_by("model" == "model", "location_name" == "location_name"))

WIS_and_coverage_23ln <- WIS_Season23ln %>% 
  group_by(model, location_name) %>% 
  summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% 
  distinct() %>% 
  ungroup()

inc.rankings_location23ln <- raw_scores23ln %>% 
  filter(location != "US") %>% 
  summarise_scores(by = c("model", "location_name"), relative_skill = TRUE,  baseline = "Flusight-baseline", na.rm = TRUE) %>% 
  mutate(wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, location_name, wis,rel_wis, mae) %>% # mutate(below = ifelse(rel_wis < 1, 1, 0)) %>% 
  group_by(location_name) %>%
  mutate(baseline_wis = wis[model == "Flusight-baseline"]) %>%
  ungroup() %>%
  mutate(below = ifelse(wis < baseline_wis & model != "Flusight-baseline", 1, 0)) %>% select(-baseline_wis) %>% 
  left_join(WIS_and_coverage_23ln, by = join_by("model" == "model", "location_name" == "location_name"))

inc.rankings_location <- inc.rankings_location21ln %>% 
  mutate(season = "2021-2022") %>% 
  rbind(mutate(inc.rankings_location23ln, season = "2022-2023"))

##### Absolute WIS by Week table: Table S4
Scores_tab21ln <- scores_tab_function(inc.rankings_location21ln,inc.rankings_all21ln, WIS_Season21ln)
Scores_tab23ln <- scores_tab_function(inc.rankings_location23ln, inc.rankings_all23ln, WIS_Season23ln)

cov95_breakdown21ln <- cov95_function(WIS_Season21ln, Scores_tab21ln)

cov95_breakdown23ln <- cov95_function(WIS_Season23ln, Scores_tab23ln)

cov95_breakdownall <- rbind(mutate(cov95_breakdown21ln, season = "2021-2022"), mutate(cov95_breakdown23ln, season = "2022-2023"))

write.csv(cov95_breakdownall, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/cov95_breakdownall.csv"), row.names = FALSE)


##### Model rank plot: Figure S5

WIS_Season21ln$season <- "2021-2022"
WIS_Season23ln$season <- "2022-2023"

WIS_Season <- rbind(WIS_Season21ln, WIS_Season23ln) %>% rename("target_end_date" = "date", "WIS" = "wis")

write.csv(WIS_Season, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/WIS_Season.csv"), row.names = FALSE)


inc_scores_overall <- WIS_Season %>%
  # filter(include_overall == "TRUE") %>%
  group_by(target_end_date, target, location_name, season) %>%
  mutate(n_models = n()) %>%
  ##filter(n_models >= 15) %>%
  arrange(WIS) %>%
  mutate(model_rank = row_number(), rank_percentile = model_rank/n_models) %>%
  arrange(-WIS) %>%
  mutate(rev_rank = (row_number()-1)/(n_models-1)) %>%
  ungroup() %>%
  mutate(model = reorder(model, rev_rank, FUN=function(x) quantile(x, probs=0.25, na.rm=TRUE)))

write.csv(inc_scores_overall, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc_scores_overall.csv"), row.names = FALSE)

##### Absolute WIS by model: Figure S6

WIS_Season21ln$season <- "2021-2022"
WIS_Season23ln$season <- "2022-2023"

WIS_Season <- rbind(WIS_Season21ln, WIS_Season23ln) %>% rename("target_end_date" = "date", "WIS" = "wis")

abs_states <- WIS_Season %>% filter(location_name != "National") %>% 
  filter(target == "1 wk ahead inc flu hosp" | target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(model = model,
            target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
            abs_WIS = mean(WIS)) %>% 
  #mutate(model_color = ifelse(model == "Flusight-baseline", "#ff0903", ifelse(model == "Flusight-ensemble", "#b993ff", "#abbfcb"))) %>% 
  ungroup() %>% mutate(log_WIS = log10(abs_WIS))

write.csv(abs_states, paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/abs_states.csv"),row.names = FALSE)

##### Relative WIS by Location: Figure S7
inc.rankings_all_nice <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% group_by(season) %>% arrange(season, rel_wis) %>% mutate(modelorder = paste(model, season))

scores <- inc.rankings_location %>% filter(is.finite(rel_wis)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))

write.csv(scores, paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/scores.csv"),row.names = FALSE)


