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

userid="rpe5"

'%!in%' <- Negate('%in%')

last.tuesday21 = as.Date("2022-06-21")
last.tuesday23 = as.Date("2023-05-16")

window.width = c(2, 4, 8)

eval.weeks = 8

#possibly update
weeks.to.eval21 = 
  seq(as.Date("2022-02-21"),
      as.Date("2022-06-20"),
      by=7) %>% 
  as.character()

weeks.to.eval23 = 
  seq(as.Date("2022-10-17"),
      as.Date("2023-05-15"),
      by=7) %>% 
  as.character()

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",userid,"/Desktop/GitHub")
manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
flusight_forecast_data <-paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-forecast-data")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##### Generate Scores

all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet"))
all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet"))


##Observed Data
obs_data21 <- read_csv(paste0(flusight_forecast_data,"/data-truth/truth-Incident Hospitalizations-Archived_9-12-2022.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(all_dat21$target_end_date)), location != 78 )


obs_data21 <- obs_data21 %>%
  rename(value_inc = value,
         target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())


location.names21 = obs_data21 %>% select(location, location_name) %>% unique()



obs_data23 <- read_csv(paste0(flusight_forecast_data,"/data-truth/truth-Incident Hospitalizations-2023-06-23.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(all_dat23$target_end_date) ), location != 78)


obs_data23 <- obs_data23 %>%
  dplyr::rename(value_inc = value,
                target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())


location.names23 = obs_data23 %>% select(location, location_name) %>% unique()



dat_for_scores21 <- dat_for_scores_function(all_dat21, obs_data21)
dat_for_scores23 <- dat_for_scores_function(all_dat23, obs_data23)

dat_for_scores21ln <- dat_for_scores21 %>% mutate(value = log(value+1), report =log(report+1))
dat_for_scores23ln <- dat_for_scores23 %>% mutate(value = log(value+1), report = log(report+1))

##### WIS Calculations

WIS_all21ln <- wis_all_function(dat_for_scores21ln)
WIS_all23ln <- wis_all_function(dat_for_scores23ln)


# pull out data on forecasts 
WIS_alllocations21ln <- WIS_all21ln
WIS_alllocations23ln <- WIS_all23ln

WIS_all21ln = filter(WIS_all21ln, location_name != "National")
WIS_all23ln = filter(WIS_all23ln, location_name != "National")

WIS_Season21ln <- filter(WIS_all21ln, as.Date(forecast_date) >= as.Date("2022-02-19"), as.Date(forecast_date) <= as.Date(last.tuesday21+4)) %>% {unique(.)}
WIS_Season23ln <- filter(WIS_all23ln, as.Date(forecast_date) >= as.Date("2022-10-17"), as.Date(forecast_date) <= as.Date(last.tuesday23+4)) %>% {unique(.)}

##### Season Rankings: Table S3


inc.rankings_all21ln <- inc.rankings_all_func(WIS_Season21ln)
inc.rankings_all23ln <- inc.rankings_all_func(WIS_Season23ln)

inc.rankings_all <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% arrange(season, rel.WIS.skill)


WIS_Season21ln <- WIS_Season21ln %>% filter(model %in% inc.rankings_all21ln$model)
inc.rankings_location21ln <- make_WIS_ranking_location(WIS_Season21ln)
inc.rankings_location21ln$below <- ifelse(inc.rankings_location21ln$relative_WIS < 1, 1, 0)

WIS_Season23ln <- WIS_Season23ln %>% filter(model %in% inc.rankings_all23ln$model)
inc.rankings_location23ln <- make_WIS_ranking_location(WIS_Season23ln)
inc.rankings_location23ln$below <- ifelse(inc.rankings_location23ln$relative_WIS < 1, 1, 0)

inc.rankings_location <- inc.rankings_location21ln %>% mutate(season = "2021-2022") %>% rbind(mutate(inc.rankings_location23ln, season = "2022-2023"))

national21ln <- WIS_alllocations21ln %>% filter(location_name == "National", model != "Flusight-ensemble", model != "Flusight-baseline")
national21ln <- national21ln %>% group_by(forecast_date) %>% summarise(n = length(unique(model)))

national23ln <- WIS_alllocations23ln %>% filter(location_name == "National", model != "Flusight-ensemble", model != "Flusight-baseline")
national23ln <- national23ln %>% group_by(forecast_date) %>% summarise(n = length(unique(model)))

##### Absolute WIS by Week table: Table S4

model_abs <- abs_states %>% unique() %>% group_by(model, target, season) %>% summarise(
  model = model,
  target = target,
  minimum = min(abs_WIS),
  maximum = max(abs_WIS),
  Median = median(abs_WIS)
) %>% ungroup() %>% unique()

abs_states_all <- WIS_Season %>% filter(location_name != "National") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(model = model,
            target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
            abs_WIS = mean(WIS)) %>% 
  mutate(model_color = ifelse(model == "Flusight-baseline", "red", ifelse(model == "Flusight-ensemble", "green", "gray")))


model_abs_all <- abs_states_all %>% unique() %>% group_by(model, target, season) %>% summarise(
  model = model,
  target = target,
  minimum = min(abs_WIS),
  maximum = max(abs_WIS),
  Median = median(abs_WIS)
) %>% unique()

abs_breakdown <- WIS_Season %>% group_by(model, season) %>% summarise(
  model = model,
  One_week_abs = mean(WIS[target == "1 wk ahead inc flu hosp"]),
  Two_week_abs = mean(WIS[target == "2 wk ahead inc flu hosp"]),
  Three_week_abs = mean(WIS[target == "3 wk ahead inc flu hosp"]),
  Four_week_abs = mean(WIS[target == "4 wk ahead inc flu hosp"])
) %>% unique() 


abs_breakdown_WIS <- merge(inc.rankings_all[,c(1,3,11)], abs_breakdown, by= c("model", "season")) %>% arrange(season, rel.WIS.skill)




fs_wis_min_date21ln <- abs_states %>% filter(season == "2021-2022", model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max_date21ln <- abs_states %>% filter(season == "2021-2022",model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min4_date21ln <- abs_states %>% filter(season == "2021-2022", model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max4_date21ln <- abs_states %>% filter(season == "2021-2022",model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min_date23ln <- abs_states %>% filter(season == "2022-2023", model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max_date23ln <- abs_states %>% filter(season == "2022-2023",model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min4_date23ln <- abs_states %>% filter(season == "2022-2023", model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max4_date23ln <- abs_states %>% filter(season == "2022-2023",model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

##### Model rank plot: Figure S5


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

n_unique_predict<- inc_scores_overall %>% filter(season == "2022-2023") %>% 
  group_by(target_end_date, location_name, target, season) %>%
  summarize(n()) %>%
  nrow()


## average rank
average_rank_percent <- inc_scores_overall %>%
  group_by(model, season) %>%
  summarize(average_rank = mean(rev_rank), total_n = n(),
            n_top50 = sum(rev_rank> 0.5) , pct_top50 = n_top50/total_n*100,
            n_top25 = sum(rev_rank> 0.75) , pct_top25 = n_top25/total_n*100,
            n_bottom50 = sum(rev_rank< 0.5) , pct_bottom50 = n_bottom50/total_n*100,
            n_bottom25 = sum(rev_rank< 0.25) , pct_bottom25 = n_bottom25/total_n*100) %>%
  #print(n=Inf) %>% 
  arrange(-pct_top50)

ave_rank_percent21ln <- average_rank_percent %>% filter(season == "2021-2022") %>% ungroup()
ave_rank_percent23ln <- average_rank_percent %>% filter(season == "2022-2023") %>% ungroup()

bimodal_rank21 <- ave_rank_percent21ln %>% filter(pct_top25 < 50, pct_bottom25 < 50) %>%  mutate(pct_top_bottom = pct_top25 + pct_bottom25) %>% filter(pct_top_bottom >50)
bimodal_rank23 <- ave_rank_percent23ln%>% filter(pct_top25 < 50, pct_bottom25 < 50)  %>% mutate(pct_top_bottom = pct_top25 + pct_bottom25) %>% filter(pct_top_bottom >50)


##### Absolute WIS by model: Figure S6


WIS_Season21ln$season <- "2021-2022"
WIS_Season23ln$season <- "2022-2023"

WIS_Season <- rbind(WIS_Season21ln, WIS_Season23ln)

abs_states <- WIS_Season %>% filter(location_name != "National") %>% 
  filter(target == "1 wk ahead inc flu hosp" | target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(model = model,
            target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
            abs_WIS = mean(WIS)) %>% 
  #mutate(model_color = ifelse(model == "Flusight-baseline", "#ff0903", ifelse(model == "Flusight-ensemble", "#b993ff", "#abbfcb"))) %>% 
  ungroup() %>% mutate(log_WIS = log10(abs_WIS))

abs_flusight <- abs_states %>% filter(model %in% c("Flusight-baseline", "Flusight-ensemble"))
abs_not_flusight <- abs_states %>% filter(model %!in% c("Flusight-baseline", "Flusight-ensemble"))

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"))


##### Relative WIS by Location: Figure S7

inc.rankings_all_nice <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% group_by(season) %>% arrange(season, rel.WIS.skill) %>% mutate(modelorder = paste(model, season))

scores <- inc.rankings_location %>% filter(is.finite(relative_WIS)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))


modelrankings21ln <- inc.rankings_location21ln %>%  ungroup()%>% group_by(model) %>% summarise(low = min(relative_WIS), high = max(relative_WIS), median = median(relative_WIS), mean = mean(relative_WIS)) %>% mutate(diff = high - low)

modelrankings23ln <- inc.rankings_location23ln %>%  ungroup()%>% group_by(model) %>% summarise(low = min(relative_WIS), high = max(relative_WIS), median = median(relative_WIS), mean = mean(relative_WIS)) %>% mutate(diff = high - low)

#arrange(filter(Scores_tab21ln,  model != "Flusight-ensemble", below_baseline_pct > 50), desc(below_baseline_pct))
scores_tab_nice21ln <- Scores_tab21ln %>% mutate(across(where(is.numeric),~round(.x, 2)))

#arrange(filter(Scores_tab23ln,  model != "Flusight-ensemble", below_baseline_pct > 50), desc(below_baseline_pct))
scores_tab_nice23ln <- arrange(filter(Scores_tab23ln,  model != "Flusight-ensemble", below_baseline_pct > 50), desc(below_baseline_pct)) %>% mutate(across(where(is.numeric),~round(.x, 2)))

##### WIS avg by week

plot.scores <- WIS_Season %>% filter(target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, season) %>% 
  summarise(model = model,
            target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
            Avg_WIS = mean(WIS)) %>%
  unique()

AVG <- plot.scores %>% group_by(target_end_date, season) %>% 
  summarise(target_end_date = as.Date(target_end_date, format = "%Y-%m-%d"),
            Avg_WIS = mean(Avg_WIS)) %>% 
  mutate(model = rep("Average Score of All Models"), .before = target_end_date) %>% 
  unique()     

plot.scores <- as.data.frame(rbind(plot.scores, AVG)) %>% 
  filter(model %in% c("Average Score of All Models", "Flusight-baseline", "Flusight-ensemble"))

