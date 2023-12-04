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

dat_for_scores21 <- dat_for_scores_function(all_dat21, obs_data21)
dat_for_scores23 <- dat_for_scores_function(all_dat23, obs_data23)

dat_for_scores21ln <- dat_for_scores21 %>% mutate(value = log1p(value), report = log1p(report))
dat_for_scores23ln <- dat_for_scores23 %>% mutate(value = log1p(value), report = log1p(report))

##### WIS Calculations

WIS_all21ln <- wis_all_function(dat_for_scores21ln)
WIS_all23ln <- wis_all_function(dat_for_scores23ln)

WIS_all21ln = filter(WIS_all21ln, location_name != "National")
WIS_all23ln = filter(WIS_all23ln, location_name != "National")

WIS_Season21ln <- filter(WIS_all21ln, as.Date(forecast_date) >= as.Date("2022-02-19"), as.Date(forecast_date) <= as.Date(last.tuesday21+4)) %>% {unique(.)}
WIS_Season23ln <- filter(WIS_all23ln, as.Date(forecast_date) >= as.Date("2022-10-17"), as.Date(forecast_date) <= as.Date(last.tuesday23+4)) %>% {unique(.)}

##### Season Rankings: Table S3

inc.rankings_all21ln <- inc.rankings_all_func(WIS_Season21ln)
inc.rankings_all23ln <- inc.rankings_all_func(WIS_Season23ln)

inc.rankings_all <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% arrange(season, rel.WIS.skill)

# write.csv(inc.rankings_all, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc.rankings_all.csv"), row.names = FALSE)

### necessary analytics for below output
WIS_Season21ln <- WIS_Season21ln %>% filter(model %in% inc.rankings_all21ln$model)
inc.rankings_location21ln <- make_WIS_ranking_location(WIS_Season21ln)
inc.rankings_location21ln$below <- ifelse(inc.rankings_location21ln$relative_WIS < 1, 1, 0)

WIS_Season23ln <- WIS_Season23ln %>% filter(model %in% inc.rankings_all23ln$model)
inc.rankings_location23ln <- make_WIS_ranking_location(WIS_Season23ln)
inc.rankings_location23ln$below <- ifelse(inc.rankings_location23ln$relative_WIS < 1, 1, 0)

inc.rankings_location <- inc.rankings_location21ln %>% mutate(season = "2021-2022") %>% rbind(mutate(inc.rankings_location23ln, season = "2022-2023"))

##### Absolute WIS by Week table: Table S4
Scores_tab21ln <- scores_tab_function(inc.rankings_location21ln,inc.rankings_all21ln, WIS_Season21ln)
Scores_tab23ln <- scores_tab_function(inc.rankings_location23ln, inc.rankings_all23ln, WIS_Season23ln)

cov95_breakdown21ln <- cov95_function(WIS_Season21ln, Scores_tab21ln)

cov95_breakdown23ln <- cov95_function(WIS_Season23ln, Scores_tab23ln)

cov95_breakdownall <- rbind(mutate(cov95_breakdown21ln, season = "2021-2022"), mutate(cov95_breakdown23ln, season = "2022-2023"))

# write.csv(cov95_breakdownall, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/cov95_breakdownall.csv"), row.names = FALSE)


##### Model rank plot: Figure S5

WIS_Season21ln$season <- "2021-2022"
WIS_Season23ln$season <- "2022-2023"

WIS_Season <- rbind(WIS_Season21ln, WIS_Season23ln)

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

# write.csv(inc_scores_overall, paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc_scores_overall.csv"), row.names = FALSE)

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

write.csv(abs_states, paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/abs_states.csv"),row.names = FALSE)

##### Relative WIS by Location: Figure S7
inc.rankings_all_nice <- rbind(mutate(inc.rankings_all21ln, season = "2021-2022"), mutate(inc.rankings_all23ln, season = "2022-2023")) %>% group_by(season) %>% arrange(season, rel.WIS.skill) %>% mutate(modelorder = paste(model, season))

scores <- inc.rankings_location %>% filter(is.finite(relative_WIS)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))

# write.csv(scores, paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/scores.csv"),row.names = FALSE)


