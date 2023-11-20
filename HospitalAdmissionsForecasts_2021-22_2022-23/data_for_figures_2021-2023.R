######### Setup

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

userid="rpe5"

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


dashboard_r_code <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code")
flusight_forecast_data <-paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-forecast-data")
dashboard_r_code_weekly_data <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code/Weekly Data/")

suppressMessages(invisible(source(paste0(dashboard_r_code_weekly_data,"Model names and colors.R"))))
source(paste0(dashboard_r_code,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

################################# Season Data

datapath <- paste0(flusight_forecast_data,"/data-forecasts")
filenames = list.files(path=datapath, pattern=".csv", 
                       full.names = TRUE, recursive = TRUE)

unique_model_names <- unique(str_split(filenames, "/", simplify = TRUE)[, 8:9])
unique_model_names = cbind(unique_model_names, substr(unique_model_names[,2],1,10) ) 

unique_model_names = data.frame(model = as.character(unique_model_names[,1]),
                                filename = as.character(unique_model_names[,2]), 
                                date.submitted = unique_model_names[,3],
                                next.tuesday = get_next_tuesday(as.Date(unique_model_names[,3]))) %>% 
  group_by(model, next.tuesday) %>% 
  summarise(filename = as.character(filename[which.max(as.Date(date.submitted))])) %>% 
  ungroup() %>% 
  mutate(model = as.character(model),
         next.tuesday = as.character(next.tuesday))

dat21 <- unique_model_names %>% 
  filter(next.tuesday %in% 
           as.character(
             last.tuesday21 - 7*(0:((eval.weeks-1)+max(window.width)+(4-1)-1))
           )
  ) %>%
  filter(next.tuesday == as.character(last.tuesday21))

dat23  <- unique_model_names %>% 
  filter(next.tuesday %in% 
           as.character(
             last.tuesday23 - 7*(0:((eval.weeks-1)+max(window.width)+(4-1)-1))
           )
  ) %>%
  filter(next.tuesday == as.character(last.tuesday23))


filenames = 
  paste0(flusight_forecast_data,"/data-forecasts/",
         unique_model_names$model, "/",
         unique_model_names$filename)


dat_list <- lapply(filenames,
                   FUN = function(x){
                     read_csv(x, col_types = cols(.default = "c")) %>%
                       filter(grepl("hosp", target),
                              target %in% 
                                paste(1:4,"wk ahead inc flu hosp"))
                   }
)


models = unique_model_names$model
unique_models = unique(models)

all_dat_new <- tibble()
for (i in c(1:length(dat_list))) {
  all_dat_new <- bind_rows(all_dat_new,
                           dat_list[[i]] %>%
                             select(location, target, target_end_date, forecast_date,
                                    type, quantile, value) %>%
                             mutate(
                               location = as.character(location),
                               location = 
                                 as.character(ifelse(location %in% as.character(1:9),
                                                     paste0("0",location),location)),
                               model = models[i],
                               value = as.numeric(value),
                               quantile = as.numeric(quantile)  ,
                               quantile=ifelse(quantile=="NaN", NA, quantile)))
}


all_dat21 = all_dat_new %>% filter(forecast_date %in% weeks.to.eval21, location != 78) %>% {unique(.)}


all_dat23 <- all_dat_new %>% filter(forecast_date %in% weeks.to.eval23, location != 78) %>% {unique(.)}



##Observed Data
obs_data21 <- read_csv(paste0(dashboard_r_code,"/truth-Incident Hospitalizations-Archived_9-12-2022.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(all_dat21$target_end_date)), location != 78 )


obs_data21 <- obs_data21 %>%
  rename(value_inc = value,
         target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())


location.names21 = obs_data21 %>% select(location, location_name) %>% unique()


all_dat21 = left_join(all_dat21,location.names21, by = c("location"))


obs_data23 <- read_csv(paste0(flusight_forecast_data,"/data-truth/truth-Incident Hospitalizations.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(all_dat23$target_end_date) ), location != 78)


obs_data23 <- obs_data23 %>%
  dplyr::rename(value_inc = value,
                target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())


location.names23 = obs_data23 %>% select(location, location_name) %>% unique()


all_dat23 = left_join(all_dat23,location.names23, by = c("location"))


dat_for_scores21 <- dat_for_scores_function(all_dat21, obs_data21)
dat_for_scores23 <- dat_for_scores_function(all_dat23, obs_data23)

################### WIS Calculations

WIS_all21 <- wis_all_function(dat_for_scores21)
WIS_all23 <- wis_all_function(dat_for_scores23)


# pull out data on forecasts 
WIS_alllocations21 <- WIS_all21
WIS_alllocations23 <- WIS_all23

WIS_all21 = filter(WIS_all21, location_name != "National")
WIS_all23 = filter(WIS_all23, location_name != "National")

WIS_Season21 <- filter(WIS_all21, as.Date(forecast_date) >= as.Date("2022-02-19"), as.Date(forecast_date) <= as.Date(last.tuesday21+4)) %>% {unique(.)}
WIS_Season23 <- filter(WIS_all23, as.Date(forecast_date) >= as.Date("2022-10-17"), as.Date(forecast_date) <= as.Date(last.tuesday23+4)) %>% {unique(.)}

############# Inc Rankings

inc.rankings_all21 <- inc.rankings_all_func(WIS_Season21)
inc.rankings_all23 <- inc.rankings_all_func(WIS_Season23)

# write.csv(inc.rankings_all21, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc.rankings_all21.csv"))
# write.csv(inc.rankings_all23, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc.rankings_all23.csv"))
# write.csv(inc.rankings_location21, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc.rankings_location21.csv"))
# write.csv(inc.rankings_location23, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc.rankings_location23.csv"))

############### Absolute WIS by Model

abs_states <- WIS_Season %>% filter(location_name != "National") %>% 
  filter(target == "1 wk ahead inc flu hosp" | target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(abs_WIS = mean(WIS)) %>% 
  #mutate(model_color = ifelse(model == "Flusight-baseline", "#ff0903", ifelse(model == "Flusight-ensemble", "#b993ff", "#abbfcb"))) %>% 
  ungroup() %>% mutate(log_WIS = log10(abs_WIS)) #%>% pivot_longer(cols = c(abs_WIS, log_WIS), names_to = "transform", values_to = "abs_WIS")

abs_flusight <- abs_states %>% filter(model %in% c("Flusight-baseline", "Flusight-ensemble"))
abs_not_flusight <- abs_states %>% filter(model %!in% c("Flusight-baseline", "Flusight-ensemble"))

# write.csv(abs_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/abs_flusight.csv"))
# write.csv(abs_not_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/abs_not_flusight.csv"))

########### 95% Coverage by Model

coverage95_states <- WIS_Season %>% filter(location_name != "National") %>% 
  filter(target == "1 wk ahead inc flu hosp"| target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(coverage95 = mean(coverage.95)) %>% 
  mutate(model_color = ifelse(model == "Flusight-baseline", "#d6936b", ifelse(model == "Flusight-ensemble", "#6baed6", "#abbfcb"))) %>% ungroup()

coverage95_flusight <- coverage95_states %>% filter(model %in% c("Flusight-baseline", "Flusight-ensemble"))
coverage95_not_flusight <- coverage95_states %>% filter(model %!in% c("Flusight-baseline", "Flusight-ensemble"))

# write.csv(coverage95_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/coverage95_flusight.csv"))
# write.csv(coverage95_not_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/coverage95_not_flusight.csv"))

########### 50% Coverage by Model

coverage50_states <- WIS_Season %>% filter(location_name != "National") %>% 
  filter(target == "1 wk ahead inc flu hosp" | target == "4 wk ahead inc flu hosp") %>% 
  group_by(model, target_end_date, target, season) %>% 
  summarise(coverage50 = mean(coverage.50),
            coverage95 = mean(coverage.95)) %>% 
  mutate(model_color = ifelse(model == "Flusight-baseline", "red", ifelse(model == "Flusight-ensemble", "green", "gray"))) %>% ungroup()# %>% 
#pivot_longer(cols = c(coverage50, coverage95), names_to = "percent", values_to = "coverage")

coverage50_flusight <- coverage50_states %>% filter(model %in% c("Flusight-baseline", "Flusight-ensemble"))
coverage50_not_flusight <- coverage50_states %>% filter(model %!in% c("Flusight-baseline", "Flusight-ensemble"))

# write.csv(coverage50_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/coverage50_flusight.csv"))
# write.csv(coverage50_not_flusight, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/coverage50_not_flusight.csv"))

############ Absolute WIS by Week

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

#write.csv(abs_breakdown_WIS, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/abs_breakdown_WIS.csv"))

########## Model Ranks

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

#write.csv(inc_scores_overall, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc_scores_overall.csv"))

##### Relative WIS by Location

inc.rankings_all_nice <- rbind(mutate(inc.rankings_all21, season = "2021-2022"), mutate(inc.rankings_all23, season = "2022-2023")) %>% group_by(season) %>% arrange(season, rel.WIS.skill) %>% mutate(modelorder = paste(model, season))

#write.csv(inc.rankings_all_nice, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/inc.rankings_all_nice.csv"))

###### Backfill Epi curve

tues_start <- as.Date("2023-01-17")
tues_end <- as.Date("2023-06-13")

tuesdates <- seq(from = tues_start, to = tues_end, by = 7)

jsonpath <- read.socrata(url = "https://healthdata.gov/resource/qqte-vkut.json")

tues_set <- data.frame(state = character(), date = as.Date(character()), value = integer(), report_date = character(), epiweek = character(), epiyear = character())
d <- tues_start

#loop to pull in covidcast api data and aggregate it weekly
while (d <= last(tuesdates)){
  d = as.Date(d, origin = "1970-01-01")
  filename = case_when(length(jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12]) > 1 ~
                         jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][2],
                       TRUE ~  jsonpath[str_detect(string = jsonpath$update_date, pattern = as.character(d)), 12][1])
  
  #print(d)
  df = read.csv(filename) %>% mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) -1)
  df1 = df %>% filter(as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")) > as.Date(d-10), date < as.Date(d - 2), state != "AS", state != "VI") %>%
    select(state, date, previous_day_admission_influenza_confirmed) %>%
    mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d")), report_date = (as.Date((get_next_saturday(d) - 7), "%Y-%m-%d")), epiweek = epiweek(date), epiyear = epiyear(date), .keep = "all") %>%
    rename(value = previous_day_admission_influenza_confirmed) %>% select(state, date, value, report_date, epiweek, epiyear)
  tues_set = rbind(tues_set, df1)
  d <- d + 7
}


finalset21 <- backfilldata_func(startdate = "2022-01-31", enddate = "2022-07-18")
forecastdata21 <- nation_summary_func(finalset21)
truthtransmute21 <-  obs_data21  %>% transmute(location_name = case_when(location_name == "National" ~ "US", TRUE ~ location_name),
                                               wk_end_date = as.Date(target_end_date), value = as.numeric(as.character(value_inc)),
                                               report_date  = as.Date("2022-09-12")) %>%
  filter(wk_end_date >= "2022-01-01")

truthaddon21 <- truthtransmute21 %>% filter(location_name == "US") %>% 
  rename(truthdate = report_date, date = wk_end_date, truthvalue = value) %>% 
  group_by(truthdate, date) %>% ungroup()


fullset21 <- forecastdata21 %>% filter(state == "US") %>% left_join(., truthaddon21, by = c("state" = "location_name", "report_date" = "date" ))

finalset23 <- backfilldata_func("2022-10-17","2023-01-09")
finalset23 <- rbind(finalset23, tues_set)
forecastdata23 <- nation_summary_func(finalset23)
truthtransmute23 <-  obs_data23  %>% transmute(location_name = case_when(location_name == "National" ~ "US", TRUE ~ location_name),
                                               wk_end_date = as.Date(target_end_date), value = as.numeric(as.character(value_inc)),
                                               report_date  = as.Date("2023-05-20")) %>%
  filter(wk_end_date >= "2022-10-22")

truthaddon23 <- truthtransmute23 %>% filter(location_name == "US") %>% 
  rename(truthdate = report_date, date = wk_end_date, truthvalue = value) %>% 
  group_by(truthdate, date) %>% ungroup()


fullset23 <- forecastdata23 %>% filter(state == "US") %>% left_join(., truthaddon23, by = c("state" = "location_name", "report_date" = "date" ))

fullset <- rbind(mutate(fullset21, season = "A) 2021-2022"), mutate(fullset23, season = "B) 2022-2023"))

#write.csv(fullset, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/fullset.csv"))

##### Backfill Differences

diffdf21 <- diff_df_function(truthtransmute21, forecastdata21) %>% mutate(season = "A) 2021-2022")
diffdf23 <- diff_df_function(truthtransmute23, forecastdata23) %>% mutate(season = "B) 2022-2023")

diffdf <- rbind(diffdf21, diffdf23)

dffsummary <- diffdf %>% filter (location_name != "Virgin Islands") %>% group_by(season, location_name) %>% 
  summarise(meanpercdiff = mean(final_perc_change, na.rm= TRUE), 
            perclow = min(final_perc_change, na.rm = TRUE),
            perchigh = max(final_perc_change, na.rm =  TRUE), 
            perciqr = IQR(final_perc_change, na.rm = TRUE),
            perc_median = median(final_perc_change, na.rm = TRUE),
            meanabsolute = mean(absolutediff, na.rm = TRUE), 
            absolow = min(absolutediff, na.rm = TRUE), 
            absohigh = max(absolutediff, na.rm = TRUE), 
            absoiqr = IQR(absolutediff, na.rm = TRUE), 
            median = median(absolutediff, na.rm = TRUE)) %>% ungroup
#write.csv(dffsummary, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code/2022-23/dfsummary_Update.csv"), row.names = FALSE)


#percent of updates where the change is >= 10 hospitalizations 

statediffs21 <- diffdf %>% filter(state != "US", season == "A) 2021-2022", !is.na(absolutediff))
statediffs23 <- diffdf %>% filter(state != "US", season == "B) 2022-2023", !is.na(absolutediff))

#write.csv(diffdf, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/diffdf.csv"))

###### Backfill Matrix Plot

weeklydat21 <- hosp_data_read_func(from = as.Date("2022-01-31"), to = as.Date("2022-07-18"))
weeklydat23 <- hosp_data_read_func(from = as.Date("2022-10-10"), to = as.Date("2023-01-09"))
weeklydat23a <- hosp_data_read_func(from = as.Date("2022-10-11"), to = as.Date("2023-06-13"), tuesday = TRUE) %>% filter(report_date >= "2023-01-17")


weeklydat23 <- rbind(weeklydat23, weeklydat23a)

#write.csv(weeklydat21, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/weeklydat21.csv"))
#write.csv(weeklydat23, paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Manuscripts/hospitalizations_season_2021_2022/Data_for_Figures/weeklydat23.csv"))