######### Setup

library(RSocrata)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

select = dplyr::select
filter = dplyr::filter

#CDC UserID goes here
userid="rpe5"

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",userid,"/Desktop/GitHub")
manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
flusight_forecast_data <-paste0(githubpath, "/Flusight-forecast-data")
source(paste0(manuscript_repo,"/functions2022-2023.R"))

flu21_target_end_dates <- c("2022-02-26", "2022-03-05", "2022-03-12", "2022-03-19", "2022-03-26", "2022-04-02","2022-04-09", "2022-04-16", 
                            "2022-04-23", "2022-04-30", "2022-05-07", "2022-05-14", "2022-05-21", "2022-05-28", "2022-06-04", "2022-06-11", 
                            "2022-06-18", "2022-06-25", "2022-07-02", "2022-07-09", "2022-07-16")
flu23_target_end_dates <- c("2022-10-22", "2022-10-29", "2022-11-05", "2022-11-12", "2022-11-19", "2022-11-26", "2022-12-03", "2022-12-10",
                            "2022-12-17", "2022-12-24", "2022-12-31", "2023-01-07", "2023-01-14", "2023-01-21", "2023-01-28", "2023-02-04", 
                            "2023-02-11", "2023-02-18", "2023-02-25", "2023-03-04", "2023-03-11", "2023-03-18", "2023-03-25", "2023-04-01",
                            "2023-04-08", "2023-04-15", "2023-04-22", "2023-04-29", "2023-05-06", "2023-05-13", "2023-05-20", "2023-05-27" ,
                            "2023-06-03", "2023-06-10")

obs_data21 <- read_csv(paste0(flusight_forecast_data,"/data-truth/truth-Incident Hospitalizations-Archived_9-12-2022.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(flu21_target_end_dates)), location != 78 )


obs_data21 <- obs_data21 %>%
  rename(value_inc = value,
         target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())

obs_data23 <- read_csv(paste0(flusight_forecast_data,"/data-truth/truth-Incident Hospitalizations-2023-06-23.csv")) %>%
  mutate(wk_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>%
  select(-date) %>%
  filter(wk_end_date %in% as.Date(unique(flu23_target_end_dates)), location != 78)


obs_data23 <- obs_data23 %>%
  dplyr::rename(value_inc = value,
                target_end_date = wk_end_date) %>%
  filter(target_end_date < Sys.Date())

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter


###### Backfill Epi curve: Figure S2

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

#write.csv(fullset, paste0(manuscript_repo, "/Data_for_Figures/fullset.csv"))

##### Backfill Differences: Figure S3

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
#write.csv(dffsummary, paste0(manuscript_repo, "/Data_for_Figures/dfsummary_Update.csv"), row.names = FALSE)


#percent of updates where the change is >= 10 hospitalizations 

statediffs21 <- diffdf %>% filter(state != "US", season == "A) 2021-2022", !is.na(absolutediff))
statediffs23 <- diffdf %>% filter(state != "US", season == "B) 2022-2023", !is.na(absolutediff))

#write.csv(diffdf, paste0(manuscript_repo, "/Data_for_Figures/diffdf.csv"))

###### Backfill Matrix Plot: Figure S4

weeklydat21 <- hosp_data_read_func(from = as.Date("2022-01-31"), to = as.Date("2022-07-18"))
weeklydat23 <- hosp_data_read_func(from = as.Date("2022-10-10"), to = as.Date("2023-01-09"))
weeklydat23a <- hosp_data_read_func(from = as.Date("2022-10-11"), to = as.Date("2023-06-13"), tuesday = TRUE) %>% filter(report_date >= "2023-01-17")


weeklydat23 <- rbind(weeklydat23, weeklydat23a)

#write.csv(weeklydat21, paste0(manuscript_repo, "/Data_for_Figures/weeklydat21.csv"))
#write.csv(weeklydat23, paste0(manuscript_repo, "/Data_for_Figures/weeklydat23.csv"))