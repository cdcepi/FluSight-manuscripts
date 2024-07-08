###PLEASE READ###

# This file can be used to generate the figures and tables found in the backfill supplemental section of the manuscript using the data output by the data_for_backfill_figures_2021-2023.R script.
# The data have been output to the Data for Backfill Figures folder, so it is not necessary to run data_for_backfill_figures_2021-2023.R prior to using this script.
# Each section contains the vizualization code related to the figure of the same name.


library(RSocrata)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

select = dplyr::select
filter = dplyr::filter


#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub")
manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
backfill_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23/Supplemental_analyses/backfill-analysis")
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

###### Backfill Epicurve: Figure S2


fullset <- read.csv(paste0(backfill_repo, "/Data for Backfill Figures/fullset.csv")) %>% mutate(report_date = as.Date(report_date)) %>% 
  mutate(season = case_when(season == "A) 2021-2022" ~ "2021-2022",  TRUE ~ "2022-2023"))

figures2 <- 
  ggplot(fullset, aes(x = report_date)) +
  geom_line(aes(y = value, color = "Weekly Reported Hospitalizations"))+
  geom_line(aes(y = truthvalue, color = "Final Hospitalizations"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_vline(aes(xintercept = as.Date("2022-02-21")), linetype = "dashed")+
  labs(y = "Weekly hospitalizations",  x = NULL) +
  scale_color_manual(name = NULL, values = c("Weekly Reported Hospitalizations" = "#6baed6", "Final Hospitalizations" = "#d6936b"))+
  theme_bw() +
  theme(legend.position="bottom")+
  facet_grid(cols = vars(season), scales = "free_x")

figures2

# ggsave(plot = figures2, paste0(manuscript_repo,"/Supplemental_analyses/Supplemental Output/Figure_S2.png"), height = 6, width = 8, dpi = 300)

###### Absolute Difference: Figure S3

diffdf <- read.csv(paste0(backfill_repo, "/Data for Backfill Figures/diffdf.csv")) %>% mutate(season = case_when(season == "A) 2021-2022" ~ "2021-2022", 
                                                                                                                 TRUE ~ "2022-2023"))

figures3a <- diffdf %>% filter(location_name != "US") %>% 
  ggplot(aes(x = absolutediff, y = location_name))+
  geom_boxplot(color = "black", outlier.color = "black")+
  scale_y_discrete( limits = rev) +
  labs(x = "Difference", y = "")+
  theme_bw()+
  facet_grid(cols = vars(season))

figures3a

####### Relative Differences

figures3b <- diffdf %>% filter(location_name != "US") %>% #can take out != National
  ggplot(aes(x = final_perc_change*100, y = location_name))+
  geom_boxplot(color = "black", outlier.color = "black")+
  scale_y_discrete( limits = rev) +
  labs(x = "Percent Difference", y = "")+
  theme_bw()+
  facet_grid(cols = vars(season))
figures3b

###### Backfill Matrix Plot: Figure S4

####check start date
stairstep_function <- function(weeklydat, fromdate, todate){
  matrixset <-  weeklydat %>% group_by(report_date, epiweek) %>%
    summarise(value = sum(value, na.rm = TRUE), date = as.Date(max(date)), asof = as.Date(date + 2)) %>%
    ungroup() %>% #filter(date > as.Date("2022-01-29"), date < as.Date("2022-06-21")) %>%
    select(-epiweek)
  
  
  log10limlow <- min(log10(matrixset$value))
  log10limhigh <- max(log10(matrixset$value))
  
  stairplt <- 
    matrixset %>% 
    ggplot(aes(y = date,x= report_date))+
    geom_tile(aes( fill = log10(value)))+
    theme_bw()+
    geom_label(aes(label = scales::comma(value)), label.padding = unit(.09, "lines"), size = 2.5)+
    scale_fill_gradient(name = NULL,low = "#6baed6", high = "#d6936b", limits = c(log10limlow, log10limhigh)) +
    scale_y_date(date_breaks = "1 week", date_labels = "%b %d", expand = c(0.01,0.01))+
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d", expand = c(0.01,0.01))+
    labs(x = "Week Reported", y = "Initial Admission Week")+
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust = 1))
  return(stairplt)
}

weeklydat21 <- read.csv(paste0(backfill_repo, "/Data for Backfill Figures/weeklydat21.csv")) %>% mutate(date = as.Date(date), report_date = as.Date(report_date))
weeklydat23 <- read.csv(paste0(backfill_repo, "/Data for Backfill Figures/weeklydat23.csv")) %>% mutate(date = as.Date(date), report_date = as.Date(report_date))

figures4a <- stairstep_function(weeklydat21)
figures4b <- stairstep_function(weeklydat23)

