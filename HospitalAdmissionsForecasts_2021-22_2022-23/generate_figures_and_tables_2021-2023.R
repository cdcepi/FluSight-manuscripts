###PLEASE READ###

# This file can be used to generate the figures and tables found in the main section of the manuscript using the data output by the data_for_figures_2021-2023.R script.
# The data have been output to the Data_for_Figures folder, so it is not necessary to run data_for_figures_2021-2023.R prior to using this script.
# All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below.
# Each subsequent section contains the vizualization code related to the figure of the same name.

### FluSight 2021-22, 2022-23
### CDC FluSight Team 



######### Setup
library(arrow)
library(tidyverse)
library(covidHubUtils)
library(scoringutils)
library(ggridges)


#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub")


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


inc.rankings_all21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all21.csv"))
inc.rankings_all23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all23.csv"))

inc.rankings_all <- rbind(mutate(inc.rankings_all21, season = "2021-2022"), mutate(inc.rankings_all23, season = "2022-2023")) %>% arrange(season, rel_wis)

logtable1 <- read.csv(paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/logtable1.csv"))


WIS_Season21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season21.csv"))
WIS_Season23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season23.csv"))

inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))


abs_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
abs_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_not_flusight.csv"))%>% mutate(target_end_date = as.Date(target_end_date))

Scores_tab21 <- scores_tab_function(inc.rankings_location21,inc.rankings_all21, WIS_Season21)
Scores_tab23 <- scores_tab_function(inc.rankings_location23, inc.rankings_all23, WIS_Season23)

coverage95_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
coverage95_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_not_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))


WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023"))

cov95_breakdown21 <- cov95_function(WIS_Season21, Scores_tab21)

cov95_breakdown23 <- cov95_function(WIS_Season23, Scores_tab23)


cov95_breakdownall <- rbind(mutate(cov95_breakdown21, season = "2021-2022"), mutate(cov95_breakdown23, season = "2022-2023"))

inc_scores_overall <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc_scores_overall.csv"))

abs_breakdown_WIS <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_breakdown_WIS.csv"))

inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))
inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))


##################### Inc Rankings/ Table 1

#uncomment to just run generate this figure
# inc.rankings_all21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all21.csv"))
# inc.rankings_all23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all23.csv"))
# inc.rankings_all <- rbind(mutate(inc.rankings_all21, season = "2021-2022"), mutate(inc.rankings_all23, season = "2022-2023")) %>% arrange(season, rel_wis)
# logtable1 <- read.csv(paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/logtable1.csv"))

inc.rankings_all %>% left_join(., logtable1, join_by("model" == "Model", "season" == "Season")) %>% 
  rename(Model = model,
         `Absolute WIS` = wis,
         `Relative WIS`= rel_wis,
         `50% Coverage (%)` = cov_50,
         `95% Coverage (%)` = cov_95,
         `% of Forecasts Submitted`  = per_forecasts,
         `% of Locations Forecasted` = per_locations,
         # `% of Locations Fully Forecasted` = frac.locations.fully.forecasted,
         # `% of Submitted Locations with All Forecasts` = frac.submitted.locations.fully.forecasted,
         `Season` = season, 
         `Log Transformed Absolute WIS` = Absolute.WIS, 
         `Log Transformed Relative WIS` = Relative.WIS,
         `MAE` = mae) %>% 
  select(Model, `Absolute WIS`, `Relative WIS`,
         MAE, `50% Coverage (%)`, 
         `95% Coverage (%)`, 
         `% of Forecasts Submitted`, 
         `Log Transformed Absolute WIS`, 
         `Log Transformed Relative WIS`) %>%
  #datatable()
  #arrange(`Season`, `Relative WIS`) %>% 
  mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lcccccccc"), caption = "Table 1",#col.names = c()
  ) %>% 
  kableExtra::pack_rows(index = table(inc.rankings_all$season)) %>% 
  kableExtra::footnote( general_title = "")# %>% 
#kableExtra::kable_classic() %>% kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/table1.pdf"))



##################### Forecasts and Observed Figure 1a and b

#uncomment to just run generate this figure
# all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
# all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
# obs_data21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
# obs_data23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

fig21 <- forecastsandobservedplt(all_dat21, obs_data21, "a")


fig23 <- forecastsandobservedplt(all_dat23, obs_data23, "b")

##################### Absolute WIS by Model

#uncomment to just run generate this figure
# abs_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
# abs_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_not_flusight.csv"))%>% mutate(target_end_date = as.Date(target_end_date))

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"))


##################### Figure 4a Absolute (log) WIS by model
figure4a <-  ggplot(abs_flusight, aes(x = target_end_date,
                                y = log_WIS, group = model,
                                col = model)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c("#d6936b", "#6baed6")) +
  geom_line(data = abs_not_flusight, aes(x = target_end_date, y = log_WIS, group = model), color = adjustcolor("grey50", .35)) +
  labs(y = "Absolute WIS",
       x = "Forecast Target End Date",
       color = "Model",
       title = "Absolute WIS by Model"
  ) +
  theme_bw()+
  scale_x_date(breaks = seq.Date(from = min(abs_flusight$target_end_date), to= max(abs_flusight$target_end_date), by = "2 weeks"), date_labels = "%d %b") +
  scale_y_continuous(breaks = c(0,1,2,3), labels = c("0", "1", "10", "100"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())+
  facet_grid(rows = vars(target), cols = vars(season), labeller = wis_labels,  scales = "free_x")



##################### Coverage and Scores



##################### 95% Coverage by Models Figure 4b

#uncomment to just run generate this figure
coverage95_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
coverage95_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_not_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))


coverage_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                                 `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                                 `2021-2022` = "2021-2022", 
                                 `2022-2023` = "2022-2023"))



figure4b <- ggplot(coverage95_flusight, aes(x = target_end_date, 
                                     y = coverage95, group = model,
                                     col = model)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_line(data = coverage95_not_flusight, aes(x = target_end_date, y = coverage95, group = model), color = adjustcolor("grey50", .35)) + 
  labs(y = "95% Coverage",
       x = "",
       color = "Model",
       title = "95% Coverage by Model") +
  scale_color_manual(values = c("#d6936b", "#6baed6")) +
  theme_bw()+
  scale_x_date(breaks = seq.Date(from = min(coverage95_flusight$target_end_date), to= max(coverage95_flusight$target_end_date), by = "2 weeks"), date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())+
  facet_grid(rows = vars(target), cols = vars(season), labeller = coverage_labels, scales = "free_x")



##################### Coverage Tables Table 2

#uncomment to just run generate this figure
# WIS_Season21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season21.csv"))
# WIS_Season23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season23.csv"))
# inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
# inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))
# inc.rankings_all21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all21.csv"))
# inc.rankings_all23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all23.csv"))
# Scores_tab21 <- scores_tab_function(inc.rankings_location21,inc.rankings_all21, WIS_Season21)
# Scores_tab23 <- scores_tab_function(inc.rankings_location23, inc.rankings_all23, WIS_Season23)
# WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023"))

cov95_breakdown21 <- cov95_function(WIS_Season21, Scores_tab21)

cov95_breakdown23 <- cov95_function(WIS_Season23, Scores_tab23)


cov95_breakdownall <- rbind(mutate(cov95_breakdown21, season = "2021-2022"), mutate(cov95_breakdown23, season = "2022-2023"))

cov95_breakdownall %>% arrange(season, Relative_WIS) %>% mutate_if(is.numeric, round, digits = 2) %>% select(-season) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table 2", col.names = c("Model", "Relative WIS", "% WIS Below Baseline", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage", "% Cov abv 90 (1 Wk)", "% Cov abv 90 (2 Wk)", "% Cov abv 90 (3 Wk)", "% Cov abv 90 (4 Wk)")) %>% 
  kableExtra::footnote(general = "Table 2: % WIS Below Baseline shows the percent of WIS values for each model below the corresponding FluSight-Baseline WIS. The '% Cov abv 90' columns show the percent of weekly 95% coverage values that are greater than or equal to 90% for each model by horizon.", general_title = "") %>% 
  kableExtra::pack_rows(index = table(cov95_breakdownall$season)) %>% 
  kableExtra::kable_classic() # %>% 
# kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/","table2.pdf"))


##################### Absolute WIS by Week / Table 3

# abs_breakdown_WIS <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_breakdown_WIS.csv"))


abs_breakdown_WIS %>% select(-season) %>% mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lccccc"), caption = "Table 3", col.names = c("Model", "Relative WIS", "1 Wk ABS WIS", "2 Wk ABS WIS", "3 Wk ABS WIS", "4 Wk ABS WIS")) %>% 
  kableExtra::footnote(general = "Table 3", general_title = "")%>% 
  kableExtra::pack_rows(index = table(abs_breakdown_WIS$season)) %>% 
  kableExtra::kable_classic() # %>% 
# kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/","table3.pdf"))



##################### Model Ranks Figure 2

#uncomment to just run generate this figure
# inc_scores_overall <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc_scores_overall.csv"))
# inc.rankings_all21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all21.csv"))
# inc.rankings_all23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all23.csv"))

inc.rankings_all <- rbind(inc.rankings_all21, inc.rankings_all23)

scores_order <- inc.rankings_all
levels_order <- unique(scores_order$model)


figure2 <- inc_scores_overall %>% 
  ggplot(aes(y = factor(model, levels = levels_order), x=rev_rank, fill = factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, color = "gray30"
  ) +
  scale_fill_manual(values = c("#6baed6", "#c86bd6","#d6936b","#78d66b"), name = "Quartiles")+
  labs(x = "Standardized Rank", y = "Model", color = "Quartiles")+
  scale_x_continuous(limits=c(0,1)) + 
  theme_bw()+
  facet_grid(rows = vars(season), scales = "free_y", labeller = as_labeller(c(`2021-2022` = "2021-2022",`2022-2023` = "2022-2023")) )+
  theme(strip.text = element_text(size = 10))
figure2

##################### Relative WIS by Location Figure 3 & Supplemental Table X

#uncomment to just run generate this figure
# inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
# inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))
# inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))
# inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))

scores <- inc.rankings_location %>% filter(is.finite(relative_WIS)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))
scores_order <- inc.rankings_all_nice
levels_order <- scores_order$modelorder

figure3 <- ggplot(scores, 
                      aes(x = factor(modelorder, levels = levels_order), y=location_name, 
                          fill= scales::oob_squish(relative_WIS, range = c(- 2.584963, 2.584963)), 
                          group = season)) +
  geom_tile() +
  theme_bw()+
  geom_text(aes(label = signif(relative_WIS, 2)), size = 2.5) + # I adapted the rounding 
  scale_fill_gradient2(low ="#6baed6", high =  "#d6936b", midpoint = 1, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2), 
                       labels =c("0.25", 0.5, 1, 2, 4)) + 
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 7),
        title = element_text(size = 9)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(labels = function(x) substring(x, 1, nchar(x)-10))+
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "2021-2022",`2022-2023` = "2022-2023")))+
  theme(axis.ticks.y = element_blank())
figure3

# this is the output for this figure as a table 
data_csv <- scores %>% select(model, location_name, relative_WIS, season) 
#write.csv(data_csv, paste0(manuscript_repo,"/Supplemental_analyses/Supplemental Output/Table_Figure3_a.csv"),row.names = FALSE)


##################### Relative WIS Distribution Figure S 1

#uncomment to just run generate this figure
# inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))
# inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
# inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))
# inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))

model_order <- merge(inc.rankings_all_nice[,c("model", "rel_wis")], inc.rankings_location, by = "model", all.y = TRUE) %>% arrange(rel_wis)

figures1 <- model_order %>% 
  ggplot( aes(x = fct_inorder(model), y = relative_WIS))+
  geom_boxplot()+
  theme_bw() +
  geom_hline(aes(yintercept = 1), color = "#6baed6")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0,1,0,25))+
  labs(x = "Model", y = "Relative WIS")+
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "A) 2021-2022", `2022-2023` = "B) 2022-2023")))

figures1
