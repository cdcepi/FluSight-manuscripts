###PLEASE READ###

# This file can be used to generate the figures and tables found in the log-transformed supplemental section of the manuscript using the data output by the data_for_log_transformed_figures_2021-2023.R script. 
# The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run data_for_log_transformed_figures_2021-2023.R prior to using this script. 
# All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. 
# Each subsequent section contains the vizualization code related to the figure of the same name.



### Setup

library(tidyverse)
library(epiDisplay)
library(MMWRweek)
library(DT)
library(plotly)
library(gridExtra)
library(covidHubUtils)
library(ggridges)
#library(viridis)
#library(cowplot)
#library(scales)
#library(RSocrata)
library(arrow)


'%!in%' <- Negate('%in%')

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub")
manuscript_repo <- paste0(githubpath, "/FluSight-manuscripts/HospitalAdmissionsForecasts_2021-22_2022-23")
flusight_forecast_data <-paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-forecast-data")

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##### Season Rankings: Table S3

inc.rankings_all <- read.csv(paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc.rankings_all.csv"))

inc.rankings_all %>%
  rename(Model = model,
         `Absolute WIS` = wis,
         `Relative WIS`= rel_wis,
         `50% Coverage (%)` = Percent.Cov.50,
         `95% Coverage (%)` = Percent.Cov.95,
         `% of Forecasts Submitted`  = per_forecasts,
         `% of Locations Forecasted` = per_locations,
         # `% of Locations Fully Forecasted` = frac.locations.fully.forecasted,
         # `% of Submitted Locations with All Forecasts` = frac.submitted.locations.fully.forecasted,
         `Season` = season, 
         `MAE` = mae) %>% 
  select(Model, `Absolute WIS`, `Relative WIS`,
         MAE, `50% Coverage (%)`, 
         `95% Coverage (%)`, 
         `% of Forecasts Submitted`) %>%
  #datatable()
  #arrange(`Season`, `Relative WIS`) %>% 
  mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table S3",#col.names = c()
  ) %>% 
  kableExtra::pack_rows(index = table(inc.rankings_all$season)) %>% 
  kableExtra::footnote( general_title = "") %>% 
kableExtra::kable_classic() %>% kableExtra::save_kable(file = paste0(manuscript_repo,"/Supplemental_analyses/Supplemental Output/table_S3.pdf"))



##### Absolute WIS by Week Table: Table S4

cov95_breakdownall <- read.csv(paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/cov95_breakdownall.csv"))

cov95_breakdownall %>% arrange(season, Relative_WIS) %>% mutate_if(is.numeric, round, digits = 2) %>% select(-season) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table S4", col.names = c("Model", "Relative WIS", "% WIS Below Baseline", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage", "% Cov abv 90 (1 Wk)", "% Cov abv 90 (2 Wk)", "% Cov abv 90 (3 Wk)", "% Cov abv 90 (4 Wk)")) %>% 
  kableExtra::footnote(general = "Table S4: % WIS Below Baseline shows the percent of WIS values for each model below the corresponding FluSight-Baseline WIS. The '% Cov abv 90' columns show the percent of weekly 95% coverage values that are greater than or equal to 90% for each model by horizon.", general_title = "") %>% 
  kableExtra::pack_rows(index = table(cov95_breakdownall$season)) %>% 
  kableExtra::kable_classic()  %>% 
kableExtra::save_kable(file = paste0(manuscript_repo,"/Supplemental_analyses/Supplemental Output/table_S4.pdf"))

##### Model rank plot: Figure S5

WIS_Season <- read.csv(paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/WIS_Season.csv"))

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



figures5 <- inc_scores_overall %>% 
  ggplot(aes(y=model, x=rev_rank, fill = factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, color = "gray30"
  ) +
  scale_fill_manual(values = c("#6baed6", "#c86bd6","#d6936b","#78d66b"), name = "Quartiles")+
  labs(x = "Standardized Rank", y = "Model", color = "Quartiles")+
  scale_x_continuous(limits=c(0,1)) + 
  theme_bw()+
  facet_grid(rows = vars(season), scales = "free_y", labeller = as_labeller(c(`2021-2022` = "A) 2021-2022",`2022-2023` = "B) 2022-2023")))

# ggsave(paste0(manuscript_repo, "/Supplemental_analyses/Supplemental Output/Figure_S5.png"), width=8, height=8, units="in", plot = figures5)

##### Absolute WIS by model: Figure S6

abs_states <- read.csv(paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/abs_states.csv")) %>% 
  mutate(target_end_date = as.Date(target_end_date))

abs_flusight <- abs_states %>% filter(model %in% c("Flusight-baseline", "Flusight-ensemble"))
abs_not_flusight <- abs_states %>% filter(model %!in% c("Flusight-baseline", "Flusight-ensemble"))

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"))


figures6 <- ggplot(abs_flusight, aes(x = target_end_date, 
                              y = abs_WIS, group = model,
                              col = model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  scale_color_manual(values = c("#d6936b", "#6baed6")) +
  geom_line(data = abs_not_flusight, aes(x = target_end_date, y = abs_WIS, group = model), color = adjustcolor("grey50", .35)) + 
  labs(y = "Absolute WIS",
       x = "Forecast Target End Date",
       color = "Model",
       title = "Absolute WIS by Model") +
  theme_bw()+
  scale_x_date(breaks = seq.Date(from = min(abs_flusight$target_end_date), to= max(abs_flusight$target_end_date), by = "2 weeks"), date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())+
  facet_grid(rows = vars(target), cols = vars(season), labeller = wis_labels,  scales = "free_x")

# ggsave(paste0(manuscript_repo, "/Supplemental_analyses/Supplemental Output/Figure_S6.png"), plot = figures6, width = 8, height = 5)

##### Relative WIS by Location Figure S7
inc.rankings_all <- read.csv(paste0(manuscript_repo, "/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/inc.rankings_all.csv"))

inc.rankings_all_nice <- inc.rankings_all %>% group_by(season) %>% arrange(season, rel_wis) %>% mutate(modelorder = paste(model, season))

scores <- read.csv(paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/Data for Log-Transformed Figures/scores.csv"))

scores_order <- inc.rankings_all_nice
levels_order <- scores_order$modelorder

figures7 <- ggplot(scores, 
                      aes(x = factor(modelorder, levels = levels_order), y=location_name, 
                          fill= scales::oob_squish(rel_wis, range = c(- 2.584963, 2.584963)), 
                          group = season)) +
  geom_tile() +
  theme_bw()+
  geom_text(aes(label = signif(rel_wis, 2)), size = 2.5) + # I adapted the rounding 
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
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "A) 2021-2022",`2022-2023` = "B) 2022-2023")))+
  theme(axis.ticks.y = element_blank())

# ggsave(paste0(manuscript_repo, "/Supplemental_analyses/Supplemental Output/Figure_S7.png"), plot = figures7, width = 12, height= 8)

