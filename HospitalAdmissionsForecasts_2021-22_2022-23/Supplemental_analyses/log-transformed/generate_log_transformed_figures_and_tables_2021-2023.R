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

##### Season Rankings

inc.rankings_all %>%
  rename(Model = model,
         `Absolute WIS` = mean.WIS,
         `Relative WIS`= rel.WIS.skill,
         `50% Coverage (%)` = Percent.Cov.50,
         `95% Coverage (%)` = Percent.Cov.95,
         `% of Forecasts Submitted`  = frac.forecasts.submitted,
         `% of Locations Forecasted` = frac.locations.submitted,
         `% of Locations Fully Forecasted` = frac.locations.fully.forecasted,
         `% of Submitted Locations with All Forecasts` = frac.submitted.locations.fully.forecasted,
         `Season` = season) %>% 
  select(Model, `Absolute WIS`, `Relative WIS`,
         MAE, `50% Coverage (%)`, 
         `95% Coverage (%)`, 
         `% of Forecasts Submitted`) %>%
  #datatable()
  #arrange(`Season`, `Relative WIS`) %>% 
  mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table 1",#col.names = c()
  ) %>% 
  kableExtra::pack_rows(index = table(inc.rankings_all$season)) %>% 
  kableExtra::footnote( general_title = "") #%>% 
#kableExtra::kable_classic() %>% kableExtra::save_kable(file = paste0(dashboard_r_code,"/2022-23/supplemental_log-transformed/table1_all.pdf"))

##### Forecasts and Observed

fig21ln <- forecastsandobservedplt(all_dat21ln, obs_data21ln, "a")


fig23ln <- forecastsandobservedplt(all_dat23ln, obs_data23ln, "b")

##### Absolute WIS by model

g <- ggplot(abs_flusight, aes(x = target_end_date, 
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

g

##### Coverage Figures

Scores_tab21ln <- scores_tab_function(inc.rankings_location21ln,inc.rankings_all21ln, WIS_Season21ln)
Scores_tab23ln <- scores_tab_function(inc.rankings_location23ln, inc.rankings_all23ln, WIS_Season23ln)

##### 95% Coverage by Model

coverage_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                                 `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                                 `2021-2022` = "2021-2022", 
                                 `2022-2023` = "2022-2023"))



z <- ggplot(coverage95_flusight, aes(x = target_end_date, 
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

z

##### 50% coverage by model

y <- ggplot(coverage50_flusight, aes(x = target_end_date, 
                                     y = coverage50, group = model,
                                     col = model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_line(data = coverage50_not_flusight, aes(x = target_end_date, y = coverage50, group = model), color = adjustcolor("grey50", .35)) + 
  labs(y = "50% Coverage",
       x = "",
       color = "Model",
       title = "50% Coverage by Model") +
  scale_color_manual(values = c("#d6936b", "#6baed6")) +
  theme_bw()+
  scale_x_date(breaks = seq.Date(from = min(coverage50_flusight$target_end_date), to= max(coverage50_flusight$target_end_date), by = "2 weeks"), date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())+
  facet_grid(rows = vars(target), cols = vars(season), labeller = coverage_labels, scales = "free_x")
#theme_minimal()
y

##### Coverage Tables

cov95_breakdownall %>% arrange(season, Relative_WIS) %>% mutate_if(is.numeric, round, digits = 2) %>% select(-season) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table 2", col.names = c("Model", "Relative WIS", "% WIS Below Baseline", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage", "% Cov abv 90 (1 Wk)", "% Cov abv 90 (2 Wk)", "% Cov abv 90 (3 Wk)", "% Cov abv 90 (4 Wk)")) %>% 
  kableExtra::footnote(general = "Table 2: % WIS Below Baseline shows the percent of WIS values for each model below the corresponding FluSight-Baseline WIS. The '% Cov abv 90' columns show the percent of weekly 95% coverage values that are greater than or equal to 90% for each model by horizon.", general_title = "") %>% 
  kableExtra::pack_rows(index = table(cov95_breakdownall$season)) %>% 
  kableExtra::kable_classic()

##### Absolute WIS by Week Table


abs_breakdown_WIS %>% select(-season) %>% mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lccccc"), caption = "Table 3", col.names = c("Model", "Relative WIS", "1 Wk ABS WIS", "2 Wk ABS WIS", "3 Wk ABS WIS", "4 Wk ABS WIS")) %>% 
  kableExtra::footnote(general = "Table 3", general_title = "")%>% 
  kableExtra::pack_rows(index = table(abs_breakdown_WIS$season)) %>% 
  kableExtra::kable_classic() # %>% 
# kableExtra::save_kable(file = paste0(dashboard_r_code,"/2022-23/supplemental_log-transformed/","table3.pdf"))

##### Model rank plot


p2 <- inc_scores_overall %>% 
  ggplot(aes(y=model, x=rev_rank, fill = factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, color = "gray30"
  ) +
  scale_fill_manual(values = c("#6baed6", "#c86bd6","#d6936b","#78d66b"), name = "Quartiles")+
  labs(x = "Standardized Rank", y = "Model", color = "Quartiles")+
  scale_x_continuous(limits=c(0,1)) + 
  theme_bw()+
  facet_grid(rows = vars(season), scales = "free_y", labeller = as_labeller(c("2021-2022" = "A") "2021-2022","2022-2023" = "B") "2022-2023")))
p2

##### Relative WIS by Location

scores_order <- inc.rankings_all_nice
levels_order <- scores_order$modelorder

fig_wis_loc <- ggplot(scores, 
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
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "A) 2021-2022",`2022-2023` = "B) 2022-2023")))+
  theme(axis.ticks.y = element_blank())
fig_wis_loc

# jpeg(file = paste0(manuscript_repo, "/Supplemental_analyses/Supplemental Output/WIS_scores_Location_all.jpg"), width=12, height=8, units="in", res=300)
# print(fig_wis_loc)
# dev.off()
# 
#ggsave(paste0(manuscript_repo, "/Supplemental_analyses/Supplemental Output/logWIS_scores_Location_all.png"), plot = fig_wis_loc, width = 12, height= 8)

##### WIS avg by week

x <- ggplot(plot.scores, aes(x = target_end_date, 
                             y = Avg_WIS, group = model,
                             col = model)) +
  geom_line() + geom_point() +
  labs(y = "Average WIS",
       x = "",
       color = "Model",
       title = "Average 4-Week Ahead Weighted Interval Scores by Model") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = c( "#d66bae", "#6baed6", "#aed66b"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(season), scales = "free_x")
x

##### Relative WIS Distribution


model_order <- merge(inc.rankings_all_nice[,c(1:3, 11)], inc.rankings_location, by = c("model","season"), all.y = TRUE) %>% arrange(season,rel.WIS.skill)

u <- model_order %>% 
  ggplot( aes(x = fct_inorder(model), y = relative_WIS))+
  geom_boxplot()+
  theme_bw() +
  geom_hline(aes(yintercept = 1), color = "#6baed6")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0,1,0,25))+
  labs(x = "Model", y = "Relative WIS")+
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "A) 2021-2022", `2022-2023` = "B) 2022-2023")))

u
