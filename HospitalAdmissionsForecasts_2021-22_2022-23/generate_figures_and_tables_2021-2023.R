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


WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023")) %>% rename("target_end_date" = "date", "WIS" = "wis")

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
         `50% Coverage (%)` = Percent.Cov.50,
         `95% Coverage (%)` = Percent.Cov.95,
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
  kableExtra::footnote( general_title = "") %>% 
kableExtra::kable_classic() #%>% kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/Table_1.pdf"))



##################### Forecasts and Observed Figure 1a and b

#uncomment to just run generate this figure
# all_dat21 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat21.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
# all_dat23 <- read_parquet(paste0(manuscript_repo, "/Data_for_Figures/all_dat23.parquet")) %>% mutate(target_end_date = as.Date(target_end_date))
# obs_data21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data21.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
# obs_data23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/obs_data23.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

all_dat_forplt <- bind_rows(mutate(all_dat21, season = "2021-2022"), mutate(all_dat23, season = "2022-2023"))
obs_dat_forplt <- bind_rows(mutate(obs_data21, season = "2021-2022"), mutate(obs_data23, season = "2022-2023"))


figure1 <- forecastsandobservedplt(all_dat_forplt, obs_dat_forplt)
figure1
#additions were made to figure 1 thanks to reviewer comments

all_dat23_ <- all_dat23 %>% mutate(target_end_date = as.Date(target_end_date)) %>%  pivot_wider(names_from = c(type, quantile))
obs_data23_ <- obs_data23 %>% mutate(target_end_date = as.Date(target_end_date))


m_colors <- c('CADPH-FluCAT_Ensemble' = '#FFCC00',
              'CEPH-Rtrend_fluH' = '#0c7eb3',
              'CMU-TimeSeries' = '#481769FF',
              'CU-ensemble' = '#472A7AFF',
              'Flusight-baseline' = '#FDE725FF',
              'GT-FluFNP' = '#3D4E8AFF',
              'ISU_NiemiLab-Flu' = '#d0d90c',
              'JHU_IDD-CovidSP' = '#2e1023',
              'LUcompUncertLab-HWAR2' = '#990000',
              'LUcompUncertLab-KalmanFilter' = '#993366',
              'LUcompUncertLab-ensemble_rclp' = '#9933FF',
              'LUcompUncertLab-hier_mech_model' = '#cc23de',
              'LUcompUncertLab-stacked_ili' = '#22E8CD',
              'MIGHTE-Nsemble' = '#780f57',
              'MOBS-GLEAM_FLUH' = '#1F978BFF',
              'NIH-Flu_ARIMA' = '#00FFCC',
              'PSI-DICE' = '#e66200',
              'SGroup-RandomForest' = '#2EB37CFF',
              'SigSci-CREG' = '#65CB5EFF',
              'SigSci-TSENS' = '#89D548FF',
              'UGA_flucast-OKeeffe' = '#33FF00',
              'UGuelph-FluPLUG' = '#e3b17a',
              'UMass-trends_ensemble' = '#B0DD2FFF',
              'UNC_IDD-InfluPaint' = '#c5aa27',
              'UVAFluX-Ensemble' = '#FDE725FF',
              'VTSanghani-ExogModel' = '#C9E120FF',
              'VTSanghani-Transformer' = '#fffb00', 
              'Reported' = 'black')

## adding in model-specific plots
plts_4 <- 
  all_dat23_ %>% filter((forecast_date == "2022-11-14" | forecast_date == "2022-12-05" | forecast_date == "2023-02-27"), location == "US") %>% 
  ggplot(aes(x = target_end_date, y = quantile_0.5))+
  theme_bw()+
  geom_ribbon(aes(ymin = quantile_0.05, ymax = quantile_0.975, color = model, fill = model), alpha = 0.35, show.legend = FALSE)+
  geom_line(aes(color = model), show.legend = FALSE)+
  geom_point(aes(color = model, fill = model))+
  geom_line(data = filter(obs_data23_, location == "US", target_end_date <= "2023-04-22"), aes(y = value_inc), color = 'black')+
  geom_point(data = filter(obs_data23_, location == "US", target_end_date <= "2023-04-22"), aes(y = value_inc), color = 'black')+
  scale_y_continuous(label = scales::comma)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  scale_color_manual(values = m_colors, drop = FALSE)+
  scale_fill_manual(values = m_colors, drop = FALSE)+
  coord_cartesian(ylim = c(0, 50000))+
  labs(x = NULL, y = "Weekly Hospital Admissions", color = NULL, fill = NULL)+
  facet_wrap(facets = vars(forecast_date), nrow = 1, ncol = 3)+
  guides(color = guide_legend(ncol = 9),
         fill = guide_legend(ncol = 9))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.margin = margin(-5,-5,-5,-20), 
        legend.margin = margin(-5,-5,-5,-20),
        text = element_text(size = 7),
        legend.text = element_text(size = 5),
        legend.key.spacing = unit(0, "mm")
        #axis.text.x = element_text(angle = 45, hjust = 1)
  )



new_figure1 <- plot_grid(figure1, plts_4, ncol = 1)
new_figure1

# ggsave(paste0(manuscript_repo,"/Output/Figure_1.pdf"), plot = new_figure1, width=12, height=8)

##################### Model Ranks Figure 2

#uncomment to just run generate this figure
# WIS_Season21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season21.csv"))
# WIS_Season23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season23.csv"))
# WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023")) %>% rename("target_end_date" = "date", "WIS" = "wis")

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

Figure_2 <- inc_scores_overall %>% 
  ggplot(aes(y = model, x=rev_rank, fill = factor(after_stat(quantile)))) +
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
Figure_2


 # ggsave(paste0(manuscript_repo,"/Output/Figure_2.pdf"), width=8, height=8, units="in", plot = Figure_2)

##################### Relative WIS by Location Figure 3 & Supplemental Table X

#uncomment to just run generate this figure
# inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
# inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))
# inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))
# inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))

scores <- inc.rankings_location %>% filter(is.finite(rel_wis)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))
scores_order <- inc.rankings_all_nice
levels_order <- scores_order$modelorder

Figure_3 <- ggplot(scores, 
                   aes(x = factor(modelorder, levels = levels_order), y=location_name, 
                       fill= scales::oob_squish(rel_wis, range = c(- 2.584963, 2.584963)), 
                       group = season)) +
  geom_tile() +
  theme_bw()+
  geom_text(aes(label = signif(rel_wis, 2)), size = 2.5) + # I adapted the rounding 
  scale_fill_gradient2(low ="#6baed6", high =  "#d6936b", midpoint = 1, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2), 
                       labels =c("0.25", 0.5, 0, 1, 2)) + 
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 7),
        legend.position = "top", 
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.margin = margin(0,-5,-5,-5), 
        title = element_text(size = 9)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(labels = function(x) substring(x, 1, nchar(x)-10))+
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "2021-2022",`2022-2023` = "2022-2023")))+
  theme(axis.ticks.y = element_blank())

Figure_3

 # ggsave(paste0(manuscript_repo,"/Output/Figure_3.pdf"), plot = Figure_3, width = 12, height= 8)


# this is the output for this figure as a table 
# data_csv <- scores %>% select(model, location_name, rel_wis, season)
# write.csv(data_csv, paste0(manuscript_repo,"/Supplemental_analyses/Supplemental Output/Figure_3_Table.csv"),row.names = FALSE)




##################### Absolute WIS by Model

#uncomment to just run generate this figure
# abs_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
# abs_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_not_flusight.csv"))%>% mutate(target_end_date = as.Date(target_end_date))

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"))


##################### Figure 4a Absolute (log) WIS by model
Figure_4 <-  ggplot(abs_flusight, aes(x = target_end_date,
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

# ggsave(paste0(manuscript_repo,"/Output/Figure_4.pdf"), plot = Figure_4a, width = 8, height = 5)

##################### Coverage and Scores



##################### 95% Coverage by Models Figure 4b

#uncomment to just run generate this figure
# coverage95_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
# coverage95_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_not_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))


coverage_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                                 `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                                 `2021-2022` = "2021-2022", 
                                 `2022-2023` = "2022-2023"))



Figure_5 <- ggplot(coverage95_flusight, aes(x = target_end_date, 
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

# ggsave(paste0(manuscript_repo,"/Output/Figure_5.pdf"), width=8, height=6, plot = Figure_4b)

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
# WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023")) %>% rename("target_end_date" = "date", "WIS" = "wis")

cov95_breakdown21 <- cov95_function(WIS_Season21, Scores_tab21)

cov95_breakdown23 <- cov95_function(WIS_Season23, Scores_tab23)


cov95_breakdownall <- rbind(mutate(cov95_breakdown21, season = "2021-2022"), mutate(cov95_breakdown23, season = "2022-2023"))

cov95_breakdownall %>% arrange(season, Relative_WIS) %>% mutate_if(is.numeric, round, digits = 2) %>% select(-season) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table 2", col.names = c("Model", "Relative WIS", "% WIS Below Baseline", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage", "% Cov abv 90 (1 Wk)", "% Cov abv 90 (2 Wk)", "% Cov abv 90 (3 Wk)", "% Cov abv 90 (4 Wk)")) %>% 
  kableExtra::footnote(general = "Table 2: % WIS Below Baseline shows the percent of WIS values for each model below the corresponding FluSight-Baseline WIS. The '% Cov abv 90' columns show the percent of weekly 95% coverage values that are greater than or equal to 90% for each model by horizon.", general_title = "") %>% 
  kableExtra::pack_rows(index = table(cov95_breakdownall$season)) %>% 
  kableExtra::kable_classic()#  %>% 
 #kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/","Table_2.pdf"))


##################### Absolute WIS by Week / Table 3

# abs_breakdown_WIS <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_breakdown_WIS.csv"))


abs_breakdown_WIS %>% select(-season) %>% mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lccccc"), caption = "Table 3", col.names = c("Model", "Relative WIS", "1 Wk ABS WIS", "2 Wk ABS WIS", "3 Wk ABS WIS", "4 Wk ABS WIS")) %>% 
  kableExtra::footnote(general = "Table 3", general_title = "")%>% 
  kableExtra::pack_rows(index = table(abs_breakdown_WIS$season)) %>% 
  kableExtra::kable_classic()# %>% 
 #kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/","Table_3.pdf"))






##################### Relative WIS Distribution Figure S 1

#uncomment to just run generate this figure
# inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))
# inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
# inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))
# inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))

model_order <- merge(inc.rankings_all_nice[,c("model", "rel_wis")], inc.rankings_location, by = "model", all.y = TRUE) %>% arrange(rel_wis.x)

Figure_S1 <- model_order %>% 
  ggplot( aes(x = fct_inorder(model), y = rel_wis.y))+
  geom_boxplot()+
  theme_bw() +
  geom_hline(aes(yintercept = 1), color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0,1,0,25))+
  labs(x = "Model", y = "Relative WIS")+
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "2021-2022", `2022-2023` = "2022-2023")))

Figure_S1
# ggsave(paste0(manuscript_repo,"/Output/Figure_S1.jpg"), plot = Figure_S1, width=10, height=8)
