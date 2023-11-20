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

##################### Inc Rankings

inc.rankings_all <- rbind(mutate(inc.rankings_all21, season = "2021-2022"), mutate(inc.rankings_all23, season = "2022-2023")) %>% arrange(season, rel.WIS.skill)

logtable1 <- read.csv(paste0(dashboard_r_code,"/2022-23/supplemental_log-transformed/logtable1.csv")) 

inc.rankings_all %>% left_join(., logtable1, join_by("model" == "Model", "season" == "Season")) %>% 
  rename(Model = model,
         `Absolute WIS` = mean.WIS,
         `Relative WIS`= rel.WIS.skill,
         `50% Coverage (%)` = Percent.Cov.50,
         `95% Coverage (%)` = Percent.Cov.95,
         `% of Forecasts Submitted`  = frac.forecasts.submitted,
         `% of Locations Forecasted` = frac.locations.submitted,
         `% of Locations Fully Forecasted` = frac.locations.fully.forecasted,
         `% of Submitted Locations with All Forecasts` = frac.submitted.locations.fully.forecasted,
         `Season` = season, 
         `Log Transformed Absolute WIS` = Absolute.WIS, 
         `Log Transformed Relative WIS` = Relative.WIS) %>% 
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
  kableExtra::footnote( general_title = "") #%>% 
#kableExtra::kable_classic() %>% kableExtra::save_kable(file = paste0(dashboard_r_code,"/2022-23/table1_all.pdf"))




WIS_Season21 <- WIS_Season21 %>% filter(model %in% inc.rankings_all21$model)
inc.rankings_location21 <- make_WIS_ranking_location(WIS_Season21)
inc.rankings_location21$below <- ifelse(inc.rankings_location21$relative_WIS < 1, 1, 0)

WIS_Season23 <- WIS_Season23 %>% filter(model %in% inc.rankings_all23$model)
inc.rankings_location23 <- make_WIS_ranking_location(WIS_Season23)
inc.rankings_location23$below <- ifelse(inc.rankings_location23$relative_WIS < 1, 1, 0)


############### Forecasts and Observed

fig21 <- forecastsandobservedplt(all_dat21, obs_data21, "a")


fig23 <- forecastsandobservedplt(all_dat23, obs_data23, "b")

############## Absolute WIS by Model

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"#, 
                            #abs_WIS = "Panel A", 
                            #log_WIS = "Panel B"
))


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


lg <-  ggplot(abs_flusight, aes(x = target_end_date,
                                y = log_WIS, group = model,
                                col = model)) +
  geom_line(size = 1) + geom_point(size = 2) +
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

lg

########## Coverage and Scores

Scores_tab21 <- scores_tab_function(inc.rankings_location21,inc.rankings_all21, WIS_Season21)
Scores_tab23 <- scores_tab_function(inc.rankings_location23, inc.rankings_all23, WIS_Season23)

########## 95% Coverage by Models

coverage_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                                 `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                                 `2021-2022` = "2021-2022", 
                                 `2022-2023` = "2022-2023"#, 
                                 #coverage50 = "Panel C", 
                                 #coverage95 = "Panel D"
))



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

############ Coverage Tables

locationcount <- length(unique(WIS_Season$location_name))# - 1

coverage95_summary <- WIS_Season %>%  filter(model == "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "1 wk ahead inc flu hosp") %>% 
  group_by(forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage95 = sum(coverage.95)/locationcount) %>% ungroup() %>% unique()

coverage95_summary_4 <- WIS_Season %>%  filter(model == "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "4 wk ahead inc flu hosp") %>% 
  group_by(forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage95 = sum(coverage.95)/locationcount) %>% ungroup() %>%  unique()
coverage50_summary_4 <- WIS_Season %>%  filter(model == "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "4 wk ahead inc flu hosp") %>% 
  group_by(forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage50 = sum(coverage.50)/locationcount) %>% unique()


coverage50_summary <- WIS_Season %>% filter(model == "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "1 wk ahead inc flu hosp") %>% 
  group_by(forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage50 = sum(coverage.50)/locationcount) %>% ungroup() %>% unique()

coverage95_summary_all <- WIS_Season %>%  filter(model != "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "1 wk ahead inc flu hosp") %>% 
  group_by(model, forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage95 = sum(coverage.95)/locationcount) %>% unique() %>% 
  group_by(model) %>% summarise(model = model,
                                avg = mean(coverage95)) %>% unique()

coverage50_summary_all <- WIS_Season %>% filter(model != "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "1 wk ahead inc flu hosp") %>% 
  group_by(model, forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage50 = sum(coverage.50)/locationcount)# %>% 

coverage95_summary_all2 <- WIS_Season %>%  filter(model != "Flusight-ensemble") %>% filter(location_name != "National") %>%
  filter(target == "2 wk ahead inc flu hosp") %>% 
  group_by(model, forecast_date, season) %>% 
  summarise(model = model,
            forecast_date = as.Date(forecast_date, format = "%Y-%m-%d"),
            coverage95 = sum(coverage.95)/locationcount) %>% unique() %>% 
  group_by(model) %>% summarise(model = model,
                                avg = mean(coverage95)) %>% unique()

weekly_breakdown <- WIS_Season %>% group_by(model, season) %>% summarise(
  model = model,
  One_week_Cov = mean(coverage.95[target == "1 wk ahead inc flu hosp"])*100,
  Two_week_Cov = mean(coverage.95[target == "2 wk ahead inc flu hosp"])*100,
  Three_week_Cov = mean(coverage.95[target == "3 wk ahead inc flu hosp"])*100,
  Four_week_Cov = mean(coverage.95[target == "4 wk ahead inc flu hosp"])*100
) %>% unique()



cov95_breakdown21 <- cov95_function(WIS_Season21, Scores_tab21)

cov95_breakdown23 <- cov95_function(WIS_Season23, Scores_tab23)


cov95_breakdownall <- rbind(mutate(cov95_breakdown21, season = "2021-2022"), mutate(cov95_breakdown23, season = "2022-2023"))

cov95_breakdownall %>% arrange(season, Relative_WIS) %>% mutate_if(is.numeric, round, digits = 2) %>% select(-season) %>%  
  knitr::kable(align = c("lcccccccccc"), caption = "Table 2", col.names = c("Model", "Relative WIS", "% WIS Below Baseline", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage", "% Cov abv 90 (1 Wk)", "% Cov abv 90 (2 Wk)", "% Cov abv 90 (3 Wk)", "% Cov abv 90 (4 Wk)")) %>% 
  kableExtra::footnote(general = "Table 2: % WIS Below Baseline shows the percent of WIS values for each model below the corresponding FluSight-Baseline WIS. The '% Cov abv 90' columns show the percent of weekly 95% coverage values that are greater than or equal to 90% for each model by horizon.", general_title = "") %>% 
  kableExtra::pack_rows(index = table(cov95_breakdownall$season)) %>% 
  kableExtra::kable_classic() # %>% 

########### Absolute WIS by Week

abs_breakdown_WIS %>% select(-season) %>% mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lccccc"), caption = "Table 3", col.names = c("Model", "Relative WIS", "1 Wk ABS WIS", "2 Wk ABS WIS", "3 Wk ABS WIS", "4 Wk ABS WIS")) %>% 
  kableExtra::footnote(general = "Table 3", general_title = "")%>% 
  kableExtra::pack_rows(index = table(abs_breakdown_WIS$season)) %>% 
  kableExtra::kable_classic() # %>% 
# kableExtra::save_kable(file = paste0(dashboard_r_code,"/2022-23/","table3.pdf"))

fs_wis_min_date21 <- abs_states %>% filter(season == "2021-2022", model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max_date21 <- abs_states %>% filter(season == "2021-2022",model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min4_date21 <- abs_states %>% filter(season == "2021-2022", model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max4_date21 <- abs_states %>% filter(season == "2021-2022",model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min_date23 <- abs_states %>% filter(season == "2022-2023", model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max_date23 <- abs_states %>% filter(season == "2022-2023",model == "Flusight-ensemble", target == "1 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

fs_wis_min4_date23 <- abs_states %>% filter(season == "2022-2023", model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(abs_WIS) %>% {head(., n = 1)} %>% pull(target_end_date)
fs_wis_max4_date23 <- abs_states %>% filter(season == "2022-2023",model == "Flusight-ensemble", target == "4 wk ahead inc flu hosp") %>% arrange(desc(abs_WIS)) %>% {head(., n = 1)} %>% pull(target_end_date)

############ Model Ranks

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

ave_rank_percent21 <- average_rank_percent %>% filter(season == "2021-2022") %>% ungroup()
ave_rank_percent23 <- average_rank_percent %>% filter(season == "2022-2023") %>% ungroup()

bimodal_rank21 <- ave_rank_percent21 %>% mutate(pct_top_bottom = pct_top25 + pct_bottom25) %>% filter(pct_top_bottom >=75)
bimodal_rank23 <- ave_rank_percent23 %>% mutate(pct_top_bottom = pct_top25 + pct_bottom25) %>% filter(pct_top_bottom >=75)

scores_order <- inc.rankings_all
levels_order <- unique(scores_order$model)

p2 <- inc_scores_overall %>% 
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
p2

##### Relative WIS by Location

scores <- inc.rankings_location %>% filter(is.finite(relative_WIS)) %>% left_join(., y = inc.rankings_all_nice[,c("model","season", "modelorder")], by = c("model", "season"))
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
  facet_grid(cols = vars(season), scales = "free_x", labeller = as_labeller(c(`2021-2022` = "2021-2022",`2022-2023` = "2022-2023")))+
  theme(axis.ticks.y = element_blank())
fig_wis_loc


ggsave(paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code/2022-23/WIS_scores_Location_all.jpg"), plot = fig_wis_loc, width = 12, height= 8)


modelrankings21 <- inc.rankings_location21 %>%  ungroup()%>% group_by(model) %>% summarise(low = min(relative_WIS), high = max(relative_WIS), median = median(relative_WIS), mean = mean(relative_WIS)) %>% mutate(diff = high - low)

modelrankings23 <- inc.rankings_location23 %>%  ungroup()%>% group_by(model) %>% summarise(low = min(relative_WIS), high = max(relative_WIS), median = median(relative_WIS), mean = mean(relative_WIS)) %>% mutate(diff = high - low)

scores_tab_nice21 <- Scores_tab21 %>% mutate(across(where(is.numeric),~round(.x, 2)))

scores_tab_nice23 <- arrange(filter(Scores_tab23,  model != "Flusight-ensemble", below_baseline_pct > 50), desc(below_baseline_pct)) %>% mutate(across(where(is.numeric),~round(.x, 2)))

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

model_order <- merge(inc.rankings_all_nice[,1:3], inc.rankings_location, by = "model", all.y = TRUE) %>% arrange(rel.WIS.skill)

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

###### Backfill Epicurve

wklyplt <- 
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

wklyplt

###### Absolute Difference

absoplt <- diffdf %>% filter(location_name != "US") %>% 
  ggplot(aes(x = absolutediff, y = location_name))+
  geom_boxplot(color = "#d6936b", outlier.color = "black")+
  scale_y_discrete( limits = rev) +
  labs(x = "Difference", y = "", title = "Absolute Changes")+
  theme_bw()+
  facet_grid(cols = vars(season))

absoplt

####### Relative Differences

percplt <- diffdf %>% filter(location_name != "US") %>% #can take out != National
  ggplot(aes(x = final_perc_change*100, y = location_name))+
  geom_boxplot(color = "#6baed6", outlier.color = "black")+
  scale_y_discrete( limits = rev) +
  labs(x = "Percent Difference", y = "", title = "Relative Changes")+
  theme_bw()+
  facet_grid(cols = vars(season))
percplt

###### Backfill Matrix Plot

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

stairplt21 <- stairstep_function(weeklydat21)
stairplt23 <- stairstep_function(weeklydat23)

stairplt21
stairplt23
