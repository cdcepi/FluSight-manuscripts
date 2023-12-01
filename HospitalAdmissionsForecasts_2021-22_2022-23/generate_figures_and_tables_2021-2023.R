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
#CDC UserID goes here
userid="rpe5"

#update path to where cloned GitHub repository lives
githubpath = paste0("C:/Users/",userid,"/Desktop/GitHub")


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

suppressMessages(invisible(source(paste0(manuscript_repo,"/Model names and colors.R"))))
source(paste0(manuscript_repo,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

##################### Inc Rankings/ Table 1

inc.rankings_all21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all21.csv"))
inc.rankings_all23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all23.csv"))

inc.rankings_all <- rbind(mutate(inc.rankings_all21, season = "2021-2022"), mutate(inc.rankings_all23, season = "2022-2023")) %>% arrange(season, rel.WIS.skill)

logtable1 <- read.csv(paste0(manuscript_repo,"/Supplemental_analyses/log-transformed/logtable1.csv")) 

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
#kableExtra::kable_classic() %>% kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/table1.pdf"))


#### 

WIS_Season21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season21.csv"))
WIS_Season23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/WIS_Season23.csv"))

inc.rankings_location21 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location21.csv"))
inc.rankings_location23 <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_location23.csv"))

WIS_Season21 <- WIS_Season21 %>% filter(model %in% inc.rankings_all21$model)
inc.rankings_location21 <- make_WIS_ranking_location(WIS_Season21)
inc.rankings_location21$below <- ifelse(inc.rankings_location21$relative_WIS < 1, 1, 0)

WIS_Season23 <- WIS_Season23 %>% filter(model %in% inc.rankings_all23$model)
inc.rankings_location23 <- make_WIS_ranking_location(WIS_Season23)
inc.rankings_location23$below <- ifelse(inc.rankings_location23$relative_WIS < 1, 1, 0)


############### Forecasts and Observed

#### Alex Need to output all_dat and obs_dat #######

fig21 <- forecastsandobservedplt(all_dat21, obs_data21, "a")


fig23 <- forecastsandobservedplt(all_dat23, obs_data23, "b")

############## Absolute WIS by Model
abs_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
abs_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_not_flusight.csv"))%>% mutate(target_end_date = as.Date(target_end_date))

wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                            `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                            `2021-2022` = "2021-2022", 
                            `2022-2023` = "2022-2023"))




## Figure 4a Absolute (log) WIS by model
figure4a <-  ggplot(abs_flusight, aes(x = target_end_date,
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

figure4a

########## Coverage and Scores

Scores_tab21 <- scores_tab_function(inc.rankings_location21,inc.rankings_all21, WIS_Season21)
Scores_tab23 <- scores_tab_function(inc.rankings_location23, inc.rankings_all23, WIS_Season23)

########## 95% Coverage by Models Figure 4b

coverage95_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))
coverage95_not_flusight <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/coverage95_not_flusight.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

#### Alex Need to output coverage95_notflusight ##########

coverage_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead", 
                                 `4 wk ahead inc flu hosp` = "4 Week Ahead", 
                                 `2021-2022` = "2021-2022", 
                                 `2022-2023` = "2022-2023"#, 
                                 #coverage50 = "Panel C", 
                                 #coverage95 = "Panel D"
))



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

figure4b

############ Coverage Tables Table 2
#This relies on a lot of code above / not self contained sections

WIS_Season <- rbind(mutate(WIS_Season21, season = "2021-2022"), mutate(WIS_Season23, season = "2022-2023"))

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



########### Absolute WIS by Week / Table 3

abs_breakdown_WIS <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/abs_breakdown_WIS.csv"))

#### Alex need to output abs_states

abs_breakdown_WIS %>% select(-season) %>% mutate_if(is.numeric, round, digits = 2) %>%  
  knitr::kable(align = c("lccccc"), caption = "Table 3", col.names = c("Model", "Relative WIS", "1 Wk ABS WIS", "2 Wk ABS WIS", "3 Wk ABS WIS", "4 Wk ABS WIS")) %>% 
  kableExtra::footnote(general = "Table 3", general_title = "")%>% 
  kableExtra::pack_rows(index = table(abs_breakdown_WIS$season)) %>% 
  kableExtra::kable_classic() # %>% 
# kableExtra::save_kable(file = paste0(manuscript_repo,"/Output/","table3.pdf"))



############ Model Ranks

inc_scores_overall <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc_scores_overall.csv"))

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


inc.rankings_all <- rbind(inc.rankings_all21, inc.rankings_all23)

scores_order <- inc.rankings_all
levels_order <- unique(scores_order$model)

#### Alex, I got this to work in flusight qmd, no clue why it's unordered now. ####
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

##### Relative WIS by Location Figure 3
inc.rankings_location <- rbind(mutate(inc.rankings_location21, season = "2021-2022"), mutate(inc.rankings_location23, season = "2022-2023"))
inc.rankings_all_nice <- read.csv(paste0(manuscript_repo, "/Data_for_Figures/inc.rankings_all_nice.csv"))

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


##### Relative WIS Distribution Figure S 1  Sarabeth, where should this be?

model_order <- merge(inc.rankings_all_nice[,c("model", "rel.WIS.skill")], inc.rankings_location, by = "model", all.y = TRUE) %>% arrange(rel.WIS.skill)

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
