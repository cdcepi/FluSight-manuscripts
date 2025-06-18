## FluSight 2023/2024 End of Year Report 
## CDC FluSight Team 
## 3/31/2025


# Packages
# install.packages("https://github.com/epiforecasts/scoringutils/archive/refs/tags/v1.2.2.tar.gz", repos = NULL, type = "source")
library(scoringutils)
library(tidyverse)
# install.packages("hubData", repos = c("https://hubverse-org.r-universe.dev", "https://cloud.r-project.org"))
library(hubData)
library(ggridges)

# Setting up paths to be used for rest of script
dashboard_r_code <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-manuscripts/FluSight2023_24")
flusight_forecast_data <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-forecast-hub")

source(paste0(dashboard_r_code,"/functions2023-2024.R"))

#set forms for common functions
select = dplyr::select
filter = dplyr::filter
mutate = dplyr::mutate

#read in model type as designated using "FluSight-manuscripts/FluSight2023_24/model_type_designation.R"
model_metadata <- read.csv(paste0(dashboard_r_code, "/model_types.csv")) %>% select(-methods, -model_name_pulled) %>% 
  ### Based on team feedback, updating model types here ###
  mutate(mechanistic = case_when(model_name == "UNC_IDD-InfluPaint" ~ FALSE,
                                 model_name == "FluSight-ensemble" ~ FALSE,
                                 model_name == "FluSight-lop_norm" ~ FALSE,
                                 TRUE ~ mechanistic), 
         stat = case_when(model_name == "CEPH-Rtrend_fluH" ~ TRUE, 
                          model_name == "FluSight-ensemble" ~ FALSE,
                          model_name == "FluSight-lop_norm" ~ FALSE,
                          TRUE ~ stat), 
         ai_ml = case_when(model_name == "UGA_flucast-INFLAenza" ~ TRUE, 
                           model_name == "FluSight-ensemble" ~ FALSE,
                           model_name == "FluSight-lop_norm" ~ FALSE,
                           TRUE ~ ai_ml)) %>% 
  mutate(model_type = case_when(mechanistic == TRUE & ai_ml == TRUE & stat == TRUE ~ "mech, AI/ML, stat", 
                                mechanistic == FALSE & ai_ml == TRUE & stat == TRUE ~ "AI/ML, stat",
                                mechanistic == TRUE & ai_ml == FALSE & stat == TRUE ~ "mech, stat",
                                mechanistic == TRUE & ai_ml == TRUE & stat == FALSE ~ "mech, AI/ML",
                                mechanistic == TRUE & ai_ml == FALSE & stat == FALSE ~ "mech",
                                mechanistic == FALSE & ai_ml == TRUE & stat == FALSE ~ "AI/ML",
                                mechanistic == FALSE & ai_ml == FALSE & stat == TRUE ~ "stat"))

#connect to FluSight Hub
hub_con <- connect_hub(flusight_forecast_data) 
raw_forecasts <- hub_con %>%
  dplyr::filter(
    reference_date != "2023-10-07", #UMass submitted test forecasts prior to this date 
    reference_date <= "2024-05-30",
    output_type == "quantile",
    location != "78"
  ) %>% 
  dplyr::collect() %>%
  as_model_out_tbl()

# pull raw target data 
raw_target <-readr::read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/auxiliary-data/target-data-archive/target-hospital-admissions_2024-04-27.csv")%>% 
  mutate(date = as.Date(date)) %>% select(-1)

raw_data <- raw_forecasts %>% 
  dplyr::filter(horizon > -1) %>% 
  dplyr::filter(output_type == "quantile") %>% 
  mutate(target_end_date = as.Date(target_end_date)) %>% 
  dplyr::left_join(
    raw_target %>%  dplyr::select(target_end_date = date, location, location_name, true_value = value),
    by = c("location", "target_end_date")
  ) %>% 
  dplyr::rename(model=model_id, quantile=output_type_id) %>% 
  dplyr::mutate(quantile = as.numeric(quantile)) %>% dplyr::filter(target_end_date <= as.Date("2024-05-01")) %>% ## May 1 was the last date of mandatory reporting
  filter(!(location_name == "Massachusetts" & reference_date == "2023-12-09")) ## MA did not report these data on time, 

all_dat24 <- raw_data %>% 
  rename("forecast_date" = reference_date, "type" = output_type) %>% 
  unite(target, horizon, target, sep = " ", remove = T) %>% 
  dplyr::select(location, target, target_end_date, forecast_date, type, quantile, value, model, location_name) %>% 
  mutate(location_name = ifelse(location == 'US', 'National', location_name))%>% 
  mutate(logvalue = log1p(value))

obs_data24 <- raw_target %>%
  mutate(target_end_date = as.Date(date, "%m/%d/%y"),
         location_name = ifelse(location == 'US', 'National', location_name)) %>% 
  rename(report = value) %>%
  select(-date) %>%
  filter(target_end_date %in% as.Date(unique(all_dat24$target_end_date)))%>% 
  dplyr::select(location, location_name, report, target_end_date) %>% 
  mutate(logreport = log1p(report))

location.names24 = obs_data24 %>% select(location, location_name) %>% unique()

## Inclusion criteria: models that submitted at least 75% of forecasts, not US
include24 <- all_dat24 %>% 
  mutate(baseline_n_forecasts = sum(!is.na(value[model == "FluSight-baseline"]))) %>% 
  group_by(model) %>% 
  mutate(n_forecasts = sum(!is.na(value)),
         n_locations = length(unique(location))) %>%
  mutate(per_forecasts = round((n_forecasts/(baseline_n_forecasts))*100,2),
         per_locations = round((n_locations/length(unique(all_dat24$location)))*100,2)) %>% 
  filter(per_forecasts >= 75) %>%
  filter(location !="US") %>% 
  select(model,n_forecasts, per_forecasts, n_locations, per_locations) %>% 
  distinct() %>% 
  ungroup()


#function for prepping the data for scoring 
### inclusion criteria were added to dat_for_scores_function 

dat_for_scores24 <- dat_for_scores_function(all_dat24, obs_data24, include24) 
dat_for_scores24_log <- dat_for_scores_function_log(all_dat24, obs_data24, include24) 


## Getting coverage from non-log-transformed values
raw_scores24 <- dat_for_scores24 %>% select(-c(logvalue, logreport)) %>% 
#  as_forecast_quantile(observed = "true_value", predicted = "prediction", quantile_level = "quantile") %>% 
  scoringutils::score()

wis_season_by_model_24 <- raw_scores24 %>%
  filter(location != "US", model %in% include24$model) %>% 
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"), relative_skill=TRUE,  baseline="FluSight-baseline", na.rm  = TRUE)%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2))%>%
  select(model, cov_50, cov_95) ## rel wis, wis, and mae only on log scores 

## scoring log-transformed values
raw_scores24_log <- dat_for_scores24_log %>% select(-c(value, report)) %>% 
 # as_forecast_quantile(observed = "true_value", predicted = "prediction", quantile_level = "quantile") %>% 
  scoringutils::score()

wis_season_by_model_24_log <- raw_scores24_log %>%
  filter(location != "US", model %in% include24$model) %>% 
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline", na.rm  = TRUE)%>%
  mutate(wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae) ## Not using log coverage 

## joining log and non-log transformed scores 
wis_season_by_model <- left_join(wis_season_by_model_24_log, wis_season_by_model_24, join_by("model")) ## Coverage is not log-transformed, rel wis is. 


## this starts creation of output for table 1
inc.rankings_all24 <- include24 %>%
  left_join(wis_season_by_model, by="model")

## method for checking to see coverage %
WIS_all24 <- raw_scores24_log %>% summarise_scores(na.rm = TRUE) %>% 
  mutate(wis=round(interval_score,2))

dat_24 <- dat_for_scores24 %>% select(-c(logvalue, logreport)) %>% 
  pivot_wider(names_from = c(type, quantile), values_from=c(prediction, true_value)) %>% 
  select(target, model, forecast_date, location, date, prediction_quantile_0.025, prediction_quantile_0.25, 
         prediction_quantile_0.5, prediction_quantile_0.75, prediction_quantile_0.975, true_value_quantile_0.5)

WIS_all24 <- merge(WIS_all24, dat_24, by = c("target", "model", "forecast_date", "location", "date")) %>% 
  rename("report" = "true_value_quantile_0.5") %>% 
  mutate(coverage.50 = ifelse(report >= prediction_quantile_0.25 & report <= prediction_quantile_0.75,T,F),  
         coverage.95 = ifelse(report >= prediction_quantile_0.025 & report <= prediction_quantile_0.975,T,F))

# pull out data on forecasts 
WIS_alllocations24 <- WIS_all24

WIS_all24 = filter(WIS_all24, location_name != "National")

WIS_Season24 <- WIS_all24

and_coverage_24 <- WIS_Season24 %>% 
  group_by(model) %>% 
  summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% 
  distinct() %>% 
  ungroup()
inc.rankings_all24 <- left_join(inc.rankings_all24, and_coverage_24, by = "model")

### Figure 1: National ensemble Forecasts

ntl_ens_forecasts <- all_dat24 %>% filter(location == "US", model == "FluSight-ensemble") %>% 
  select(-logvalue) %>% 
  mutate(month = month(forecast_date)) %>%
  group_by(month) %>%
  filter(forecast_date == min(forecast_date)) %>%
  ungroup() %>% 
  filter(forecast_date != min(forecast_date)) %>% 
  pivot_wider(names_from = c(type, quantile), values_from = value) %>% 
  ungroup()

ntl_ens_plt <-
  ggplot(ntl_ens_forecasts, aes(x = target_end_date, y = quantile_0.5))+
  geom_ribbon(aes(ymin = quantile_0.025, ymax = quantile_0.975, group = interaction(model, forecast_date),  alpha = "95%"),fill = "#3BBBB0")+
  geom_ribbon(aes(ymin = quantile_0.25, ymax = quantile_0.75, group = interaction(model, forecast_date), alpha = "50%"), fill = "#3BBBB0")+
  geom_line(aes(color = "Forecasted",group = interaction(model, forecast_date)))+
  geom_point(aes(color = "Forecasted", group = interaction(model, forecast_date)))+
  geom_line(data = filter(obs_data24, location == "US"), aes(x = target_end_date, y = report, color = "Observed"))+
  geom_point(data = filter(obs_data24, location == "US"), aes(x = target_end_date , y = report, color = "Observed"))+
  #  facet_grid(rows = vars(model))+
  scale_color_manual(values = c("Forecasted" = "#006166", "Observed" = "black"), name = "Weekly admissions")+
  scale_alpha_manual(values = c("95%" = .25, "50%" = .50), name = "Prediction Interval")+
  theme_bw()+
  scale_y_continuous(labels = scales::comma, name = "Weekly hospital admissions")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", name = NULL)+
    theme(legend.position = "inside", 
          legend.position.inside = c(.85,.7))

ntl_ens_plt
ggsave(paste0(dashboard_r_code,"/viz/figure1_ntl_ensv1.png"), plot = ntl_ens_plt, width=8, height=4.5) ## original 12 x 8, aspect ratio for web is 16:9

## Figure 1 csv 
ntl_ens_output <- ntl_ens_forecasts %>% 
  mutate(median = quantile_0.5,
         lower_50 = quantile_0.25, 
         upper_50 = quantile_0.75, 
         lower_95 = quantile_0.025,
         upper_95 = quantile_0.975) %>% 
  select(location, target, target_end_date, forecast_date, model, location_name, month, median, lower_50, upper_50, lower_95, upper_95) %>% 
  left_join(obs_data24, join_by(location, location_name, target_end_date)) %>% 
  rename(observed = report) %>% select(-logreport)

# write.csv(ntl_ens_output, paste0(dashboard_r_code, "/output_data/figure1_data.csv"), row.names = FALSE)

### Table 1 

inc.rankings_all <- mutate(inc.rankings_all24) %>% arrange(rel_wis)

inc.rankings_all %>% 
  left_join(model_metadata, join_by("model" == "model_name")) %>% 
  mutate(model_type = ifelse(ensemble_of_models == TRUE, paste("ens, ", model_type), model_type),
         display_name = paste0(model,"$^{", toupper(model_type), "}$")) %>% 
  rename(Model = display_name,
         `Absolute WIS` = wis,
         `Relative WIS`= rel_wis,
         `50% Coverage (%)` = cov_50,
         `95% Coverage (%)` = cov_95,
         `% of Forecasts Submitted`  = per_forecasts,
         `% of Locations Forecasted` = per_locations,
         `MAE` = mae) %>% 
  select(Model,  `Relative WIS`,
       `50% Coverage (%)`, 
         `95% Coverage (%)`, 
         `% of Forecasts Submitted`, 
         #           `Log Transformed Absolute WIS`,
         #           `Log Transformed Relative WIS`
  ) %>%
  mutate_if(is.numeric, round, digits = 2) %>%  
  mutate(`% of Forecasts Submitted` = round(`% of Forecasts Submitted`)) %>% 
  knitr::kable(align = c("lcccccccc"), 
               caption = "ENS indicates ensemble, 
               STAT indicates statistical components, 
               MECH is mechanistic,
               AI/ML indicates Artificial Intillegence or machine learning components") %>% 
  kableExtra::footnote( general_title = "") %>%
  kableExtra::kable_classic()

table1 <- inc.rankings_all %>% 
  left_join(model_metadata, join_by("model" == "model_name")) %>% 
  mutate(model_type = ifelse(ensemble_of_models == TRUE, paste("ens, ", model_type), model_type),
         display_name = paste0(model," (", toupper(model_type),")")) %>% 
  rename(Model = display_name,
         `Absolute WIS` = wis,
         `Relative WIS`= rel_wis,
         `50% Coverage (%)` = cov_50,
         `95% Coverage (%)` = cov_95,
         `% of Forecasts Submitted`  = per_forecasts,
         `% of Locations Forecasted` = per_locations,
         `MAE` = mae) %>% 
  select(Model,  `Relative WIS`,
         `50% Coverage (%)`, 
         `95% Coverage (%)`, 
         `% of Forecasts Submitted`, 
         #           `Log Transformed Absolute WIS`,
         #           `Log Transformed Relative WIS`
  ) %>%
  mutate_if(is.numeric, round, digits = 2) %>%  
  mutate(`% of Forecasts Submitted` = round(`% of Forecasts Submitted`)) %>% as.data.frame()

# write.csv(table1, paste0(dashboard_r_code, "/output_data/table1_data.csv"), row.names = FALSE)


WIS_and_coverage_24 <- WIS_Season24 %>%
  group_by(model, location_name) %>% 
  summarise(Percent.Cov.50 = mean(coverage.50, na.rm = TRUE), 
            Percent.Cov.95 = mean(coverage.95, na.rm = TRUE)) %>% 
  distinct() %>% 
  ungroup()


#scoring by location 
inc.rankings_location <- raw_scores24_log %>% 
  filter(location != "US") %>% 
  summarise_scores(by = c("model", "location_name"), relative_skill = TRUE,  baseline = "FluSight-baseline", na.rm = TRUE) %>% 
  mutate(wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, location_name, wis,rel_wis, mae) %>%
  group_by(location_name) %>%
  mutate(baseline_wis = wis[model == "FluSight-baseline"]) %>%
  ungroup() %>%
  mutate(below = ifelse(wis < baseline_wis & model != "FluSight-baseline", 1, 0)) %>% 
  select(-baseline_wis) %>% 
  left_join(WIS_and_coverage_24, by = join_by("model" == "model", "location_name" == "location_name"))


## scores table for figures and coverage
Scores_tab24 <- scores_tab_function(inc.rankings_location, inc.rankings_all24, WIS_Season24)


inc.rankings_all_nice <- inc.rankings_all24 %>% arrange(rel_wis) 

scores <- inc.rankings_location %>% filter(is.finite(rel_wis)) %>% 
  left_join(., y = inc.rankings_all_nice[,c("model")], by = c("model"))
scores_order <- inc.rankings_all_nice
levels_order <- scores_order$model

jurisdiction_scores <- scores %>% 
  group_by(location_name) %>% 
  summarise(mean_rel_wis = mean(rel_wis), median_rel_wis = median(rel_wis), 
            mean_cov_50 = mean(Percent.Cov.50), mean_cov_95 = mean(Percent.Cov.95)) %>% 
  arrange(median_rel_wis) %>% 
  mutate(location_name = fct_inorder(location_name)) %>% 
  ungroup()

location_order <- jurisdiction_scores$location_name

wis_location_plt <- ggplot(scores, 
                           aes(x = factor(model, levels = levels_order), 
                               y = factor(location_name, levels = location_order), 
                               fill = scales::oob_squish(rel_wis, range = c(- 2.584963, 2.584963)))) +
  geom_tile() +
  theme_bw()+
  geom_text(aes(label = signif(rel_wis, 2), color = ifelse(rel_wis <= 2, "black", "white")), size = 2.5) + 
  scale_color_manual(values = c( "black","white"))+
  scale_fill_gradient2(low ="#00a599", high =  "#9b007e", midpoint = 1, na.value = "grey50", 
                       name = "Relative WIS", 
                       breaks = c(-2,-1,0,1,2)) + 
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 7),
        title = element_text(size = 9)) +
  guides(color = "none")+
  scale_y_discrete(limits = rev) +
  theme(axis.ticks.y = element_blank())

wis_location_plt
# ggsave(paste0(dashboard_r_code,"/viz/figure2_relwis_location.png"), width=12, height=8, plot = wis_location_plt)


figure2_output <- scores %>% select(model, location_name, rel_wis) %>% 
  mutate(Jurisdiction = location_name, 
         model = factor(model, levels = scores_order$model)) %>% 
  arrange(model) %>% 
  pivot_wider(id_cols = Jurisdiction, names_from = model, values_from = rel_wis) 
# write.csv(figure2_output, paste0(dashboard_r_code, "/output_data/table2_data.csv"), row.names = FALSE)

figure2_stats <- scores %>% select(model, location_name, rel_wis) %>% 
  mutate(Jurisdiction = location_name, 
         model = factor(model, levels = scores_order$model)) %>% 
  group_by(model) %>% 
  summarise(difference = max(rel_wis,na.rm = TRUE) - min(rel_wis, na.rm = TRUE), 
            perc_under1 = sum(rel_wis <1)/n()) %>% 
  ungroup()

## Figure 3 
WIS_Season <- WIS_Season24 %>% rename("target_end_date" = "date", "WIS" = "wis")

coverage95_states <- WIS_Season %>% filter(location_name != "National") %>% 
  group_by(model, target_end_date, target) %>% 
  summarise(coverage95 = mean(coverage.95)) %>% 
  ungroup()

complete_df0 <- coverage95_states %>%
  filter(target == "0 wk inc flu hosp") %>% 
  select(model, target_end_date, target) %>%
  distinct() %>%
  complete(model, target_end_date, target)

complete_df1 <- coverage95_states %>%
  filter(target == "1 wk inc flu hosp") %>% 
  select(model, target_end_date, target) %>%
  distinct() %>%
  complete(model, target_end_date, target)

complete_df2 <- coverage95_states %>%
  filter(target == "2 wk inc flu hosp") %>% 
  select(model, target_end_date, target) %>%
  distinct() %>%
  complete(model, target_end_date, target)

complete_df3 <- coverage95_states %>%
  filter(target == "3 wk inc flu hosp") %>% 
  select(model, target_end_date, target) %>%
  distinct() %>%
  complete(model, target_end_date, target)

complete_df <- bind_rows(complete_df0, complete_df1, complete_df2, complete_df3) %>% arrange(model, target_end_date, target)

# Join with the original dataframe
result_df <- complete_df %>%
  left_join(coverage95_states, by = c("model", "target_end_date", "target")) %>%
  mutate(coverage95 = ifelse(is.na(coverage95), "No data", coverage95))

# write.csv(result_df, paste0(dashboard_r_code, "/output_data/figure2_data.csv"), row.names = FALSE)

coverage95_flusight <- coverage95_states %>% filter(model == "FluSight-ensemble") 
coverage95_not_flusight <- coverage95_states %>% filter(model != "FluSight-ensemble") 

coverage_labels <- as_labeller(c(`0 wk inc flu hosp` = "0 Week Ahead",
                                 `1 wk inc flu hosp` = "1 Week Ahead", 
                                 `2 wk inc flu hosp` = "2 Week Ahead",
                                 `3 wk inc flu hosp` = "3 Week Ahead"))



coverage_95_plt <-
  ggplot(coverage95_flusight, aes(x = target_end_date, 
                                  y = coverage95, group = model,
                                  col = model)) +
  geom_line(data = coverage95_not_flusight, aes(x = target_end_date, y = coverage95, group = model, color = "model")) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 2) +
  geom_hline(aes(yintercept = 0.95), linetype = 2)+
  labs(y = "95% Coverage", x = "", color = "Model") +
  scale_color_manual(values = c("#00a599", adjustcolor("grey50", .35)),
                     labels = c("FluSight-ensemble", "Contributed Models"),
                     drop = FALSE) +
  theme_bw()+
  scale_x_date(breaks = seq.Date(from = min(coverage95_flusight$target_end_date), to= max(coverage95_flusight$target_end_date), by = "2 weeks"), 
               date_labels = "%d %b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank(), 
        legend.position = "right")+
  facet_wrap(facets = vars(target), labeller = coverage_labels, scales = "free_x")

coverage_95_plt
# ggsave(paste0(dashboard_r_code,"/viz/figure3_coverage95.png"), width=10, height=8, plot = coverage_95_plt)



