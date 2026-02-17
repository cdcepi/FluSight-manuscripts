##################################################################
# The purpose of the below code is to implement a weighted ensemble
# method based on the performance of the models in the prior 6 weeks
# This is based on what was previously done for COVID-19
#See https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9247236/#b11 

#This is a combination of CDC code and code from UMASS collaborators (Evan Ray, Li Shandross)

#Load in the packages
library(covidHubUtils)
library(dplyr)
library(tidyverse)
library(zoltr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)
#library(CombineDistributions)
library(scoringutils)

#This is the date of the forecast of interest. Will use the prior 6 weeks of forecast to evaluate the best
#performing teams

#Load functions needed below
source("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/evaluation_functions.R")
source("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/as_covid_hub_forecasts.R")
ensemble_code_path = paste0("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble")

flu_baseline_all<-readRDS("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/flu_baseline_all.rds")
flu_truth_all<-readRDS("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/flu_truth_all.rds")
#Load in the truth data as of each forecast date for the burn-in period evaluation
source("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/list_as_of_data.R")
#Connecting to old Flusight data
flusight_path = paste0("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/Flusight-forecast-data-master") 
setwd(flusight_path)

#Create dates of interest
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(6:19) #Starting 6 weeks later than the start of forecast (burn period)

#Empty dataframes for below
#For scores
weighted_US_score_all<-data.frame()
weighted_state_score_all<-data.frame()
#For forecasts
weighted_state_forecasts_all<-data.frame()
weighted_US_forecasts_all<-data.frame()

weighted_US_weights<-data.frame()
weighted_state_weights<-data.frame()


###########################################################################################
#Weighting component here:


# Define a function for the sigmoid transformation. The theta is what I interpret as a scaling parameter
#see section 2.6 from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9247236/
#When theta is 0, the forecasts all get an equal weight. The larger theta is, the greater the weight 
#the better performing forecasts get. All of this code functions but this scaling parameter is the 
#main concern (does it function properly, is there a different way to do this, etc.)


sigmoid <- function(x,theta) {
  return(exp(x*theta))
}
#Setting values to search across. Cutting off at 5 because no monotonically increasing ensembles are produced when the 
#value is greater than 5
theta<-seq(0,100,0.1)
#Weighting component ends.
###################################################################################################################

#This is the first of multiple for loops

#This first loop establishes the "current date" as if we were creating the ensemble in the past

for (j in 1:length(flu_dates_22_23)) {
  forecast_date_current<-flu_dates_22_23[j]
  
#This the period of time we will use to evalute performance
flu_dates_22_23_retro <- as.Date(forecast_date_current) - weeks(1:6)
forecasts_6week<-data.frame()

#Another loop across the lookback/burn-in/traing period
for (i in 1:length(flu_dates_22_23_retro)) {
  forecast_date<-flu_dates_22_23_retro[i]
  
  #Start of a bunch of code previously developed by ARM to pull all the component forecasts
  output_dir <- paste0(ensemble_code_path, "/", forecast_date, "/")
  
  if(!dir.exists(output_dir)){
    dir.create(path = output_dir)
  }
  
  # Get the models to be included in the ensemble
  if(!file.exists(paste0(output_dir, "models-to-include-in-ensemble-retro-", forecast_date, ".csv"))){
    file_names = list.files(path = paste0(flusight_path, "/data-forecasts"))
    all_models = file_names[!(file_names %in% c("Flusight-ensemble")) &
                              !grepl(paste0(".md", collapse = "|"), file_names)]
    all_metadata = paste0(flusight_path, "/data-forecasts/", all_models,
                          "/metadata-", all_models, ".txt") %>%
      lapply(., read.delim)
    include <- c()
    for(i in 1:length(all_models)){
      
      metadata = all_metadata[[i]]
      
      # this checks to see that this week's file is in the model directory and
      # that it is a designated primary, secondary, or proposed model in the metadata
      if(file.exists(paste0(flusight_path, "/data-forecasts/", all_models[i], 
                            "/", forecast_date, "-", all_models[i], ".csv")) &
         (colSums("team_model_designation: primary" == metadata) +
          colSums("team_model_designation: proposed" == metadata) +
          colSums("team_model_designation: secondary" == metadata) > 0)){
        include = c(include, all_models[i])
      }
    }
    #List of functions to include in the retrospective evaluation
    write.csv(data.frame(model = include),paste0(output_dir, "models-to-include-in-ensemble-retro-", forecast_date, ".csv"))
  }
  
  # We start by loading the forecasts of weekly incident hospitalizations from dplyr::selected models
  eligible_models = read.csv(paste0(output_dir, "models-to-include-in-ensemble-retro-", forecast_date, ".csv"),
                             header = TRUE)
  models =as.character(eligible_models$model)
  
  
  #Read in forecast data
  
  forecast_data <- load_forecasts_repo(
    file_path = paste0(flusight_path, "/data-forecasts/"),
    models = models,
    targets = c(paste(1:4, "wk ahead inc flu hosp")),
    forecast_dates = forecast_date,
    hub = "FluSight",
    types = "quantile")%>%
    dplyr::rename(full_location_name = location_name) %>%
    mutate(full_location_name = case_when(location == "US" ~ "United States",
                                          location != "US" ~ full_location_name))
  

  #Remove any that dont have quantiles
  forecast_data<-forecast_data%>%filter(is.na(forecast_data$quantile)==F)
  
  forecasts_6week<-rbind(forecasts_6week,forecast_data)
}  

#Doing this work separately for the US overall and then all the states and territories
flu_truth_us<-list_as_of[[j+5]]%>%filter(location=='US')
flu_truth_states<-list_as_of[[j+5]]%>%filter(location!='US')

flu_truth_us$target_end_date<-as.Date(flu_truth_us$target_end_date)
flu_truth_states$target_end_date<-as.Date(flu_truth_states$target_end_date)

forecasts_6week$temporal_resolution<-'week'

#Preparing for the ensemble/scoring functions
  task_id_cols <- c("forecast_date", "location", "horizon", "target_variable", "target_end_date")
 
  forecasts_6week<-forecasts_6week%>%dplyr::rename(model_id=model,output_type=type,output_type_id=quantile )
  forecasts_6week2<-as_covid_hub_forecasts(forecasts_6week)
  
  #wouldn't have truth data from the future, so limiting evaluation to only those forecast were we have data
  #as of the current simulated date
  forecasts_6week3<-forecasts_6week2%>%mutate(days_off=forecast_date_current-forecast_date)%>% 
    filter((horizon==4 & days_off>=28) |(horizon==3 & days_off>=21)|(horizon==2 & days_off>=14)|(horizon==1 & days_off>=7))
#sub-setting to 4 week forecast
  forecasts_4week<- forecasts_6week3%>% filter((forecast_date_current-forecast_date)/7 <= 4)   
#sub-setting to 2 week forecast
  forecasts_2week<- forecasts_6week3%>% filter((forecast_date_current-forecast_date)/7 <= 2)  

#Splitting the national forecasts from state specific ones
  #6 week burn
  forecasts_6week_us<-forecasts_6week3%>%filter(location=='US')
  forecasts_6week_state<-forecasts_6week3%>%filter(location!='US')
  #4 week burn
  forecasts_4week_us<-forecasts_4week%>%filter(location=='US')
  forecasts_4week_state<-forecasts_4week%>%filter(location!='US')
  #2 week burn
  forecasts_2week_us<-forecasts_2week%>%filter(location=='US')
  forecasts_2week_state<-forecasts_2week%>%filter(location!='US')

#Now will subset to only those models that have the correct number of forecasts for the given week
#Unable to effectively score the models if they are missing a week
#Will do this separately for each burn period and separately for US vs states
  count_us6<-forecasts_6week_us%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  count_state6<-forecasts_6week_state%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  
  count_us4<-forecasts_4week_us%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  count_state4<-forecasts_4week_state%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  
  count_us2<-forecasts_2week_us%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  count_state2<-forecasts_2week_state%>%group_by( model)%>%
    summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")
  
     #6 week burn
  forecasts_6week_us<-forecasts_6week_us%>%right_join(count_us6)%>%
    dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  forecasts_6week_state<-forecasts_6week_state%>%right_join(count_state6)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  #4 week burn
  forecasts_4week_us<-forecasts_4week_us%>%right_join(count_us4)%>%
    dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  forecasts_4week_state<-forecasts_4week_state%>%right_join(count_state4)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  #2 week burn
  forecasts_2week_us<-forecasts_2week_us%>%right_join(count_us2)%>%
    dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  forecasts_2week_state<-forecasts_2week_state%>%right_join(count_state2)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%rename(prediction=value)%>%filter(!is.na(true_value))
  
  #Evaluate the forecasts
  score_6week_eval_us <- forecasts_6week_us %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_6week_eval_state <-forecasts_6week_state %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_4week_eval_us <- forecasts_4week_us %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_4week_eval_state <- forecasts_4week_state  %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_2week_eval_us <- forecasts_2week_us  %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_2week_eval_state <- forecasts_2week_state  %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  


#Bring in the forecasts for the "current" date

# Get the models to be included in the ensemble
if(!file.exists(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"))){
  file_names = list.files(path = paste0(flusight_path, "/data-forecasts"))
  all_models = file_names[!(file_names %in% c("Flusight-baseline", "Flusight-ensemble")) &
                            !grepl(paste0(".md", collapse = "|"), file_names)]
  all_metadata = paste0(flusight_path, "/data-forecasts/", all_models,
                        "/metadata-", all_models, ".txt") %>%
    lapply(., read.delim)
  include <- c()
  for(i in 1:length(all_models)){
    
    metadata = all_metadata[[i]]
    
    # this checks to see that this week's file is in the model directory and
    # that it is a designated primary, secondary, or proposed model in the metadata
    if(file.exists(paste0(flusight_path, "/data-forecasts/", all_models[i], 
                          "/", forecast_date_current, "-", all_models[i], ".csv")) &
       (colSums("team_model_designation: primary" == metadata) +
        colSums("team_model_designation: proposed" == metadata) +
        colSums("team_model_designation: secondary" == metadata) > 0)){
      include = c(include, all_models[i])
    }
  }
  write.csv(data.frame(model = include),paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"))
}

# We start by loading the forecasts of weekly incident hospitalizations from selected models
eligible_models = read.csv(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"),
                           header = TRUE)
models =as.character(eligible_models$model)


#Read in forecast data

forecast_data <- load_forecasts_repo(
  file_path = paste0(flusight_path, "/data-forecasts/"),
  models = models,
  targets = c(paste(1:4, "wk ahead inc flu hosp")),
  forecast_dates = forecast_date_current,
  hub = "FluSight",
  types = "quantile")%>%
  dplyr::rename(full_location_name = location_name) %>%
  mutate(full_location_name = case_when(location == "US" ~ "United States",
                                        location != "US" ~ full_location_name))


forecast_data$model_id<-forecast_data$model
forecast_data$output_type<-forecast_data$type
forecast_data$output_type_id<-forecast_data$quantile

forecast_data_us<-forecast_data%>%filter(location=='US')
forecast_data_state<-forecast_data%>%filter(location!='US')
#Done reading in the "current data"


#Limiting the weight calculation to those that are actually included this week's forecasts
models_current_us<-as.data.frame(unique(forecast_data_us$model_id))%>%rename(model=`unique(forecast_data_us$model_id)`)
models_current_state<-as.data.frame(unique(forecast_data_state$model_id))%>%rename(model=`unique(forecast_data_state$model_id)`)

score_6week_eval_us<-score_6week_eval_us%>%inner_join(models_current_us)
score_4week_eval_us<-score_4week_eval_us%>%inner_join(models_current_us)
score_2week_eval_us<-score_2week_eval_us%>%inner_join(models_current_us)

score_6week_eval_state<-score_6week_eval_state%>%inner_join(models_current_state)
score_4week_eval_state<-score_4week_eval_state%>%inner_join(models_current_state)
score_2week_eval_state<-score_2week_eval_state%>%inner_join(models_current_state)

forecast_weight_us6<-right_join(forecast_data_us,score_6week_eval_us) 
forecast_weight_state6<-right_join(forecast_data_state,score_6week_eval_state)

forecast_weight_us4<-right_join(forecast_data_us,score_4week_eval_us)
forecast_weight_state4<-right_join(forecast_data_state,score_4week_eval_state)

forecast_weight_us2<-right_join(forecast_data_us,score_2week_eval_us)
forecast_weight_state2<-right_join(forecast_data_state,score_2week_eval_state)


#Creating a grid search for the optimal parameter to scale the weights
#Need to do this grid search separately for each ensemble we are investigating 

train_search_overall<-data.frame()
####################################################################################################################
#Weighting component here:
#6 Week burn, US overall
for(q in 1:length(theta)){
#Weights  
  score_6week_eval_us$unorm_weights <- sigmoid(-score_6week_eval_us$rel_wis,theta[q])
  score_6week_eval_us$weight <- score_6week_eval_us$unorm_weights / sum(score_6week_eval_us$unorm_weights)
  weights_us6<-score_6week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  
  if(max(weights_us6$weight) > 0.3){

    break()
  } else{
    #Need to select a theta that minimizes the WIS over the training window.
    
    #Forecasts to include
    check_WIS<-forecasts_6week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
      mutate(output_type=type)
    #Keep only those with weights
    check_WIS<-check_WIS%>%right_join(weights_us6)%>%select(!weight)
    
    #Calculate weighted median
    median_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us6,
                                             agg_fun = "median",
                                             model_id="Weighted Median train US6",
                                             task_id_cols = task_id_cols)
    
    
    median_weight_us_train$temporal_resolution<-'week'
    #Check to make sure that quantiles monotonically increase
    median_weight_us_train2<-median_weight_us_train%>%
      mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                          ifelse(location==lag(location)&horizon==lag(horizon) &
                                   value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
    median_weight_us_train2$check[1]<-1
    
    #If else statement to ignore those functions that are non-monotonically increasing 
    if(is.na(sum(median_weight_us_train2$check))){
      print('ERROR- Nonmonotic function')
    } else{
      #Calculate WIS
      median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
        dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                         by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
        rename(model=model_id, quantile=output_type_id)
      
      
      train_wis <- median_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
        add_coverage(ranges = c(50, 95), by = c("model")) %>%
        summarise_scores(by = c("model"))%>%
        mutate(cov_50=round(coverage_50*100,2),
               cov_95=round(coverage_95*100,2),
               wis=round(interval_score,2),
               mae=round(ae_median,2))%>%
        select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
      
      
      train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
      
    }
        }

}

#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value actually
#produces monotonically increasing quantiles. If not, loop back and use the next best fitting value
for(k in 1:length(theta_us6)){
#There are normalized weights
weights_us6 <- as.data.frame(cbind(score_6week_eval_us$model,
                                   sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k])/
                                     sum(sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k]))
                            ))
colnames(weights_us6)<-c('model_id','weight')

median_weight_us6<- simple_ensemble(forecast_weight_us6,weights=weights_us6,
                                    agg_fun = "median",
                                    model_id="Weighted Median ensemble US6",
                                    task_id_cols = task_id_cols)  
  
median_weight_us6<-median_weight_us6%>%
  mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                        value>=lag(value) & forecast_date==lag(forecast_date),1,
                      ifelse(location==lag(location)&horizon==lag(horizon) &
                               value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
median_weight_us6$check[1]<-1 

if(is.na(sum(median_weight_us6$check))==F){
  print('Done')
  median_weight_us6<-median_weight_us6%>%select(!check)
  
  model_id="Weighted Median ensemble US6"
  theta_keep=theta_us6[k]
    weight_info6<-cbind(weights_us6,forecast_date_current,model_id,theta_keep)
  
    break()
} else{
  #Calculate WIS
  print('ERROR- Nonmonotic function')
}
}
################################################################################
#Stop here
################################################################################

#Now the code will just repeat with minor variations based on the length of the burn period. I haven't turned this whole
#process into a function yet

#4 Week burn, US overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_4week_eval_us$unorm_weights <- sigmoid(-score_4week_eval_us$rel_wis,theta[q])
  score_4week_eval_us$weight <- score_4week_eval_us$unorm_weights / sum(score_4week_eval_us$unorm_weights)
  weights_us4<-score_4week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_us4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us4)%>%select(!weight)
  
  #Calculate weighted median
  median_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us4,
                                           agg_fun = "median",
                                           model_id="Weighted Median train US4",
                                           task_id_cols = task_id_cols)
  
  
  median_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  
  median_weight_us_train2<-median_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- median_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
    
  }
}
}
#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us4<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us4)){
  #There are normalized weights
  weights_us4 <- as.data.frame(cbind(score_4week_eval_us$model,
                                     sigmoid(-score_4week_eval_us$rel_wis,theta_us4[k])/
                                       sum(sigmoid(-score_4week_eval_us$rel_wis,theta_us4[k]))
  ))
  colnames(weights_us4)<-c('model_id','weight')
  
  median_weight_us4<- simple_ensemble(forecast_weight_us4,weights=weights_us4,
                                      agg_fun = "median",
                                      model_id="Weighted Median ensemble US4",
                                      task_id_cols = task_id_cols)  
  
  median_weight_us4<-median_weight_us4%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_us4$check[1]<-1 
  
  if(is.na(sum(median_weight_us4$check))==F){
    print('Done')
    median_weight_us4<-median_weight_us4%>%select(!check)
    model_id="Weighted Median ensemble US4"
    theta_keep=theta_us4[k]
    
    weight_info4<-cbind(weights_us4,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}



#2 Week burn, US overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_2week_eval_us$unorm_weights <- sigmoid(-score_2week_eval_us$rel_wis,theta[q])
  score_2week_eval_us$weight <- score_2week_eval_us$unorm_weights / sum(score_2week_eval_us$unorm_weights)
  weights_us2<-score_2week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_us2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us2)%>%select(!weight)
  
  #Calculate weighted median
  median_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us2,
                                           agg_fun = "median",
                                           model_id="Weighted Median train US2",
                                           task_id_cols = task_id_cols)
  
  
  median_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  median_weight_us_train2<-median_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- median_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us2<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us2)){
  #There are normalized weights
  weights_us2 <- as.data.frame(cbind(score_2week_eval_us$model,
                                     sigmoid(-score_2week_eval_us$rel_wis,theta_us2[k])/
                                       sum(sigmoid(-score_2week_eval_us$rel_wis,theta_us2[k]))
  ))
  colnames(weights_us2)<-c('model_id','weight')
  
  median_weight_us2<- simple_ensemble(forecast_weight_us2,weights=weights_us2,
                                      agg_fun = "median",
                                      model_id="Weighted Median ensemble US2",
                                      task_id_cols = task_id_cols)  
  
  median_weight_us2<-median_weight_us2%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_us2$check[1]<-1 
  
  if(is.na(sum(median_weight_us2$check))==F){
    print('Done')
    median_weight_us2<-median_weight_us2%>%select(!check)
    model_id="Weighted Median ensemble US2"
    theta_keep=theta_us2[k]
    
    weight_info2<-cbind(weights_us2,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}

#6 Week burn, states
########################################
# State now
##########################################


#6 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
#Weights  
  score_6week_eval_state$unorm_weights <- sigmoid(-score_6week_eval_state$rel_wis,theta[q])
  score_6week_eval_state$weight <- score_6week_eval_state$unorm_weights / sum(score_6week_eval_state$unorm_weights)
  weights_state6<-score_6week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state6$weight) > 0.3){
    
    break()
  } else{
#Need to select a theta that minimizes the WIS over the training window.

#Forecasts to include
check_WIS<-forecasts_6week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
  mutate(output_type=type)%>%mutate(output_type_id=quantile)
#Keep only those with weights
check_WIS<-check_WIS%>%right_join(weights_state6)%>%select(!weight)

#Calculate weighted median
  median_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state6,
                                         agg_fun = "median",
                                         model_id="Weighted Median train state6",
                                         task_id_cols = task_id_cols)

  
  median_weight_state_train$temporal_resolution<-'week'
#Check to make sure that quantiles monotonically increase
   median_weight_state_train2<-median_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_state_train2$check[1]<-1
  
#If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
#Calculate WIS
    #Calculate WIS
    median_weight_state_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- median_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state6)){
#There are normalized weights
weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model,
                                   sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k])/
                                     sum(sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k]))
                            ))
colnames(weights_state6)<-c('model_id','weight')

median_weight_state6<- simple_ensemble(forecast_weight_state6,weights=weights_state6,
                                    agg_fun = "median",
                                    model_id="Weighted Median ensemble state6",
                                    task_id_cols = task_id_cols)  
  
median_weight_state6<-median_weight_state6%>%
  mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                        value>=lag(value) & forecast_date==lag(forecast_date),1,
                      ifelse(location==lag(location)&horizon==lag(horizon) &
                               value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
median_weight_state6$check[1]<-1 

if(is.na(sum(median_weight_state6$check))==F){
  print('Done')
  median_weight_state6<-median_weight_state6%>%select(!check)
  model_id="Weighted Median ensemble state6"
  theta_keep=theta_state6[k]
  
  weight_info6_state<-cbind(weights_state6,forecast_date_current,model_id,theta_keep)
    break()
} else{
  #Calculate WIS
  print('ERROR- Nonmonotic function')
}
}


#4 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_4week_eval_state$unorm_weights <- sigmoid(-score_4week_eval_state$rel_wis,theta[q])
  score_4week_eval_state$weight <- score_4week_eval_state$unorm_weights / sum(score_4week_eval_state$unorm_weights)
  weights_state4<-score_4week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_state%>%filter(model!="Flstateight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state4)%>%select(!weight)
  
  #Calculate weighted median
  median_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state4,
                                           agg_fun = "median",
                                           model_id="Weighted Median train state4",
                                           task_id_cols = task_id_cols)
  
  
  median_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  median_weight_state_train2<-median_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    median_weight_state_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- median_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state4<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state4)){
  #There are normalized weights
  weights_state4 <- as.data.frame(cbind(score_4week_eval_state$model,
                                     sigmoid(-score_4week_eval_state$rel_wis,theta_state4[k])/
                                       sum(sigmoid(-score_4week_eval_state$rel_wis,theta_state4[k]))
  ))
  colnames(weights_state4)<-c('model_id','weight')
  
  median_weight_state4<- simple_ensemble(forecast_weight_state4,weights=weights_state4,
                                      agg_fun = "median",
                                      model_id="Weighted Median ensemble state4",
                                      task_id_cols = task_id_cols)  
  
  median_weight_state4<-median_weight_state4%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_state4$check[1]<-1 
  
  if(is.na(sum(median_weight_state4$check))==F){
    print('Done')
    median_weight_state4<-median_weight_state4%>%select(!check)
    model_id="Weighted Median ensemble state4"
    theta_keep=theta_state4[k]
    
    weight_info4_state<-cbind(weights_state4,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}



#2 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_2week_eval_state$unorm_weights <- sigmoid(-score_2week_eval_state$rel_wis,theta[q])
  score_2week_eval_state$weight <- score_2week_eval_state$unorm_weights / sum(score_2week_eval_state$unorm_weights)
  weights_state2<-score_2week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_state%>%filter(model!="Flstateight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state2)%>%select(!weight)
  
  #Calculate weighted median
  median_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state2,
                                           agg_fun = "median",
                                           model_id="Weighted Median train state2",
                                           task_id_cols = task_id_cols)
  
  
  median_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  median_weight_state_train2<-median_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    median_weight_state_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- median_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state2<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state2)){
  #There are normalized weights
  weights_state2 <- as.data.frame(cbind(score_2week_eval_state$model,
                                     sigmoid(-score_2week_eval_state$rel_wis,theta_state2[k])/
                                       sum(sigmoid(-score_2week_eval_state$rel_wis,theta_state2[k]))
  ))
  colnames(weights_state2)<-c('model_id','weight')
  
  median_weight_state2<- simple_ensemble(forecast_weight_state2,weights=weights_state2,
                                      agg_fun = "median",
                                      model_id="Weighted Median ensemble state2",
                                      task_id_cols = task_id_cols)  
  
  median_weight_state2<-median_weight_state2%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  median_weight_state2$check[1]<-1 
  
  if(is.na(sum(median_weight_state2$check))==F){
    print('Done')
    median_weight_state2<-median_weight_state2%>%select(!check)
    model_id="Weighted Median ensemble state2"
    theta_keep=theta_state2[k]
    
    weight_info2_state<-cbind(weights_state2,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}

############################################
#Now adding mean, so same process as above for the median
###########################################

train_search_overall<-data.frame()

#6 Week burn, US overall
for(q in 1:length(theta)){
  #Weights  
  score_6week_eval_us$unorm_weights <- sigmoid(-score_6week_eval_us$rel_wis,theta[q])
  score_6week_eval_us$weight <- score_6week_eval_us$unorm_weights / sum(score_6week_eval_us$unorm_weights)
  weights_us6<-score_6week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_us6$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_6week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us6)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us6,
                                           agg_fun = "mean",
                                           model_id="Weighted mean train US6",
                                           task_id_cols = task_id_cols)
  
  
  mean_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_us_train2<-mean_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    #Calculate WIS
   mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <-mean_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us6)){
  #There are normalized weights
  weights_us6 <- as.data.frame(cbind(score_6week_eval_us$model,
                                     sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k])/
                                       sum(sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k]))
  ))
  colnames(weights_us6)<-c('model_id','weight')
  
  mean_weight_us6<- simple_ensemble(forecast_weight_us6,weights=weights_us6,
                                      agg_fun = "mean",
                                      model_id="Weighted mean ensemble US6",
                                      task_id_cols = task_id_cols)  
  
  mean_weight_us6<-mean_weight_us6%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us6$check[1]<-1 
  
  if(is.na(sum(mean_weight_us6$check))==F){
    print('Done')
    mean_weight_us6<-mean_weight_us6%>%select(!check)
    model_id="Weighted mean ensemble US6"
    theta_keep=theta_us6[k]
    
    weight_info6_mean<-cbind(weights_us6,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}


#4 Week burn, US overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_4week_eval_us$unorm_weights <- sigmoid(-score_4week_eval_us$rel_wis,theta[q])
  score_4week_eval_us$weight <- score_4week_eval_us$unorm_weights / sum(score_4week_eval_us$unorm_weights)
  weights_us4<-score_4week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_us4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us4)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us4,
                                           agg_fun = "mean",
                                           model_id="Weighted mean train US4",
                                           task_id_cols = task_id_cols)
  
  
  mean_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_us_train2<-mean_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
    
  } else{
    #Calculate WIS
    #Calculate WIS
    mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- mean_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us4<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us4)){
  #There are normalized weights
  weights_us4 <- as.data.frame(cbind(score_4week_eval_us$model,
                                     sigmoid(-score_4week_eval_us$rel_wis,theta_us4[k])/
                                       sum(sigmoid(-score_4week_eval_us$rel_wis,theta_us4[k]))
  ))
  colnames(weights_us4)<-c('model_id','weight')
  
  mean_weight_us4<- simple_ensemble(forecast_weight_us4,weights=weights_us4,
                                      agg_fun = "mean",
                                      model_id="Weighted mean ensemble US4",
                                      task_id_cols = task_id_cols)  
  
  mean_weight_us4<-mean_weight_us4%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us4$check[1]<-1 
  
  if(is.na(sum(mean_weight_us4$check))==F){
    print('Done')
    mean_weight_us4<-mean_weight_us4%>%select(!check)
    model_id="Weighted mean ensemble US4"
    theta_keep=theta_us4[k]
    
    weight_info4_mean<-cbind(weights_us4,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}



#2 Week burn, US overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_2week_eval_us$unorm_weights <- sigmoid(-score_2week_eval_us$rel_wis,theta[q])
  score_2week_eval_us$weight <- score_2week_eval_us$unorm_weights / sum(score_2week_eval_us$unorm_weights)
  weights_us2<-score_2week_eval_us[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_us2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us2)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us2,
                                           agg_fun = "mean",
                                           model_id="Weighted mean train US2",
                                           task_id_cols = task_id_cols)
  
  
  mean_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_us_train2<-mean_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- mean_weight_us_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then use this to loop over the calculation of the model
theta_us2<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us2)){
  #There are normalized weights
  weights_us2 <- as.data.frame(cbind(score_2week_eval_us$model,
                                     sigmoid(-score_2week_eval_us$rel_wis,theta_us2[k])/
                                       sum(sigmoid(-score_2week_eval_us$rel_wis,theta_us2[k]))
  ))
  colnames(weights_us2)<-c('model_id','weight')
  
  mean_weight_us2<- simple_ensemble(forecast_weight_us2,weights=weights_us2,
                                      agg_fun = "mean",
                                      model_id="Weighted mean ensemble US2",
                                      task_id_cols = task_id_cols)  
  
  mean_weight_us2<-mean_weight_us2%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_us2$check[1]<-1 
  
  if(is.na(sum(mean_weight_us2$check))==F){
    print('Done')
    mean_weight_us2<-mean_weight_us2%>%select(!check)
    model_id="Weighted mean ensemble US2"
    theta_keep=theta_us2[k]
    
    weight_info2_mean<-cbind(weights_us2,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}

#6 Week burn, states
########################################
# State now
##########################################


#6 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_6week_eval_state$unorm_weights <- sigmoid(-score_6week_eval_state$rel_wis,theta[q])
  score_6week_eval_state$weight <- score_6week_eval_state$unorm_weights / sum(score_6week_eval_state$unorm_weights)
  weights_state6<-score_6week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state6$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_6week_state%>%filter(model!="Flstateight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state6)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state6,
                                              agg_fun = "mean",
                                              model_id="Weighted mean train state6",
                                              task_id_cols = task_id_cols)
  
  
  mean_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_state_train2<-mean_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- mean_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state6)){
  #There are normalized weights
  weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model,
                                        sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k])/
                                          sum(sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k]))
  ))
  colnames(weights_state6)<-c('model_id','weight')
  
  mean_weight_state6<- simple_ensemble(forecast_weight_state6,weights=weights_state6,
                                         agg_fun = "mean",
                                         model_id="Weighted mean ensemble state6",
                                         task_id_cols = task_id_cols)  
  
  mean_weight_state6<-mean_weight_state6%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state6$check[1]<-1 
  
  if(is.na(sum(mean_weight_state6$check))==F){
    print('Done')
    mean_weight_state6<-mean_weight_state6%>%select(!check)
    model_id="Weighted mean ensemble state6"
    theta_keep=theta_state6[k]
    
    weight_info6_mean_state<-cbind(weights_state6,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}


#4 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_4week_eval_state$unorm_weights <- sigmoid(-score_4week_eval_state$rel_wis,theta[q])
  score_4week_eval_state$weight <- score_4week_eval_state$unorm_weights / sum(score_4week_eval_state$unorm_weights)
  weights_state4<-score_4week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_state%>%filter(model!="Flstateight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)%>%mutate(output_type_id=quantile)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state4)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state4,
                                              agg_fun = "mean",
                                              model_id="Weighted mean train state4",
                                              task_id_cols = task_id_cols)
  
  
  mean_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_state_train2<-mean_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- mean_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state4<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state4)){
  #There are normalized weights
  weights_state4 <- as.data.frame(cbind(score_4week_eval_state$model,
                                        sigmoid(-score_4week_eval_state$rel_wis,theta_state4[k])/
                                          sum(sigmoid(-score_4week_eval_state$rel_wis,theta_state4[k]))
  ))
  colnames(weights_state4)<-c('model_id','weight')
  
  mean_weight_state4<- simple_ensemble(forecast_weight_state4,weights=weights_state4,
                                         agg_fun = "mean",
                                         model_id="Weighted mean ensemble state4",
                                         task_id_cols = task_id_cols)  
  
  mean_weight_state4<-mean_weight_state4%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state4$check[1]<-1 
  
  if(is.na(sum(mean_weight_state4$check))==F){
    print('Done')
    mean_weight_state4<-mean_weight_state4%>%select(!check)
    model_id="Weighted mean ensemble state4"
    theta_keep=theta_state4[k]
    
    weight_info4_mean_state<-cbind(weights_state4,forecast_date_current,model_id,theta_keep)
    
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}



#2 Week burn, state overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_2week_eval_state$unorm_weights <- sigmoid(-score_2week_eval_state$rel_wis,theta[q])
  score_2week_eval_state$weight <- score_2week_eval_state$unorm_weights / sum(score_2week_eval_state$unorm_weights)
  weights_state2<-score_2week_eval_state[,c('model','weight')]%>%rename(model_id=model)
  if(max(weights_state2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_state%>%filter(model!="Flstateight-baseline")%>%mutate(model_id=model, output_type_id=quantile, value=prediction)%>%
    mutate(output_type=type)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state2)%>%select(!weight)
  
  #Calculate weighted mean
  mean_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state2,
                                              agg_fun = "mean",
                                              model_id="Weighted mean train state2",
                                              task_id_cols = task_id_cols)
  
  
  mean_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  mean_weight_state_train2<-mean_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%rename(prediction=value)%>%
      rename(model=model_id, quantile=output_type_id)
    
    
    train_wis <- mean_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
      add_coverage(ranges = c(50, 95), by = c("model")) %>%
      summarise_scores(by = c("model"))%>%
      mutate(cov_50=round(coverage_50*100,2),
             cov_95=round(coverage_95*100,2),
             wis=round(interval_score,2),
             mae=round(ae_median,2))%>%
      select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
    
    
    train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis)))
    
  }
}
}
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state2<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state2)){
  #There are normalized weights
  weights_state2 <- as.data.frame(cbind(score_2week_eval_state$model,
                                        sigmoid(-score_2week_eval_state$rel_wis,theta_state2[k])/
                                          sum(sigmoid(-score_2week_eval_state$rel_wis,theta_state2[k]))
  ))
  colnames(weights_state2)<-c('model_id','weight')
  
  mean_weight_state2<- simple_ensemble(forecast_weight_state2,weights=weights_state2,
                                         agg_fun = "mean",
                                         model_id="Weighted mean ensemble state2",
                                         task_id_cols = task_id_cols)  
  
  mean_weight_state2<-mean_weight_state2%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                          value>=lag(value) & forecast_date==lag(forecast_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& forecast_date==lag(forecast_date),NA,0)))
  mean_weight_state2$check[1]<-1 
  
  if(is.na(sum(mean_weight_state2$check))==F){
    print('Done')
    mean_weight_state2<-mean_weight_state2%>%select(!check)
    model_id="Weighted mean ensemble state2"
    theta_keep=theta_state2[k]
    
    weight_info2_mean_state<-cbind(weights_state2,forecast_date_current,model_id,theta_keep)
    break()
  } else{
    #Calculate WIS
    print('ERROR- Nonmonotic function')
  }
}

mean_weight_state2$temporal_resolution<-'week'
mean_weight_us2$temporal_resolution<-'week'

mean_weight_state4$temporal_resolution<-'week'
mean_weight_us4$temporal_resolution<-'week'

mean_weight_state6$temporal_resolution<-'week'
mean_weight_us6$temporal_resolution<-'week'


median_weight_state2$temporal_resolution<-'week'
median_weight_us2$temporal_resolution<-'week'

median_weight_state4$temporal_resolution<-'week'
median_weight_us4$temporal_resolution<-'week'

median_weight_state6$temporal_resolution<-'week'
median_weight_us6$temporal_resolution<-'week'


flu_baseline_current<-flu_baseline_all%>%filter(forecast_date==forecast_date_current &location=='US')%>%
  dplyr::rename(model_id=model, output_type=type, output_type_id=quantile)

flu_baseline_current_state<-flu_baseline_all%>%filter(forecast_date==forecast_date_current &location!='US')%>%
  dplyr::rename(model_id=model, output_type=type, output_type_id=quantile)

weighted_US<-rbind(median_weight_us6,median_weight_us4,median_weight_us2,
                   mean_weight_us6,mean_weight_us4,mean_weight_us2,flu_baseline_current)

#Forecasts
weighted_US_forecasts_all<-rbind(weighted_US_forecasts_all,weighted_US)

weighted_state<-rbind(median_weight_state6,median_weight_state4,median_weight_state2,
                      mean_weight_state6,mean_weight_state4,mean_weight_state2,flu_baseline_current_state)

#Forecasts
weighted_state_forecasts_all<-rbind(weighted_state_forecasts_all,weighted_state)

#Weights
weighted_US_weights<-rbind(weighted_US_weights,
                           weight_info6_mean,weight_info4_mean,weight_info2_mean,
                           weight_info6,weight_info4,weight_info2)

weighted_state_weights<-rbind(weighted_state_weights,
                              weight_info6_mean_state,weight_info4_mean_state,weight_info2_mean_state,
                              weight_info6_state,weight_info4_state,weight_info2_state)

}






save(weighted_state_forecasts_all,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted state forecasts  new weight revis.rds")
save(weighted_US_forecasts_all,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted US forecasts  new weight revis.rds")

save(weighted_state_weights,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted state weights revis.rds")
save(weighted_US_weights,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted US weights revis.rds")

table(weighted_state_forecasts_all$forecast_date)
table(weighted_US_weights$forecast_date)

#load("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted scores revis.rds")
#load("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted forecasts revis.rds")

load("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted US forecasts  new weight revis.rds")
load("//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted state forecasts  new weight revis.rds")




#Need to bring in all models from the years and save them together to better evaluate forecasts
all_forecasts_state<-data.frame()
all_forecasts_us<-data.frame()

for (j in 1:length(flu_dates_22_23)) {
  forecast_date_current<-flu_dates_22_23[j]
  
    #Start of a bunch of code previously developed by ARM to pull all the component forecasts
    output_dir <- paste0(ensemble_code_path, "/", forecast_date_current, "/")
    
    if(!dir.exists(output_dir)){
      dir.create(path = output_dir)
    }
    
  

# Get the models to be included in the ensemble
if(!file.exists(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"))){
  file_names = list.files(path = paste0(flusight_path, "/data-forecasts"))
  all_models = file_names[!(file_names %in% c("Test")) &
                            !grepl(paste0(".md", collapse = "|"), file_names)]
  all_metadata = paste0(flusight_path, "/data-forecasts/", all_models,
                        "/metadata-", all_models, ".txt") %>%
    lapply(., read.delim)
  include <- c()
  for(i in 1:length(all_models)){
    
    metadata = all_metadata[[i]]
    
    # this checks to see that this week's file is in the model directory and
    # that it is a designated primary, secondary, or proposed model in the metadata
    if(file.exists(paste0(flusight_path, "/data-forecasts/", all_models[i], 
                          "/", forecast_date_current, "-", all_models[i], ".csv")) &
       (colSums("team_model_designation: primary" == metadata) +
        colSums("team_model_designation: proposed" == metadata) +
        colSums("team_model_designation: secondary" == metadata) > 0)){
      include = c(include, all_models[i])
    }
  }
  write.csv(data.frame(model = include),paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"))
}

# We start by loading the forecasts of weekly incident hospitalizations from selected models
eligible_models = read.csv(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date_current, ".csv"),
                           header = TRUE)
models =as.character(eligible_models$model)


#Read in forecast data

forecast_data <- load_forecasts_repo(
  file_path = paste0(flusight_path, "/data-forecasts/"),
  models = models,
  targets = c(paste(1:4, "wk ahead inc flu hosp")),
  forecast_dates = forecast_date_current,
  hub = "FluSight",
  types = "quantile")%>%
  dplyr::rename(full_location_name = location_name) %>%
  mutate(full_location_name = case_when(location == "US" ~ "United States",
                                        location != "US" ~ full_location_name))


forecast_data$model_id<-forecast_data$model
forecast_data$output_type<-forecast_data$type
forecast_data$output_type_id<-forecast_data$quantile

forecast_data_us<-forecast_data%>%filter(location=='US')
forecast_data_state<-forecast_data%>%filter(location!='US')

all_forecasts_state<-rbind(all_forecasts_state,forecast_data_state)
all_forecasts_us<-rbind(all_forecasts_us,forecast_data_us)

}

all_forecasts_state_compl<-all_forecasts_state%>%filter(model=="Flusight-ensemble")
  all_forecasts_us_compl<-all_forecasts_us%>%filter(model=="Flusight-ensemble")
table(all_forecasts_us_compl$forecast_date)
table(weighted_US_forecasts_all$forecast_date)

table(all_forecasts_us$forecast_date, all_forecasts_us$model)
library(plyr)

#save(weighted_US_weights,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/weighted US weights.rds")


all_forecasts_us_compl2323<-all_forecasts_us_compl
all_forecasts_state_compl2323<-all_forecasts_state_compl

save(all_forecasts_us_compl2323,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/all_forecasts_us_compl2323.rds")

save(all_forecasts_us_compl2323,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/all_forecasts_state_compl2323.rds")

#Score baseline and ensembles


weighted_state_forecasts_all2<-rbind.fill(weighted_state_forecasts_all,all_forecasts_state_compl)%>%
  filter(forecast_date<'2023-02-27')%>%unique()%>%left_join(flu_truth_all %>%  dplyr::select(target_end_date , location, true_value=value),
                                                            by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
  dplyr::mutate(model=model_id,quantile=output_type_id)%>%dplyr::select(-model_id, -output_type_id, -abbreviation ,-type, -full_location_name,-population )

weighted_US_forecasts_all2<-rbind.fill(weighted_US_forecasts_all,all_forecasts_us_compl)%>%
  filter(forecast_date<'2023-02-27')%>%unique()%>%left_join(flu_truth_all %>%  dplyr::select(target_end_date , location, true_value=value),
                                                            by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
  dplyr::mutate(model=model_id,quantile=output_type_id)%>%dplyr::select(-model_id, -output_type_id, -abbreviation ,-type, -full_location_name,-population )



wis_model_us_2223 <- weighted_US_forecasts_all2 %>%select(c(true_value, location, prediction, quantile, model,horizon,forecast_date ))%>%
  check_forecasts()%>%
  score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-ensemble")%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)

wis_model_states_2223 <- weighted_state_forecasts_all2 %>%select(c(true_value, location, prediction, quantile, model,horizon,forecast_date ))%>%
  check_forecasts()%>%
  score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-ensemble")%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)


#limiting to periods where <75% of jurisdictions were categorized as stable 
#using the two-week rate difference categorization schema (horizon 1). 

wis_model_us_2223_trunc <- weighted_US_forecasts_all2 %>%select(c(true_value, location, prediction, quantile, model,horizon,forecast_date ))%>%
  filter(forecast_date >= as.Date("2022-11-05") & forecast_date <= as.Date("2023-01-28"))%>%
  check_forecasts()%>%score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-ensemble")%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)

wis_model_states_2223_trunc <- weighted_state_forecasts_all2 %>%select(c(true_value, location, prediction, quantile, model,horizon,forecast_date ))%>%
  filter(forecast_date >= as.Date("2022-11-05") & forecast_date <= as.Date("2023-01-28"))%>%
  check_forecasts()%>%score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="Flusight-ensemble")%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)





save(wis_model_states_2223,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/wis_model_states_2223.rds")
save(wis_model_us_2223,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/wis_model_us_2223.rds")


save(wis_model_states_2223_trunc,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/wis_model_states_2223_trunc.rds")
save(wis_model_us_2223_trunc,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/wis_model_us_2223_trunc.rds")
 









