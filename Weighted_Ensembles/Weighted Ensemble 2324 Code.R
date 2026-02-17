remotes::install_github("Infectious-Disease-Modeling-Hubs/hubUtils")
remotes::install_github("Infectious-Disease-Modeling-Hubs/hubAdmin")
remotes::install_github("Infectious-Disease-Modeling-Hubs/hubData")


library(plyr)
library(dplyr)
library(tidyverse)
library(zoltr)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)
library(hubData)
library(hubAdmin)
library(yaml)
library(scoringutils)

source("functions/evaluation_functions.R")
source("functions/as_covid_hub_forecasts2.R")
source("functions/list_as_of_data2324.R")
setwd("../../")


flu_dates_23_24 <- as.Date("2023-10-14") + weeks(6:28)#Starting 6 weeks later than the start of forecast (burn period)
task_id_cols <- c("reference_date", "location", "horizon", "target_variable", "target_end_date")
task_id_cols2 <- c("forecast_date", "location", "horizon", "target_variable", "target_end_date")
#Empty dataframes for below
#For scores
weighted_US_score_all<-data.frame()
weighted_state_score_all<-data.frame()
#For forecasts
weighted_state_forecasts_all<-data.frame()
weighted_US_forecasts_all<-data.frame()

weighted_US_weights<-data.frame()
weighted_state_weights<-data.frame()


sigmoid <- function(x,theta) {
  return(exp(x*theta))
}
#Setting values to search across. Cutting off at 5 because no monotonically increasing ensembles are produced when the 
#value is greater than 5
theta<-seq(0,100,0.1)


#This is the first of multiple for loops

#This first loop establishes the "current date" as if we were creating the ensemble in the past
forecasts_6week<-data.frame()

for (j in 1:length(flu_dates_23_24)) {
  current_ref_date<-flu_dates_23_24[j]
  forecasts_6week<-data.frame()
  
  #This the period of time we will use to evalute performance
  flu_dates_23_24_retro <- as.Date(current_ref_date) - weeks(1:6)
  
  #Another loop across the lookback/burn-in/traing period
  for (i in 1:length(flu_dates_23_24_retro)) {
    forecast_date<-flu_dates_23_24_retro[i]
    
    
    out_path <- paste0("Test/")
    hub_path <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-forecast-hub")
    hub_con <- connect_hub(hub_path)
    forecast_data <- hub_con |>
      dplyr::filter(
        reference_date == forecast_date, 
        stringr::str_detect(model_id, "Flusight-ensemble", negate=TRUE) # remove baseline and ensembles
      ) |> 
      dplyr::collect() |>
      as_model_out_tbl() 
    

    eligible_models = read.csv("Data/model-eligbility/models-to-include-in-ensemble-202324.csv")
    models = as.character(eligible_models$Model)

    forecast_data <- forecast_data[forecast_data$model_id %in% models,]
    forecast_data <- forecast_data[forecast_data$location != 78,]
    forecast_data<-forecast_data%>%filter(is.na(forecast_data$value)==F)
    # QUANTILE ENSEMBLE
    forecast_data <- forecast_data |>
      dplyr::filter(output_type == "quantile") |>
      dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id))) # ensures quantiles treated the same regardless of presence of trailing zeros
    
    
    forecasts_6week<-rbind(forecasts_6week,forecast_data)

    
    
   }  
  forecasts_6week<-forecasts_6week%>%dplyr::rename( quantile=output_type_id, target_variable=target,
                                            model=model_id)%>%filter(horizon!=-1)
  #Doing this work separately for the US overall and then all the states and territories
  flu_truth_us<-list_as_of[[j+9]]%>%filter(location=='US')
  flu_truth_states<-list_as_of[[j+9]]%>%filter(location!='US')
  
  flu_truth_us$target_end_date<-as.Date(flu_truth_us$target_end_date)
  flu_truth_states$target_end_date<-as.Date(flu_truth_states$target_end_date)
  
  forecasts_6week$temporal_resolution<-'week'

  forecasts_6week$quantile<-as.numeric(forecasts_6week$quantile)
  #Preparing for the ensemble/scoring functions

  #wouldn't have truth data from the future, so limiting evaluation to only those forecast were we have data
  #as of the current simulated date
  forecasts_6week3<-forecasts_6week%>%mutate(days_off=current_ref_date-reference_date)%>% 
    filter((horizon==3 & days_off>=21) |(horizon==2 & days_off>=14) |(horizon==1 & days_off>=7)|(horizon==0 & days_off>=0))
  #sub-setting to 4 week forecast
  forecasts_4week<- forecasts_6week3%>% filter((current_ref_date-reference_date)/7 <= 4)   
  #sub-setting to 2 week forecast
  forecasts_2week<- forecasts_6week3%>% filter((current_ref_date-reference_date)/7 <= 2)  
  
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
  #Also adding in the truth data
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
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  forecasts_6week_state<-forecasts_6week_state%>%right_join(count_state6)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  #4 week burn
  forecasts_4week_us<-forecasts_4week_us%>%right_join(count_us4)%>%
    dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  forecasts_4week_state<-forecasts_4week_state%>%right_join(count_state4)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  #2 week burn
  forecasts_2week_us<-forecasts_2week_us%>%right_join(count_us2)%>%
    dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  forecasts_2week_state<-forecasts_2week_state%>%right_join(count_state2)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  
  #Evaluate the forecasts
  score_6week_eval_us <- forecasts_6week_us %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_6week_eval_state <-forecasts_6week_state %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_4week_eval_us <- forecasts_4week_us %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
   score_4week_eval_state <- forecasts_4week_state  %>%unique()%>%check_forecasts() %>%score()%>% 
     add_coverage(ranges = c(50, 95), by = c("model")) %>%
     summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
     mutate(cov_50=round(coverage_50*100,2),
            cov_95=round(coverage_95*100,2),
            wis=round(interval_score,2),
            mae=round(ae_median,2),
            rel_wis=round(scaled_rel_skill,2))%>%
     select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
   
   score_2week_eval_us <- forecasts_2week_us  %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  score_2week_eval_state <- forecasts_2week_state  %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
    #Bring in the forecasts for the "current" date
  
  # Get the models to be included in the ensemble

  forecast_data <- hub_con |>
    dplyr::filter(
      reference_date == current_ref_date, 
      stringr::str_detect(model_id, "Flusight", negate=TRUE) # remove baseline and ensembles
    ) |> 
    dplyr::collect() |>
    as_model_out_tbl() 
  
  #Filter to only models designated eligible for inclusion in the ensemble
  eligible_models = read.csv("Data/model-eligbility/models-to-include-in-ensemble-202324.csv") %>% filter(Model!="FluSight-baseline")
  models = as.character(eligible_models$Model)
  
  forecast_data <- forecast_data[forecast_data$model_id %in% models,]
  forecast_data <- forecast_data[forecast_data$location != 78,]
  forecast_data<-forecast_data%>%filter(is.na(forecast_data$value)==F)
  # QUANTILE ENSEMBLE
  forecast_data <- forecast_data |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id)), target_variable=target)%>%
    filter(horizon!=-1) # ensures quantiles treated the same regardless of presence of trailing zeros
   
  
  forecast_data_us<-forecast_data%>%filter(location=='US')%>%mutate(model=model_id)
  forecast_data_state<-forecast_data%>%filter(location!='US')%>%mutate(model=model_id)
  #Done reading in the "current data"
  
  #Limiting the weight calculation to those that are actually included this week's forecasts
  models_current_us<-as.data.frame(unique(forecast_data_us$model_id))%>%dplyr::rename(model=`unique(forecast_data_us$model_id)`)
  models_current_state<-as.data.frame(unique(forecast_data_state$model_id))%>%dplyr::rename(model=`unique(forecast_data_state$model_id)`)
  
  score_6week_eval_us<-score_6week_eval_us%>%inner_join(models_current_us)%>%mutate(model_id=model)
  score_4week_eval_us<-score_4week_eval_us%>%inner_join(models_current_us)%>%mutate(model_id=model)
  score_2week_eval_us<-score_2week_eval_us%>%inner_join(models_current_us)%>%mutate(model_id=model)
  
  score_6week_eval_state<-score_6week_eval_state%>%inner_join(models_current_state)%>%mutate(model_id=model)
  score_4week_eval_state<-score_4week_eval_state%>%inner_join(models_current_state)%>%mutate(model_id=model)
  score_2week_eval_state<-score_2week_eval_state%>%inner_join(models_current_state)%>%mutate(model_id=model)
  
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
    weights_us6<-score_6week_eval_us[,c('model_id','weight')]
    if(max(weights_us6$weight) > 0.3){
      
      break()
    } else{
    
    #Need to select a theta that minimizes the WIS over the training window.
    
    #Forecasts to include
    check_WIS<-forecasts_6week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                value=prediction)
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
      mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                          ifelse(location==lag(location)&horizon==lag(horizon) &
                                   value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
    median_weight_us_train2$check[1]<-1
    
    #If else statement to ignore those functions that are non-monotonically increasing 
    if(is.na(sum(median_weight_us_train2$check))){
      print('ERROR- Nonmonotic function')
    } else{
      #Calculate WIS
      median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
        dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                         by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
        dplyr::rename(model=model_id, quantile=output_type_id)
      
      
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
    weights_us6 <- as.data.frame(cbind(score_6week_eval_us$model_id,
                                       sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k])/
                                         sum(sigmoid(-score_6week_eval_us$rel_wis,theta_us6[k]))
    ))
    colnames(weights_us6)<-c('model_id','weight')

    median_weight_us6<- hubEnsembles::simple_ensemble(forecast_weight_us6,weights=weights_us6,
                                        agg_fun = "median",
                                        model_id="Weighted Median ensemble US6",
                                        task_id_cols = task_id_cols)  |>
      dplyr::mutate(value = ifelse(value < 0, 0, value))
    
    median_weight_us6<-median_weight_us6%>%
      mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) &
                            value>=lag(value) & reference_date ==lag(reference_date ),1,
                          ifelse(location==lag(location)&horizon==lag(horizon) &
                                   value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
    median_weight_us6$check[1]<-1 
    
    if(is.na(sum(median_weight_us6$check))==F){
      print('Done')
      median_weight_us6<-median_weight_us6%>%select(!check)
      
      model_id="Weighted Median ensemble US6"
      theta_keep=theta_us6[k]
      weight_info6<-cbind(weights_us6,current_ref_date,model_id,theta_keep)
      
      break()
    } else{
      #Calculate WIS
      print('ERROR- Nonmonotic function')
    }
  }




################################################################################
#Stop here
################################################################################

#Now the code will just repeat with minor variations based on the length of the burn period. 

#4 Week burn, US overall
train_search_overall<-data.frame()

for(q in 1:length(theta)){
  #Weights  
  score_4week_eval_us$unorm_weights <- sigmoid(-score_4week_eval_us$rel_wis,theta[q])
  score_4week_eval_us$weight <- score_4week_eval_us$unorm_weights / sum(score_4week_eval_us$unorm_weights)
  weights_us4<-score_4week_eval_us[,c('model_id','weight')]
  if(max(weights_us4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
    check_WIS<-forecasts_4week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                value=prediction)


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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
  median_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
    
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
  weights_us4 <- as.data.frame(cbind(score_4week_eval_us$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  median_weight_us4$check[1]<-1 
  
  if(is.na(sum(median_weight_us4$check))==F){
    print('Done')
    median_weight_us4<-median_weight_us4%>%select(!check)
    model_id="Weighted Median ensemble US4"
    theta_keep=theta_us4[k]
    
    weight_info4<-cbind(weights_us4,current_ref_date,model_id,theta_keep)
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
  weights_us2<-score_2week_eval_us[,c('model_id','weight')]
  if(max(weights_us2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                              value=prediction)
  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_us2)%>%select(!weight)
  
  #Calculate weighted median
  median_weight_us_train<- simple_ensemble(check_WIS,weights=weights_us2,
                                           agg_fun = "median",
                                           model_id="Weighted Median train US2",
                                           task_id_cols = task_id_cols)
  
  
  score_4week_eval_us$unorm_weights <- sigmoid(-score_4week_eval_us$rel_wis,theta[q])
  score_4week_eval_us$weight <- score_4week_eval_us$unorm_weights / sum(score_4week_eval_us$unorm_weights)
  weights_us4<-score_4week_eval_us[,c('model_id','weight')]
  
  median_weight_us_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  median_weight_us_train2<-median_weight_us_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
  median_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_us_train2<-median_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_us2 <- as.data.frame(cbind(score_2week_eval_us$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  median_weight_us2$check[1]<-1 
  
  if(is.na(sum(median_weight_us2$check))==F){
    print('Done')
    median_weight_us2<-median_weight_us2%>%select(!check)
    model_id="Weighted Median ensemble US2"
    theta_keep=theta_us2[k]
    
    weight_info2<-cbind(weights_us2,current_ref_date,model_id,theta_keep)
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
  weights_state6<-score_6week_eval_state[,c('model_id','weight')]
  if(max(weights_state6$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_6week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)

  #Keep only those with weights
  check_WIS<-check_WIS%>%right_join(weights_state6)%>%select(!weight)
  
  # quantile_crossing_ex<-check_WIS%>%filter(reference_date==as.Date('2023-11-11'))
  # quantile_crossing_w<-weights_state6
  # 
  # save(quantile_crossing_ex,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/quantile_crossing_ex.rds")
  # save(quantile_crossing_w,file="//cdc.gov/project/NCIRD_ID_EPI_Branch/Frutos/FluSight Ensemble/quantile_crossing_w.rds")
  # 
  # 
  #Calculate weighted median
  median_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state6,
                                              agg_fun = "median",
                                              model_id="Weighted Median train state6",
                                              task_id_cols = task_id_cols)
  
  
  median_weight_state_train$temporal_resolution<-'week'
  #Check to make sure that quantiles monotonically increase
  median_weight_state_train2<-median_weight_state_train%>%
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
  median_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_state_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  median_weight_state6$check[1]<-1 
  
  if(is.na(sum(median_weight_state6$check))==F){
    print('Done')
    median_weight_state6<-median_weight_state6%>%select(!check)
    model_id="Weighted Median ensemble state6"
    theta_keep=theta_state6[k]
    
    weight_info6_state<-cbind(weights_state6,current_ref_date,model_id,theta_keep)
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
  weights_state4<-score_4week_eval_state[,c('model_id','weight')]
  if(max(weights_state4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
  median_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_state_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_state4 <- as.data.frame(cbind(score_4week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  median_weight_state4$check[1]<-1 
  
  if(is.na(sum(median_weight_state4$check))==F){
    print('Done')
    median_weight_state4<-median_weight_state4%>%select(!check)
    model_id="Weighted Median ensemble state4"
    theta_keep=theta_state4[k]
    
    weight_info4_state<-cbind(weights_state4,current_ref_date,model_id, theta_keep)
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
  weights_state2<-score_2week_eval_state[,c('model_id','weight')]
  if(max(weights_state2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date ==lag(reference_date ),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date ==lag(reference_date ),NA,0)))
  median_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(median_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    median_weight_us_train2<-median_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
#Lowest WIS is first. Can then statee this to loop over the calculation of the model
theta_state2<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_state2)){
  #There are normalized weights
  weights_state2 <- as.data.frame(cbind(score_2week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  median_weight_state2$check[1]<-1 
  
  if(is.na(sum(median_weight_state2$check))==F){
    print('Done')
    median_weight_state2<-median_weight_state2%>%select(!check)
    model_id="Weighted Median ensemble state2"
    theta_keep=theta_state2[k]
    
    weight_info2_state<-cbind(weights_state2,current_ref_date,model_id,theta_keep)
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
  weights_us6<-score_6week_eval_us[,c('model_id','weight')]
  if(max(weights_us6$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_6week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                              value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
theta_us6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

#Now need to create a loop to make sure the ensemble with the best theta value acutally
#produces monotonically increasing quantiles
for(k in 1:length(theta_us6)){
  #There are normalized weights
  weights_us6 <- as.data.frame(cbind(score_6week_eval_us$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us6$check[1]<-1 
  
  if(is.na(sum(mean_weight_us6$check))==F){
    print('Done')
    mean_weight_us6<-mean_weight_us6%>%select(!check)
    model_id="Weighted mean ensemble US6"
    theta_keep=theta_us6[k]
    
    weight_info6_mean<-cbind(weights_us6,current_ref_date,model_id,theta_keep)
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
  weights_us4<-score_4week_eval_us[,c('model_id','weight')]
  if(max(weights_us4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                              value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
    
  } else{
    #Calculate WIS
    mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_us4 <- as.data.frame(cbind(score_4week_eval_us$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us4$check[1]<-1 
  
  if(is.na(sum(mean_weight_us4$check))==F){
    print('Done')
    mean_weight_us4<-mean_weight_us4%>%select(!check)
    model_id="Weighted mean ensemble US4"
    theta_keep=theta_us4[k]
    
    weight_info4_mean<-cbind(weights_us4,current_ref_date,model_id,theta_keep)
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
  weights_us2<-score_2week_eval_us[,c('model_id','weight')]
  if(max(weights_us2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_us%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                              value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_us_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    mean_weight_us_train2<-mean_weight_us_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_us %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_us2 <- as.data.frame(cbind(score_2week_eval_us$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_us2$check[1]<-1 
  
  if(is.na(sum(mean_weight_us2$check))==F){
    print('Done')
    mean_weight_us2<-mean_weight_us2%>%select(!check)
    model_id="Weighted mean ensemble US2"
    theta_keep=theta_us2[k]
    
    weight_info2_mean<-cbind(weights_us2,current_ref_date,model_id,theta_keep)
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
  weights_state6<-score_6week_eval_state[,c('model_id','weight')]
  if(max(weights_state6$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_6week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state6$check[1]<-1 
  
  if(is.na(sum(mean_weight_state6$check))==F){
    print('Done')
    mean_weight_state6<-mean_weight_state6%>%select(!check)
    model_id="Weighted mean ensemble state6"
    theta_keep=theta_state6[k]
    
    weight_info6_mean_state<-cbind(weights_state6,current_ref_date,model_id,theta_keep)
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
  weights_state4<-score_4week_eval_state[,c('model_id','weight')]
  if(max(weights_state4$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_4week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_state4 <- as.data.frame(cbind(score_4week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state4$check[1]<-1 
  
  if(is.na(sum(mean_weight_state4$check))==F){
    print('Done')
    mean_weight_state4<-mean_weight_state4%>%select(!check)
    model_id="Weighted mean ensemble state4"
    theta_keep=theta_state4[k]
    
    weight_info4_mean_state<-cbind(weights_state4,current_ref_date,model_id,theta_keep)
    
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
  weights_state2<-score_2week_eval_state[,c('model_id','weight')]
  if(max(weights_state2$weight) > 0.3){
    
    break()
  } else{
  #Need to select a theta that minimizes the WIS over the training window.
  
  #Forecasts to include
  check_WIS<-forecasts_2week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                 value=prediction)
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
    mutate(check=ifelse(location==lag(location)&horizon==lag(horizon) & value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state_train2$check[1]<-1
  
  #If else statement to ignore those functions that are non-monotonically increasing 
  if(is.na(sum(mean_weight_state_train2$check))){
    print('ERROR- Nonmonotic function')
  } else{
    #Calculate WIS
    mean_weight_state_train2<-mean_weight_state_train2%>%select(!check)%>%
      dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                       by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
      dplyr::rename(model=model_id, quantile=output_type_id)
    
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
  weights_state2 <- as.data.frame(cbind(score_2week_eval_state$model_id,
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
                          value>=lag(value) & reference_date==lag(reference_date),1,
                        ifelse(location==lag(location)&horizon==lag(horizon) &
                                 value<lag(value)& reference_date==lag(reference_date),NA,0)))
  mean_weight_state2$check[1]<-1 
  
  if(is.na(sum(mean_weight_state2$check))==F){
    print('Done')
    mean_weight_state2<-mean_weight_state2%>%select(!check)
    model_id="Weighted mean ensemble state2"
    theta_keep=theta_state2[k]
    
    weight_info2_mean_state<-cbind(weights_state2,current_ref_date,model_id,theta_keep)
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



weighted_US_forecasts_all<-rbind(weighted_US_forecasts_all,median_weight_us6,median_weight_us4,median_weight_us2,
                   mean_weight_us6,mean_weight_us4,mean_weight_us2)


weighted_state_forecasts_all<-rbind(weighted_state_forecasts_all,median_weight_state6,median_weight_state4,median_weight_state2,
                      mean_weight_state6,mean_weight_state4,mean_weight_state2)


#Weights
weighted_US_weights<-rbind(weighted_US_weights,
                           weight_info6_mean,weight_info4_mean,weight_info2_mean,
                           weight_info6,weight_info4,weight_info2)

weighted_state_weights<-rbind(weighted_state_weights,
                              weight_info6_mean_state,weight_info4_mean_state,weight_info2_mean_state,
                              weight_info6_state,weight_info4_state,weight_info2_state)

}


table(weighted_US_forecasts_all$reference_date)

weighted_state_weights2<-weighted_state_weights
weighted_US_weights2<-weighted_US_weights

rm(weighted_US_weights,weighted_state_weights)

save(weighted_state_forecasts_all,file="Test/weighted state forecasts 2324.rds")
save(weighted_US_forecasts_all,file="Test/weighted US forecasts 2324.rds")

save(weighted_state_weights,file="Test/weighted state weights 2324.rds")
save(weighted_US_weights,file="Test/weighted US weights 2324.rds")


load(file="Data/rds/2324/weighted state forecasts 2324.rds")
load(file="Data/rds/2324/weighted US forecasts 2324.rds")

load(file="Data/rds/2324/weighted state weights 2324.rds")
load(file="Data/rds/2324/weighted US weights 2324.rds")



#Scoring ensembles
#First bringing in ensemble, baselin, and LOP
all_forecasts_state<-data.frame()
all_forecasts_us<-data.frame()
out_path <- paste0("Test/")
hub_path <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-forecast-hub")
hub_con <- connect_hub(hub_path) 


for (j in 1:length(flu_dates_23_24)) {
  current_ref_date<-flu_dates_23_24[j]
  
forecast_data <- hub_con |>
  dplyr::filter(
    reference_date == current_ref_date) |> 
  dplyr::collect() |>
  as_model_out_tbl() 


eligible_models = read.csv("Data/model-eligbility/models-to-include-in-ensemble-202324.csv") %>% select(-X)
new_entry <- data.frame(Model = "FluSight-ensemble", Designated_Model = FALSE)
eligible_models <- rbind(eligible_models, new_entry)
models = as.character(eligible_models$Model)

forecast_data <- forecast_data[forecast_data$model_id %in% models,]
forecast_data <- forecast_data[forecast_data$location != 78,]
forecast_data<-forecast_data%>%filter(is.na(forecast_data$value)==F)
# QUANTILE ENSEMBLE
forecast_data <- forecast_data |> filter(horizon !=-1)%>%
  dplyr::filter(output_type == "quantile") |>
  dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id)), target_variable=target) # ensures quantiles treated the same regardless of presence of trailing zeros


forecast_data_us<-forecast_data%>%filter(location=='US')%>%mutate(model=model_id)
forecast_data_state<-forecast_data%>%filter(location!='US')%>%mutate(model=model_id)
#Done reading in the "current data"

all_forecasts_state<-rbind(all_forecasts_state,forecast_data_state)
all_forecasts_us<-rbind(all_forecasts_us,forecast_data_us)

}


all_forecasts_state_compl<-all_forecasts_state%>%filter(model=="FluSight-ensemble"|model=="FluSight-baseline")
all_forecasts_us_compl<-all_forecasts_us%>%filter(model=="FluSight-ensemble"|model=="FluSight-baseline")

all_forecasts_us_compl2324<-all_forecasts_us_compl
all_forecasts_state_compl2324<-all_forecasts_state_compl

save(all_forecasts_us_compl2324,file="Data/rds/2324/all_forecasts_us_compl2324.rds")
save(all_forecasts_state_compl2324,file="Data/rds/2324/all_forecasts_state_compl2324.rds")


load(file="Data/rds/2324/all_forecasts_us_compl2324.rds")
load(file="Data/rds/2324/all_forecasts_state_compl2324.rds")

library(plyr)
#Score baseline and ensembles

flu_truth_current<-flu_truth_current%>%dplyr::rename(target_end_date=date)

flu_truth_current<-flu_truth_current%>%mutate(target_end_date=as.Date(target_end_date))

weighted_state_forecasts_all2<-rbind.fill(weighted_state_forecasts_all,all_forecasts_state_compl2324)%>%unique()%>%left_join(flu_truth_current %>%  dplyr::select(target_end_date , location, true_value=value),
                                                            by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
  dplyr::mutate(model=model_id,quantile=output_type_id)%>%dplyr::select(-model_id, -output_type_id,-target)%>%mutate(quantile=as.numeric(quantile))

weighted_US_forecasts_all2<-rbind.fill(weighted_US_forecasts_all,all_forecasts_us_compl2324)%>%unique()%>%left_join(flu_truth_current %>%  dplyr::select(target_end_date , location, true_value=value),
                                                            by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
  dplyr::mutate(model=model_id,quantile=output_type_id)%>%dplyr::select(-model_id, -output_type_id,-target )%>%mutate(quantile=as.numeric(quantile))


table(weighted_US_forecasts_all2$model)
table(weighted_US_forecasts_all2$model,weighted_US_forecasts_all2$reference_date )
table(weighted_US_forecasts_all2$model,weighted_US_forecasts_all2$quantile )
table(weighted_US_forecasts_all2$model,weighted_US_forecasts_all2$horizon )
table(weighted_US_forecasts_all2$model,weighted_US_forecasts_all2$location )
table(weighted_US_forecasts_all2$model,weighted_US_forecasts_all2$true_value)

library(covidHubUtils)
weighted_US_forecasts_all2$model_id=weighted_US_forecasts_all2$model
weighted_US_forecasts_all2$forecast_date=weighted_US_forecasts_all2$reference_date
weighted_US_forecasts_all2$output_type_id=weighted_US_forecasts_all2$quantile
weighted_US_forecasts_all2$target=weighted_US_forecasts_all2$target_variable 
weighted_US_forecasts_all2$value=weighted_US_forecasts_all2$prediction 

weighted_state_forecasts_all2$model_id=weighted_state_forecasts_all2$model
weighted_state_forecasts_all2$forecast_date=weighted_state_forecasts_all2$reference_date
weighted_state_forecasts_all2$output_type_id=weighted_state_forecasts_all2$quantile
weighted_state_forecasts_all2$target=weighted_state_forecasts_all2$target_variable 
weighted_state_forecasts_all2$value=weighted_state_forecasts_all2$prediction 

test_truth2<-test_truth%>%mutate(target_end_date=as.Date(date), target_variable='wk inc flu hosp')


#Scored forecasts

wis_model_us_2324 <- weighted_US_forecasts_all2 %>%filter(is.na(true_value)==F) %>%filter(target_end_date<as.Date("2024-05-04")) %>% 
  select(c(true_value, location, prediction, quantile, model,horizon,reference_date))%>%check_forecasts()%>%score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,
                   baseline="FluSight-ensemble", relative_skill_metric='interval_score')%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)



wis_model_states_2324 <- weighted_state_forecasts_all2%>% filter(target_end_date<as.Date("2024-05-04")) %>% 
  select(c(true_value, location, prediction, quantile, model,horizon,reference_date))%>%check_forecasts()%>%score%>%
  add_coverage(ranges = c(50, 95), by = c("model")) %>%
  summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-ensemble")%>%
  mutate(cov_50=round(coverage_50*100,2),
         cov_95=round(coverage_95*100,2),
         wis=round(interval_score,2),
         mae=round(ae_median,2),
         rel_wis=round(scaled_rel_skill,2))%>%
  select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)


save(wis_model_states_2324,file="Data/rds/2324/wis_model_states_2324.rds")
save(wis_model_us_2324,file="Data/rds/2324/wis_model_us_2324.rds")
