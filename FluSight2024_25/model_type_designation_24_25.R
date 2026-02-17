library(tidyverse)
library(yaml)

###This was run in July 2024 and if run again, may pull in models that were updated for the 2024/2025 season because the same FluSight forecast hub repository was used for both seasons.
mylib <- getwd()
setwd(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-forecast-hub/model-metadata"))

yamlfiles <- list.files(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-forecast-hub/model-metadata"))
yamlfiles <- yamlfiles[!grepl("README.md", yamlfiles)]

extract_variables <- function(file_path) {
  data <- yaml.load_file(file_path)
  
  # Extract the variables from the loaded YAML data
  team_name <- data$team_name
  team_abbr <- data$team_abbr
  model_name <- data$model_name
  model_abbr <- data$model_abbr
  designated_model <- data$designated_model
  methods <- data$methods
  methods_long <- data$methods_long
  ensemble_of_models <- data$ensemble_of_models
  
  # Create a new row in the dataframe with the extracted variables
  new_row <- tibble(
    team_name = team_name,
    team_abbr = team_abbr,
    model_name = model_name,
    model_abbr = model_abbr,
    designated_model = designated_model,
    methods = methods,
    methods_long = methods_long,
    ensemble_of_models = ensemble_of_models
  )
  
  return(new_row)
}

df <- NULL

for (file_path in yamlfiles) {
  new_row <- extract_variables(file_path)
  
  df <- bind_rows(df, new_row)
}

setwd(mylib)

df <- df %>% filter(str_detect(string = model_abbr, pattern = "_cat")== FALSE )%>% 
  mutate(model_name_pulled = model_name,
         model_name = paste0(team_abbr, "-", model_abbr),
         methods = tolower(methods),
         mechanistic = case_when(str_detect(string = methods, pattern = "mechanistic")==TRUE ~ TRUE, 
                                str_detect(string = methods, pattern = "seir")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "sir")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "slir")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "compartment")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "renewal")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "dynamics")==TRUE ~ TRUE,
                                str_detect(string = methods, pattern = "km27")== TRUE ~ TRUE, 
                                TRUE ~ FALSE),
         ai_ml = case_when(str_detect(string = methods, pattern = "lstm")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "random forest")==TRUE ~ TRUE,
                           str_detect(string = model_abbr, pattern = "GBR")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "svm")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "deep")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "neural")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "machine learning")==TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "lightgbm") == TRUE ~ TRUE,
                           str_detect(string = methods, pattern = "generative")==TRUE ~ TRUE,
                                TRUE ~ FALSE), 
         stat = case_when(str_detect(string = methods, pattern = "holt")==TRUE ~ TRUE,
                          str_detect(string = methods, pattern = fixed("statistical"))== TRUE ~ TRUE,
                          str_detect(string = methods, pattern = "arima")== TRUE ~ TRUE, 
                          str_detect(string = methods, pattern = "bayesian")==TRUE ~ TRUE,
                          str_detect(string = methods, pattern = "regression")== TRUE ~ TRUE,
                          str_detect(string = methods, pattern = "random walk")== TRUE ~ TRUE,
                          str_detect(string = methods, pattern = "time-series")== TRUE ~ TRUE,
                          mechanistic == FALSE & ai_ml == FALSE ~ TRUE, 
                          TRUE ~ FALSE))
  
  
write.csv(df, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-manuscripts/FluSight2024_25/model_types.csv"), row.names = FALSE)

