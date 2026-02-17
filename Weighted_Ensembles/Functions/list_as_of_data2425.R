
# Specific files to use
desired_files <- c(
  "target-hospital-admissions_2024-11-16.csv",
  "target-hospital-admissions_2024-11-30.csv",
  "target-hospital-admissions_2024-12-14.csv",
  "target-hospital-admissions_2024-12-21.csv",
  "target-hospital-admissions_2024-12-28.csv",
  "target-hospital-admissions_2025-01-04.csv",
  "target-hospital-admissions_2025-01-11.csv",
  "target-hospital-admissions_2025-01-25.csv",
  "target-hospital-admissions_2025-02-01.csv",
  "target-hospital-admissions_2025-02-08.csv",
  "target-hospital-admissions_2025-02-15.csv",
  "target-hospital-admissions_2025-02-22.csv",
  "target-hospital-admissions_2025-03-01.csv",
  "target-hospital-admissions_2025-03-08.csv",
  "target-hospital-admissions_2025-03-15.csv",
  "target-hospital-admissions_2025-03-22.csv",
  "target-hospital-admissions_2025-03-29.csv",
  "target-hospital-admissions_2025-04-05.csv",
  "target-hospital-admissions_2025-04-12.csv",
  "target-hospital-admissions_2025-04-19.csv",
  "target-hospital-admissions_2025-04-26.csv",
  "target-hospital-admissions_2025-05-03.csv",
  "target-hospital-admissions_2025-05-10.csv",
  "target-hospital-admissions_2025-05-17.csv",
  "target-hospital-admissions_2025-05-24.csv",
  "target-hospital-admissions_2025-05-31.csv",
  "target-hospital-admissions_2025-06-07.csv",
  "target-hospital-admissions_2025-06-14.csv",
  "target-hospital-admissions_2025-06-28.csv",
  "target-hospital-admissions_2025-07-05.csv",
  "target-hospital-admissions_2025-07-19.csv"
)

# Set the working directory and list files
setwd("data/target-data-archive/")
nm <- list.files(pattern="\\.csv$", full.names = TRUE)

test_truth_all <- data.frame()
list_as_of <- list()

for (i in 1:length(nm)) {
  file_name <- basename(nm[i])  # Get the file name without the path

  # Check if the file is in the desired files list
  if (file_name %in% desired_files) {
    test_truth <- read.csv(file = nm[i])
    test_truth$target_variable <- 'inc flu hosp'
    test_truth$model <- 'flu-truth'

    # Rename and select relevant columns
    test_truth <- test_truth %>%
      dplyr::rename(target_end_date = date) %>%
      dplyr::select(location, value, target_end_date, model, target_variable)

    # Add to the list
    list_as_of[[length(list_as_of) + 1]] <- test_truth
  }
}



flu_truth_current<-read.csv(file="target-hospital-admissions_2025-07-19.csv")
flu_truth_current$target_variable<-'inc flu hosp'
flu_truth_current$model<-'flu-truth'

