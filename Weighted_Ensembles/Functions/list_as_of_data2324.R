nm <- list.files(path="data/target-data-archive/")
setwd("Data/target-data-archive/")
test_truth_all<-data.frame()
library(cdlTools)
list_as_of<-list()

for (i in 1:length(nm)) {

  test_truth<- read.csv(file=nm[i])
  test_truth$target_variable<-'inc flu hosp'
  test_truth$model<-'flu-truth'
  
  
list_as_of[[i]]<-test_truth%>%dplyr::rename(target_end_date=date)%>%select(location,value,target_end_date,model,target_variable)

}

flu_truth_current<-read.csv("target-hospital-admissions_2025-07-19.csv")
flu_truth_current$target_variable<-'inc flu hosp'
flu_truth_current$model<-'flu-truth'


