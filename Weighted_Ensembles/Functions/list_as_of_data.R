nm <- list.files(path="Data/as of data/")
setwd("Data/as of data/")
test_truth_all<-data.frame()
library(cdlTools)
list_as_of<-list()

for (i in 1:length(nm)) {

  test_truth<- read.csv(file=nm[i])
  test_truth$location_num<- fips(test_truth$location_name)
  test_truth$location_num<-sprintf("%02d", test_truth$location_num)
  test_truth$location<- ifelse(test_truth$location_name== 'National', 'US',ifelse(test_truth$location_name=='Virgin Islands',
                                                                            '78',paste(test_truth$location_num)))
  test_truth$target_variable<-'inc flu hosp'
  test_truth$model<-'flu-truth'
  
  
list_as_of[[i]]<-test_truth%>%dplyr::rename(target_end_date=wk_end_date)%>%select(location,value,target_end_date,model,target_variable)

}
#View(list_as_of[[1]])
