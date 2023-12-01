###### Backfill Epicurve

#### Alex need to rename these to match manuscript 

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
