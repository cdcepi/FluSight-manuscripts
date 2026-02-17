#Counts of forecasts per week
#setwd('C:/Users/zoh1/OneDrive - CDC/Documents - NCIRD-EPB-ARM/Modeling-Forecasting/FluSight/Manuscripts/Weighted ensemble/Frutos_FluSight Ensemble')
names(weighted_US_weights22)[4]<-"ensemble_type"
names(weighted_US_weights23)[4]<-"ensemble_type"
names(weighted_US_weights24)[4]<-"ensemble_type"
names(weighted_state_weights22)[4]<-"ensemble_type"
names(weighted_state_weights)[4]<-"ensemble_type"
names(weighted_state_weights24)[4] <- "ensemble_type"


total_count22 <-weighted_US_weights22%>%dplyr::filter(ensemble_type=='Weighted Median ensemble US2')
table(total_count22$forecast_date_current)
unique(weighted_US_weights22$model_id)
unique(weighted_US_weights22$model_id)




result <- weighted_US_weights22 %>%
  group_by(ensemble_type, forecast_date_current) %>%
  dplyr::summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  dplyr::summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)

test<-rbind(weighted_US_weights22,weighted_state_weights22)
unique(test$model_id)


result <- weighted_state_weights22 %>%
  group_by(ensemble_type, forecast_date_current) %>%
  dplyr::summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  dplyr::summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)


test<-rbind(weighted_US_weights23,weighted_state_weights)
unique(test$model_id)

############### 23-24

result <- weighted_US_weights23 %>%
  group_by(ensemble_type, current_ref_date) %>%
  summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)

test<-rbind(weighted_US_weights22,weighted_state_weights22)
unique(test$model_id)


result <- weighted_state_weights %>%
  group_by(ensemble_type, current_ref_date) %>%
  summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)


############### 24-25

result <- weighted_US_weights24 %>%
  group_by(ensemble_type, current_ref_date) %>%
  dplyr::summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  dplyr::summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)

test<-rbind(weighted_US_weights24,weighted_state_weights24)
unique(test$model_id)


result <- weighted_state_weights24 %>%
  group_by(ensemble_type, current_ref_date) %>%
  dplyr::summarise(num_model_ids = n_distinct(model_id)) %>%
  group_by(ensemble_type) %>%
  dplyr::summarise(min_num_model_ids = min(num_model_ids),
            median_num_model_ids = median(num_model_ids),
            max_num_model_ids = max(num_model_ids))

# Print the result
print(result)

#2022-2023

median_value<-weighted_US_forecasts_all_2223%>%filter(output_type_id==.5)%>%  dplyr::rename(median=value)%>%dplyr::select(!output_type_id)
ucl95_value<-weighted_US_forecasts_all_2223%>%filter(output_type_id==.95)%>%  dplyr::rename(ucl95=value)%>%dplyr::select(!output_type_id)
lcl95_value<-weighted_US_forecasts_all_2223%>%filter(output_type_id==.05)%>%  dplyr::rename(lcl95=value)%>%dplyr::select(!output_type_id)
ucl50_value<-weighted_US_forecasts_all_2223%>%filter(output_type_id==.75)%>%  dplyr::rename(ucl50=value)%>%dplyr::select(!output_type_id)
lcl50_value<-weighted_US_forecasts_all_2223%>%filter(output_type_id==.25)%>%  dplyr::rename(lcl50=value)%>%dplyr::select(!output_type_id)


plotting_value<-median_value%>%left_join(ucl95_value, by=c("model_id", "forecast_date", "target_end_date"))%>%left_join(lcl95_value, by=c("model_id", "forecast_date","target_end_date"))%>%
  left_join(ucl50_value, by=c("model_id", "forecast_date", "target_end_date"))%>%left_join(lcl50_value, by=c("model_id", "forecast_date","target_end_date"))%>%unique()


plotting_value_edit<-plotting_value%>%filter(model_id=='Weighted Median ensemble US4' |model_id=='Flusight-ensemble' )
plotting_value_edit$model_id<-factor(plotting_value_edit$model_id,
                                     levels = c('flu-truth','Flusight-ensemble',
                                                'Weighted Median ensemble US4'),
                                     labels=c('Truth', 'Untrained median', 'Median (4 week)')  )
plotting_value_edit$forecast_date<-as.Date(plotting_value_edit$forecast_date)


plotting_value_mult<-plotting_value_edit%>%
  rbind.fill(flu_truth_plot)%>%
  mutate(model_id=factor(model_id, levels=c('Truth', 'Untrained median', 'Median (4 week)')))


plot_forecast1 <-plotting_value_mult%>%
  filter(forecast_date=="2022-11-28"|forecast_date=="2022-12-26" |forecast_date=="2023-01-23"|model_id=='Truth')%>%
  ggplot(aes(y =median,x=target_end_date, col=model_id,fill=model_id,group = interaction(model_id, forecast_date)))+
  geom_ribbon(aes(ymin=lcl95,ymax=ucl95),alpha=0.2,colour = NA, show.legend = FALSE,linewidth=1)+
  scale_color_manual(values=c('black','dodgerblue3',"chocolate2"))+
  scale_fill_manual(values=c(NA,'dodgerblue3',"chocolate2"))+
  xlab("Date") +geom_ribbon(aes(ymin=lcl50,ymax=ucl50),alpha=0.5, colour = NA, show.legend = FALSE,linewidth=1)+geom_line(aes(linetype = model_id))+
  geom_point(size=2.5)+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+scale_shape_manual(values = c(1,2,2))+
  ylab("Hospital admissions")+ylim(c(0,35500))+theme_bw()+
  theme(legend.key = element_blank(), legend.title = element_blank(),
        panel.border = element_blank(),axis.line = element_line(color = 'black'), legend.position = c(0.85, 0.9),)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y",limits = c(as.Date('2022-10-17'),as.Date('2023-02-15'))) 


#############################
library(dplyr)
library(ggplot2)

# Make sure dates are Dates
plotting_value_mult <- plotting_value_mult %>%
  mutate(
    forecast_date  = as.Date(forecast_date),
    target_end_date = as.Date(target_end_date)
  )

# Earliest forecast date (excluding Truth)
first_fc <- plotting_value_mult %>%
  filter(model_id != "Truth") %>%
  summarise(first_fc = min(forecast_date, na.rm = TRUE)) %>%
  pull(first_fc)

plot_forecast1 <- plotting_value_mult %>%
  filter(forecast_date %in% as.Date(c("2022-11-28","2022-12-26","2023-01-23")) | model_id == "Truth") %>%
  ggplot(aes(y = median, x = target_end_date, col = model_id, fill = model_id,
             group = interaction(model_id, forecast_date))) +
  # training-period shading + label
  annotate("rect",
           xmin = as.Date("2022-10-22"), xmax = as.Date("2022-11-26"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey50") +
  annotate("text",
           x = as.Date("2022-10-26"), y = 25500,
           label = "Initial Training Period",
           hjust = 0, size = 3, color = "black") +
  geom_ribbon(aes(ymin = lcl95, ymax = ucl95), alpha = 0.2, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_ribbon(aes(ymin = lcl50, ymax = ucl50), alpha = 0.5, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_line(aes(linetype = model_id)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("black", "dodgerblue3", "chocolate2")) +
  scale_fill_manual(values = c(NA, "dodgerblue3", "chocolate2")) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  xlab("Date") +
  ylab("Hospital admissions") +
  ylim(c(0, 35500)) +
  theme_bw() +
  theme(
    legend.key = element_blank(), legend.title = element_blank(),
    panel.border = element_blank(), axis.line = element_line(color = "black"),
    legend.position = c(0.85, 0.9)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
               limits = c(as.Date("2022-10-17"), as.Date("2023-02-15")))



plot_forecast1


plotting_value_edit<-plotting_value%>%filter(model_id=='Weighted Median ensemble US6' |model_id=='Flusight-ensemble' )
plotting_value_edit$model_id<-factor(plotting_value_edit$model_id,
                                     levels = c('flu-truth','Flusight-ensemble',
                                                'Weighted Median ensemble US6'),
                                     labels=c('Truth', 'Untrained median', 'Median (6 week)')  )
plotting_value_edit$forecast_date<-as.Date(plotting_value_edit$forecast_date)


plotting_value_mult2<-plotting_value_edit%>%
  rbind.fill(flu_truth_plot)%>%
  mutate(model_id=factor(model_id, levels=c('Truth', 'Untrained median', 'Median (6 week)')))


plot_forecast1b <-plotting_value_mult2%>%
  filter(forecast_date=="2022-11-28"|forecast_date=="2022-12-26" |forecast_date=="2023-01-23"|model_id=='Truth')%>%
  ggplot(aes(y =median,x=target_end_date, col=model_id,fill=model_id,group = interaction(model_id, forecast_date)))+
  geom_ribbon(aes(ymin=lcl95,ymax=ucl95),alpha=0.2, show.legend = FALSE,linewidth=1)+
  scale_color_manual(values=c('black','dodgerblue3',"firebrick2"))+
  scale_fill_manual(values=c(NA,'dodgerblue3',"firebrick2"))+
  xlab("Date") +geom_ribbon(aes(ymin=lcl50,ymax=ucl50),alpha=0.5, show.legend = FALSE,linewidth=1)+geom_line(aes(linetype = model_id))+
  geom_point(size=2.5)+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+scale_shape_manual(values = c(1,2,2))+
  ylab("Hospital admissions")+ylim(c(0,35500))+theme_bw()+
  theme(legend.key = element_blank(), legend.title = element_blank(),
        panel.border = element_blank(),axis.line = element_line(color = 'black'), legend.position = c(0.85, 0.9),)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y",limits = c(as.Date('2022-10-17'),as.Date('2023-02-15'))) 

plot_forecast1/plot_forecast1b+ plot_layout(guides = "collect")



median_value<-weighted_US_forecasts_all_2324%>%filter(output_type_id==.5)%>%  dplyr::rename(median=value)%>%dplyr::select(!output_type_id)
ucl95_value<-weighted_US_forecasts_all_2324%>%filter(output_type_id==.95)%>%  dplyr::rename(ucl95=value)%>%dplyr::select(!output_type_id)
lcl95_value<-weighted_US_forecasts_all_2324%>%filter(output_type_id==.05)%>%  dplyr::rename(lcl95=value)%>%dplyr::select(!output_type_id)
ucl50_value<-weighted_US_forecasts_all_2324%>%filter(output_type_id==.75)%>%  dplyr::rename(ucl50=value)%>%dplyr::select(!output_type_id)
lcl50_value<-weighted_US_forecasts_all_2324%>%filter(output_type_id==.25)%>%  dplyr::rename(lcl50=value)%>%dplyr::select(!output_type_id)



plotting_value<-median_value%>%left_join(ucl95_value, by=c("model_id", "reference_date", "target_end_date"))%>%left_join(lcl95_value, by=c("model_id", "reference_date","target_end_date"))%>%
  left_join(ucl50_value, by=c("model_id", "reference_date", "target_end_date"))%>%left_join(lcl50_value, by=c("model_id", "reference_date","target_end_date"))%>%unique()

flu_truth_plot<-flu_truth_current%>%filter(location=='US')%>%dplyr::rename(target_end_date=date, median=value)%>%
  dplyr::select(median,target_end_date)%>%mutate(model_id='Truth',target_end_date=as.Date(target_end_date) )

plotting_value_edit<-plotting_value%>%filter(model_id=='Weighted Median ensemble US6' |model_id=='FluSight-ensemble' )
plotting_value_edit$model_id<-factor(plotting_value_edit$model_id,
                                     levels = c('flu-truth','FluSight-ensemble',
                                                'Weighted Median ensemble US6'),
                                     labels=c('Truth', 'Untrained median', 'Median (6 week)')  )
plotting_value_edit$reference_date<-as.Date(plotting_value_edit$reference_date)


plotting_value_mult<-plotting_value_edit%>%rbind.fill(flu_truth_plot)%>%
  mutate(model_id=factor(model_id, levels=c('Truth', 'Untrained median', 'Median (6 week)')))



plot_forecast2 <-plotting_value_mult%>%
  filter(reference_date=="2023-11-25"|reference_date=="2023-12-23" |reference_date=="2024-01-20"|reference_date=="2024-02-17"|
           reference_date=="2024-03-16"|reference_date=="2024-04-13"| model_id=='Truth')%>%
  ggplot(aes(y =median,x=target_end_date, col=model_id,fill=model_id,group = interaction(model_id, reference_date)))+
  geom_ribbon(aes(ymin=lcl95,ymax=ucl95),alpha=0.2, colour = NA, show.legend = FALSE,linewidth=1)+
  scale_color_manual(values=c('black','dodgerblue3',"firebrick2"))+
  scale_fill_manual(values=c(NA,'dodgerblue3',"firebrick2"))+
  xlab("Date") +geom_ribbon(aes(ymin=lcl50,ymax=ucl50),alpha=0.5, colour = NA, show.legend = FALSE,linewidth=1)+geom_line(aes(linetype = model_id))+
  geom_point(size=2.5)+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+scale_shape_manual(values = c(1,2,2))+
  ylab("Hospital admissions")+ylim(c(0,30000))+theme_bw()+
  theme(legend.key = element_blank(), legend.title = element_blank(),
        panel.border = element_blank(),axis.line = element_line(color = 'black'), legend.position = c(0.85, 0.9),)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y",limits = c(as.Date('2023-10-10'),as.Date('2024-04-30'))) 


# Find first forecast ref date (exclude Truth) with TRAINING PERIOD GREY
first_ref <- plotting_value_mult %>%
  filter(model_id != "Truth") %>%
  mutate(reference_date = as.Date(reference_date)) %>%
  summarise(first_ref = min(reference_date, na.rm = TRUE)) %>%
  pull(first_ref)

plot_forecast2 <- plotting_value_mult %>%
  filter(reference_date %in% as.Date(c("2023-11-25","2023-12-23","2024-01-20","2024-02-17","2024-03-16","2024-04-13")) |
           model_id == "Truth") %>%
  ggplot(aes(y = median, x = target_end_date, col = model_id, fill = model_id,
             group = interaction(model_id, reference_date))) +
  # training-period shading + label
  annotate("rect",
           xmin = as.Date("2023-10-10"), xmax = as.Date("2023-11-18"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey50") +
  annotate("text",
           x = as.Date("2023-10-15"), y = 20000,
           label = "Initial Training \n   Period",
           hjust = 0, size = 3, color = "black") +
  geom_ribbon(aes(ymin = lcl95, ymax = ucl95), alpha = 0.2, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_ribbon(aes(ymin = lcl50, ymax = ucl50), alpha = 0.5, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_line(aes(linetype = model_id)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("black", "dodgerblue3", "firebrick2")) +
  scale_fill_manual(values = c(NA, "dodgerblue3", "firebrick2")) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  xlab("Date") +
  ylab("Hospital admissions") +
  ylim(c(0, 30000)) +
  theme_bw() +
  theme(
    legend.key = element_blank(), legend.title = element_blank(),
    panel.border = element_blank(), axis.line = element_line(color = "black"),
    legend.position = c(0.85, 0.9)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
               limits = c(as.Date("2023-10-10"), as.Date("2024-04-30")))


library(patchwork)
plot_forecast1
plot_forecast2



plotting_value_edit<-plotting_value%>%filter(model_id=='Weighted mean ensemble US6' |model_id=='FluSight-ensemble' )
plotting_value_edit$model_id<-factor(plotting_value_edit$model_id,
                                     levels = c('flu-truth','FluSight-ensemble',
                                                'Weighted mean ensemble US6'),
                                     labels=c('Truth', 'Untrained median', 'Mean (6 week)')  )
plotting_value_edit$reference_date<-as.Date(plotting_value_edit$reference_date)


plotting_value_mult<-plotting_value_edit%>%rbind.fill(flu_truth_plot)%>%
  mutate(model_id=factor(model_id, levels=c('Truth', 'Untrained median', 'Mean (6 week)')))


plot_forecast2b <-plotting_value_mult%>%
  filter(reference_date=="2023-11-25"|reference_date=="2023-12-23" |reference_date=="2024-01-20"|reference_date=="2024-02-17"|
           reference_date=="2024-03-16"|reference_date=="2024-04-13"| model_id=='Truth')%>%
  ggplot(aes(y =median,x=target_end_date, col=model_id,fill=model_id,group = interaction(model_id, reference_date)))+
  geom_ribbon(aes(ymin=lcl95,ymax=ucl95),alpha=0.2, show.legend = FALSE,linewidth=1)+
  scale_color_manual(values=c('black','dodgerblue3',"maroon"))+
  scale_fill_manual(values=c(NA,'dodgerblue3',"maroon"))+
  xlab("Date") +geom_ribbon(aes(ymin=lcl50,ymax=ucl50),alpha=0.5, show.legend = FALSE,linewidth=1)+geom_line(aes(linetype = model_id))+
  geom_point(size=2.5)+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+scale_shape_manual(values = c(1,2,2))+
  ylab("Hospital admissions")+ylim(c(0,30000))+theme_bw()+
  theme(legend.key = element_blank(), legend.title = element_blank(),
        panel.border = element_blank(),axis.line = element_line(color = 'black'), legend.position = c(0.85, 0.9),)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y",limits = c(as.Date('2023-10-10'),as.Date('2024-04-30'))) 

plot_forecast2/plot_forecast2b

setwd('C:/Users/uqt8/OneDrive - CDC/Influenza Division/Ensemble Forecasting')

tiff("./Figures/Figure 1 2223.tiff",units="in", width=6, height=4, res=300)
plot_forecast1
dev.off()

tiff("./Figures/Figure 2 2324.tiff",units="in", width=6, height=4, res=300)
plot_forecast2
dev.off()


#Looking at weights over time`
library(viridis)
######################################################################### 2022-23

#US overall
names(weighted_US_weights22)[4]<-"ensemble_type"
weighted_US_weights22$weight<-as.numeric(weighted_US_weights22$weight)

weighted_US_weights_edit<-weighted_US_weights22
weighted_US_weights_edit<-weighted_US_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))

viridris_17<-viridis(16)
viridris_17<-append(viridris_17,"grey",after=0)

lastlevel = function (f, last, ...) {
  if (!is.factor(f)) stop("f must be a factor")
  lev <- levels(f)
  if (length(last) != 1L) 
    stop("'last' must be of length one")
  if (is.character(last)) 
    last <- match(last, lev)
  if (is.na(last)) 
    stop("'last' must be an existing level")
  nlev <- length(lev)
  if (last < 1 || last > nlev) 
    stop(gettextf("last = %d must be in 1L:%d", last, nlev), 
         domain = NA)
  factor(f, levels = lev[c(last, seq_along(lev)[-last])])
}
weighted_US_weights_edit$model_edit<-lastlevel(as.factor(weighted_US_weights_edit$model_edit),"Models with < 10% weight" )
weighted_US_weights_edit2223<-weighted_US_weights_edit%>% filter(ensemble_type=='Weighted Median ensemble US4')

weighted_US_weights_edit2223$model_edit2<-weighted_US_weights_edit2223$model_edit
weighted_US_weights_edit2223$model_edit2<-factor(weighted_US_weights_edit2223$model_edit2)
levels(weighted_US_weights_edit2223$model_edit2) <- c('', LETTERS[1:22])


panelA<-weighted_US_weights_edit2223%>% 
  ggplot(aes(x = forecast_date_current, y = weight, fill = model_edit,label = model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_17)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Median (4 Week) National Ensemble, 22—23')



#State


names(weighted_state_weights22)[4]<-"ensemble_type"
weighted_state_weights22$weight<-as.numeric(weighted_state_weights22$weight)

weighted_state_weights_edit<-weighted_state_weights22
weighted_state_weights_edit<-weighted_state_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))

viridris_11<-viridis(11)
viridris_11<-append(viridris_11,"grey",after=0)

weighted_state_weights_edit$model_edit<-lastlevel(as.factor(weighted_state_weights_edit$model_edit),"Models with < 10% weight" )
weighted_state_weights_edit2223<-weighted_state_weights_edit%>% filter(ensemble_type=='Weighted Median ensemble state2')


weighted_state_weights_edit2223$model_edit2<-weighted_state_weights_edit2223$model_edit
weighted_state_weights_edit2223$model_edit2<-factor(weighted_state_weights_edit2223$model_edit2)
levels(weighted_state_weights_edit2223$model_edit2) <- c('', LETTERS[1:22])

panelC<-weighted_state_weights_edit2223%>% 
  ggplot(aes(x = forecast_date_current, y = weight, fill = model_edit,label = model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_11)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Median (2 Week) State/Territory Ensemble, 22—23')



library(patchwork)

panelA/panelC



tiff("./Supplemental Figure 1 Weights 2022.tiff",units="in", width=10, height=8, res=300)
(panelA/panelC)
dev.off()


#US Overall

names(weighted_US_weights23)[4]<-"ensemble_type"
weighted_US_weights23$weight<-as.numeric(weighted_US_weights23$weight)

weighted_US_weights_edit<-weighted_US_weights23
weighted_US_weights_edit<-weighted_US_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))


viridris_12<-viridis(19)
viridris_12<-append(viridris_12,"grey",after=0)

weighted_US_weights_edit$model_edit<-lastlevel(as.factor(weighted_US_weights_edit$model_edit),"Models with < 10% weight" )

weighted_US_weights_edit2324<-weighted_US_weights_edit%>% filter(ensemble_type=='Weighted Median ensemble US6')

weighted_US_weights_edit2324$model_edit2<-weighted_US_weights_edit2324$model_edit
weighted_US_weights_edit2324$model_edit2<-factor(weighted_US_weights_edit2324$model_edit2)
levels(weighted_US_weights_edit2324$model_edit2) <- c('', LETTERS[1:22])




panelB<-weighted_US_weights_edit2324%>% ggplot(aes(x = current_ref_date, y = weight, fill = model_edit,label=model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_12)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Median (6 Week) National Ensemble, 23—24')




#State


names(weighted_state_weights23)[4]<-"ensemble_type"
weighted_state_weights23$weight<-as.numeric(weighted_state_weights23$weight)

weighted_state_weights_edit<-weighted_state_weights23



viridris_13<-viridis(13)
viridris_13<-append(viridris_13,"grey",after=0)


weighted_state_weights_edit<-weighted_state_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))
weighted_state_weights_edit$model_edit<-lastlevel(as.factor(weighted_state_weights_edit$model_edit),"Models with < 10% weight" )

weighted_state_weights_edit2324<-weighted_state_weights_edit%>% filter(ensemble_type=='Weighted mean ensemble state6')

weighted_state_weights_edit2324$model_edit2<-weighted_state_weights_edit2324$model_edit
weighted_state_weights_edit2324$model_edit2<-factor(weighted_state_weights_edit2324$model_edit2)
levels(weighted_state_weights_edit2324$model_edit2) <- c('', LETTERS[1:22])



panelD<-weighted_state_weights_edit2324%>% ggplot(aes(x = current_ref_date, y = weight, fill = model_edit, label=model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_13)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Mean (6 Week) State/Territory Ensemble, 23—24')


tiff("./Supplemental Figure 2 Weights 2023.tiff",units="in", width=10, height=8, res=300)
(panelB/panelD)
dev.off()





############################# 24/25
#2024-2025

median_value<-weighted_US_forecasts_all_2425%>%filter(output_type_id==.5)%>%  dplyr::rename(median=value, forecast_date=reference_date)%>%dplyr::select(!output_type_id)
ucl95_value<-weighted_US_forecasts_all_2425%>%filter(output_type_id==.95)%>%  dplyr::rename(ucl95=value, forecast_date=reference_date)%>%dplyr::select(!output_type_id)
lcl95_value<-weighted_US_forecasts_all_2425%>%filter(output_type_id==.05)%>%  dplyr::rename(lcl95=value, forecast_date=reference_date)%>%dplyr::select(!output_type_id)
ucl50_value<-weighted_US_forecasts_all_2425%>%filter(output_type_id==.75)%>%  dplyr::rename(ucl50=value, forecast_date=reference_date)%>%dplyr::select(!output_type_id)
lcl50_value<-weighted_US_forecasts_all_2425%>%filter(output_type_id==.25)%>%  dplyr::rename(lcl50=value, forecast_date=reference_date)%>%dplyr::select(!output_type_id)


plotting_value<-median_value%>%left_join(ucl95_value, by=c("model_id", "forecast_date", "target_end_date"))%>%left_join(lcl95_value, by=c("model_id", "forecast_date","target_end_date"))%>%
  left_join(ucl50_value, by=c("model_id", "forecast_date", "target_end_date"))%>%left_join(lcl50_value, by=c("model_id", "forecast_date","target_end_date"))%>%unique()


plotting_value_edit<-plotting_value%>%filter(model_id=='Weighted mean ensemble US2' |model_id=='FluSight-ensemble' )
plotting_value_edit$model_id<-factor(plotting_value_edit$model_id,
                                     levels = c('flu-truth','FluSight-ensemble',
                                                'Weighted mean ensemble US2'),
                                     labels=c('Truth', 'Untrained median', 'Mean (2 week)')  )
plotting_value_edit$forecast_date<-as.Date(plotting_value_edit$forecast_date)


plotting_value_mult<-plotting_value_edit%>%
  rbind.fill(flu_truth_plot)%>%
  mutate(model_id=factor(model_id, levels=c('Truth', 'Untrained median', 'Mean (2 week)'))) %>% filter(target_end_date<as.Date("2025-06-15"))

spec14 <- plotting_value_mult %>% filter(forecast_date=="2025-02-01")

plot_forecast3 <-plotting_value_mult%>%
  filter(forecast_date=="2025-01-04"|forecast_date=="2025-02-01" |forecast_date=="2025-03-01"| 
           forecast_date=="2025-03-29"|forecast_date=="2025-04-26"|forecast_date=="2025-05-24"|
           model_id=='Truth')%>%
  ggplot(aes(y =median,x=target_end_date, col=model_id,fill=model_id,group = interaction(model_id, forecast_date)))+
  geom_ribbon(aes(ymin=lcl95,ymax=ucl95),alpha=0.2,colour = NA, show.legend = FALSE,linewidth=1)+
  scale_color_manual(values=c('black','dodgerblue3',"darkgoldenrod"))+
  scale_fill_manual(values=c(NA,'dodgerblue3',"darkgoldenrod"))+
  xlab("Date") +geom_ribbon(aes(ymin=lcl50,ymax=ucl50),alpha=0.5, colour = NA, show.legend = FALSE,linewidth=1)+geom_line(aes(linetype = model_id))+
  geom_point(size=2.5)+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+scale_shape_manual(values = c(1,2,2))+
  ylab("Hospital admissions")+ylim(c(0,82000))+theme_bw()+
  theme(legend.key = element_blank(), legend.title = element_blank(),
        panel.border = element_blank(),axis.line = element_line(color = 'black'), legend.position = c(0.85, 0.9),)+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y",limits = c(as.Date('2024-11-22'),as.Date('2025-06-25'))) 

library(dplyr)
library(ggplot2)

# Make sure dates are Dates
plotting_value_mult <- plotting_value_mult %>%
  mutate(
    forecast_date   = as.Date(forecast_date),
    target_end_date = as.Date(target_end_date)
  )

# Earliest forecast date for non-Truth models in this subset
first_fc_2425 <- plotting_value_mult %>%
  filter(model_id != "Truth") %>%
  filter(forecast_date %in% as.Date(c("2025-01-04","2025-02-01","2025-03-01","2025-03-29","2025-04-26","2025-05-24"))) %>%
  summarise(first_fc = min(forecast_date, na.rm = TRUE)) %>%
  pull(first_fc)

plot_forecast3 <- plotting_value_mult %>%
  filter(forecast_date %in% as.Date(c("2025-01-04","2025-02-01","2025-03-01","2025-03-29","2025-04-26","2025-05-24")) |model_id == "Truth") %>%
  ggplot(aes(y = median, x = target_end_date, col = model_id, fill = model_id,group = interaction(model_id, forecast_date))) +
  annotate("rect",xmin = as.Date("2024-11-22"), xmax = as.Date("2024-12-28"),ymin = -Inf, ymax = Inf,alpha = 0.08, fill = "grey50") +
  annotate("text",x = as.Date("2024-11-26"), y = 60000,label = "Initial Training \n    Period", hjust = 0, size = 3, color = "grey30") +
  geom_ribbon(aes(ymin = lcl95, ymax = ucl95), alpha = 0.2, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_ribbon(aes(ymin = lcl50, ymax = ucl50), alpha = 0.5, colour = NA, show.legend = FALSE, linewidth = 1) +
  geom_line(aes(linetype = model_id)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("black", "dodgerblue3", "darkgoldenrod")) +
  scale_fill_manual(values = c(NA, "dodgerblue3", "darkgoldenrod")) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  xlab("Date") +ylab("Hospital admissions") +
  ylim(c(0, 82000)) +
  theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black"), legend.position = c(0.85, 0.9)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y", limits = c(as.Date("2024-11-22"), as.Date("2025-06-25")))



plot_forecast3

tiff("./Figures/Figure 3 2425.tiff",units="in", width=6, height=4, res=300)
plot_forecast3
dev.off()

#Looking at weights over time`
library(viridis)

#US overall
names(weighted_US_weights24)[4]<-"ensemble_type"
weighted_US_weights24$weight<-as.numeric(weighted_US_weights24$weight)

weighted_US_weights_edit<-weighted_US_weights24
weighted_US_weights_edit<-weighted_US_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))

viridris_19<-viridis(19)
viridris_19<-append(viridris_19,"grey",after=0)

lastlevel = function (f, last, ...) {
  if (!is.factor(f)) stop("f must be a factor")
  lev <- levels(f)
  if (length(last) != 1L) 
    stop("'last' must be of length one")
  if (is.character(last)) 
    last <- match(last, lev)
  if (is.na(last)) 
    stop("'last' must be an existing level")
  nlev <- length(lev)
  if (last < 1 || last > nlev) 
    stop(gettextf("last = %d must be in 1L:%d", last, nlev), 
         domain = NA)
  factor(f, levels = lev[c(last, seq_along(lev)[-last])])
}
weighted_US_weights_edit$model_edit<-lastlevel(as.factor(weighted_US_weights_edit$model_edit),"Models with < 10% weight" )
weighted_US_weights_edit2425<-weighted_US_weights_edit%>% filter(ensemble_type=='Weighted mean ensemble US2')

weighted_US_weights_edit2425$model_edit2<-weighted_US_weights_edit2425$model_edit
weighted_US_weights_edit2425$model_edit2<-factor(weighted_US_weights_edit2425$model_edit2)
levels(weighted_US_weights_edit2425$model_edit2) <- c('', LETTERS[1:22])


panelA24<-weighted_US_weights_edit2425%>% 
  ggplot(aes(x = current_ref_date, y = weight, fill = model_edit,label = model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_19)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Mean (2 Week) National Ensemble, 24—25')



#State


names(weighted_state_weights24)[4]<-"ensemble_type"
weighted_state_weights24$weight<-as.numeric(weighted_state_weights24$weight)

weighted_state_weights_edit<-weighted_state_weights24
weighted_state_weights_edit<-weighted_state_weights_edit%>% mutate(model_edit=ifelse(weight<.1,"Models with < 10% weight",model_id))

viridris_16<-viridis(16)
viridris_16<-append(viridris_16,"grey",after=0)

weighted_state_weights_edit$model_edit<-lastlevel(as.factor(weighted_state_weights_edit$model_edit),"Models with < 10% weight" )
weighted_state_weights_edit2425<-weighted_state_weights_edit%>% filter(ensemble_type=='Weighted Median ensemble state2')


weighted_state_weights_edit2425$model_edit2<-weighted_state_weights_edit2425$model_edit
weighted_state_weights_edit2425$model_edit2<-factor(weighted_state_weights_edit2425$model_edit2)
levels(weighted_state_weights_edit2425$model_edit2) <- c('', LETTERS[1:22])

panelC24<-weighted_state_weights_edit2425%>% 
  ggplot(aes(x = current_ref_date, y = weight, fill = model_edit,label = model_edit2)) +
  geom_col(position = "fill")+scale_fill_manual(values = viridris_16)+xlab("")+ guides(fill=guide_legend(title="Model"))+theme_classic()+
  theme(legend.key.size = unit(.05, 'in'),legend.position = "none")+geom_text(position = position_stack(vjust = 0.5),fontface = "bold")+
  ggtitle('Median (2 Week) State/Territory Ensemble, 24—25')



panelC24

library(patchwork)

panelA24/panelC24



tiff("./Figures/Supplemental Figure 3 Weights 2024.tiff",units="in", width=10, height=8, res=300)
(panelA24/panelC24)
dev.off()







# 1. Confirm the date class and unique values around Feb 22
class(weighted_state_weights_edit2425$current_ref_date)
min(weighted_state_weights_edit2425$current_ref_date, na.rm = TRUE)
max(weighted_state_weights_edit2425$current_ref_date, na.rm = TRUE)
unique(sort(weighted_state_weights_edit2425$current_ref_date))[1:50]  # inspect

# 2. Print rows for the problematic date (replace with exact date format if needed)
target_date <- as.Date("2025-02-22")   # change year if different
weighted_state_weights_edit2425 %>%
  filter(current_ref_date == target_date) %>%
  arrange(model_id) 

# 3. Check for NA or zero weights on that date
weighted_state_weights_edit2425 %>%
  filter(current_ref_date == target_date) %>%
  summarise(
    n = n(),
    n_na = sum(is.na(weight)),
    n_zero = sum(weight == 0, na.rm = TRUE),
    sum_weight = sum(weight, na.rm = TRUE)
  )

# 4. Check ensemble_type exact values and whitespace
unique(weighted_state_weights24$ensemble_type)
unique(trimws(weighted_state_weights24$ensemble_type))  # see if stray spaces

# 5. Check how many distinct fill levels and colors you provide
n_levels <- length(unique(weighted_state_weights_edit2425$model_edit))
n_colors <- length(viridris_19)
n_levels; n_colors

# Plot absolute stacked bar (not fill) to inspect actual weights
weighted_state_weights_edit2425 %>%
  ggplot(aes(x = current_ref_date, y = weight, fill = model_edit)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = viridris_20) +
  theme_classic() +
  ggtitle("Stacked weights (absolute) — check Feb 22")

ggplot(weighted_state_weights_edit2425, aes(current_ref_date, weight, fill = model_edit)) +
  geom_col(position = "stack") +
  theme_classic()


target_date <- as.Date("2025-02-22")

weighted_state_weights_edit2425 %>%
  filter(current_ref_date == target_date) %>%
  select(model_id, weight) %>%
  arrange(desc(weight))
