
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(janitor)
library(ggrepel)
library(rgdal)
library(scales)
library(glatos)
library(ggmap)
library(ggpubr)
library(mapproj)







##### Predation assessment######  

#Read in files

Telemetry_master_escapees_pred<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/PB_telem_master_escapee_2021_Dec 8 2022.csv") %>% print()
#303,570 x 32  



Telemetry_master_escapees_pred.1 <- Telemetry_master_escapees_pred %>%
  filter(sensor == "temp") %>% print()

#151,366 x 32  


#Identify max temp values in each fish:

Max_temps_escapee <- Telemetry_master_escapees_pred.1 %>% 
  group_by(serial) %>% summarise(max_temp = max(sensor_value)) %>% 
  arrange(max_temp) %>% 
  print()
#98 x 2  

#next, want to identify possible endotherm and regional endotherm preds
#for true endotherms (ie mammals/birds), setting lower limit of 30C
#for tuna and sharks, temps appear to be in the high teens and 20's.In some tuna, 
#can be as high as 30C
#https://journals.biologists.com/jeb/article/109/1/1/4154/Bluefin-Tuna-Warm-Their-Viscera-During-Digestion
#https://journals.physiology.org/doi/abs/10.1152/ajpregu.1984.246.4.r487
#https://www.journals.uchicago.edu/doi/abs/10.1086/physzool.54.3.30159948
#https://link.springer.com/article/10.1007/s10641-004-6588-9

#So for these fish, setting limits of >30, <18C
#Anything else is below 18C

#good example of the loop needed: https://stackoverflow.com/questions/15016723/create-categories-by-comparing-a-numeric-column-with-a-fixed-value

Max_temps_escapee.1<-Max_temps_escapee %>% 
  mutate(temp_cat = case_when(max_temp >= 30 ~ 'high',
                              max_temp >= 18  ~ 'mid',
                              TRUE ~ 'low')) %>% print()

#case_when appears to work through visual inspection 

Max_temps_escapee.1 %>% group_by(temp_cat) %>% summarise(n = n())

# # A tibble: 3 x 2
# temp_cat     n
# <chr>    <int>
# 1 high        68
# 2 low         25
# 3 mid          5
# >  

#most fish eaten by marine mammals it would seem

#Make a quick summary table for the results:
write_csv(Max_temps_escapee.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Predation_temp_categroies_2021_Dec 9 2022.csv')

#join that info in for ease of filtering

Telemetry_master_escapees_pred.2 <-left_join(Telemetry_master_escapees_pred.1,Max_temps_escapee.1) %>% print()

#151,366 x 35

write_csv(Telemetry_master_escapees_pred.2, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Predation_master_file_2021_Dec 9 2022.csv')

####Endotheric predators####

#of first interest, want to see whats going on with the mammal/bird predated fish
#Filter out tags with the 'high' category as those were all fish with max values
#greater than 30C 

Escapees_high <-Telemetry_master_escapees_pred.2 %>% 
  filter(temp_cat == 'high') %>% print()
#124,318 x 35

#couple checks to make sure it all looks good

range(Escapees_high$sensor_value)
#[1]  2.4030 35.0055

range(Escapees_high$max_temp)
#[1] 31.3830 35.0055

length(unique(Escapees_high$serial))
#68, matches with orginal max temp stuff 

List<-unique(Escapees_high$serial) %>% print()

List<-as_tibble(List) %>% rename(serial = value) %>% print()
#68 x 1

write_csv(List, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Endotherm_pred_list_2021_Dec 9 2022.csv')

windowsFonts(Times=windowsFont("TT Times New Roman")) 


##### Time to predation event

#library(strucchange)
#Appears that we're going to use 'strucchange'
#Based on this https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/


unique(Escapees_high$serial)

# [1] 1392803 1392805 1392806 1392809 1392810 1392816 1392817 1392819 1392821 1392823 1392825 1392826 1392827 1392828 1392829 1392830 1392831 1392832 1392833 1392834
# [21] 1392836 1392837 1392838 1392839 1392841 1392842 1392843 1392844 1392845 1392846 1392847 1392849 1392850 1392851 1392852 1392853 1392854 1392855 1392856 1392857
# [41] 1392858 1392859 1392860 1392861 1392862 1392863 1392865 1392867 1392868 1392871 1392872 1392873 1392876 1392877 1392879 1392881 1392883 1392884 1392886 1392887
# [61] 1392889 1392890 1392892 1392893 1392895 1392896 1392897 1392898
# > 

length(unique(Escapees_high$serial))

#will be doing each fish as a breakpoint analysis to determine time of predation


####1392803 serial ID######


#first, filter out desired fish and just get core metrics
A787<-Escapees_high %>% filter(serial == '1392803') %>% 
  arrange(date_time) %>% print()
#85 x 30
A787.1<- A787
A787.1$date_time <-as.numeric(A787.1$date_time)
A787.1

#inflection point analysis for when the fish was eaten
A787_result<-inflection::findiplist(A787.1$date_time,A787.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392803') %>% print() 

#convert back to real time
A787_result$pred_time <-as_datetime(A787_result$pred_time)
A787_result$serial <-as.double(A787_result$serial)

A787_result


A787_combine<-A787 %>% left_join(A787_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#168 x 33

A787_combine$surv_duration

A787_combine.1 <-A787_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()

view(A787)





#simple plot to visually confirm

ggplot(A787_combine.1, aes (x = date_time, y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  geom_vline(xintercept=A787_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  annotate('text', x = as.POSIXct('2021-09-21 23:00:00'), 
           y = 35, label = "1392803", fontface ="bold", size = 4.5) 

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392803_endo_feb 15 2022.tiff",
       dpi = 300)


####1392805 serial ID######

#first, filter out desired fish and just get core metrics
B213<-Escapees_high %>% filter(serial == '1392805') %>% 
  arrange(date_time) %>% print()
#2,069 x 30
B213.1<- B213
B213.1$date_time <-as.numeric(B213.1$date_time)
B213.1

#inflection point analysis for when the fish was eaten
B213_result<-inflection::findiplist(B213.1$date_time,B213.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392805') %>% print() 

#convert back to real time
B213_result$pred_time <-as_datetime(B213_result$pred_time)
B213_result$serial <-as.double(B213_result$serial)

B213_result


B213_combine<-B213 %>% left_join(B213_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#4138 x 33

B213_combine$surv_duration

B213_combine.1 <-B213_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
B213_combine.1

ggplot(B213_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=B213_combine$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 12:00:00'), 
           y = 35, label = "1392805", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392805 _endo_feb 15 2022.tiff",
       dpi = 300)



####1392806 serial ID######

#first, filter out desired fish and just get core metrics
C224<-Escapees_high %>% filter(serial == '1392806') %>% 
  arrange(date_time) %>% print()
#263 x 30
C224.1<- C224
C224.1$date_time <-as.numeric(C224.1$date_time)
C224.1

view(C224)

#inflection point analysis for when the fish was eaten
C224_result<-inflection::findiplist(C224.1$date_time,C224.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392806') %>% print() 

#convert back to real time
C224_result$pred_time <-as_datetime(C224_result$pred_time)
C224_result$serial <-as.double(C224_result$serial)

C224_result


C224_combine<-C224 %>% left_join(C224_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#526 x 33

C224_combine$surv_duration

C224_combine.1 <-C224_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
C224_combine.1

ggplot(C224_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=C224_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-02 23:00:00'), 
           y = 30, label = "1392806", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("3 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392806 _endo_feb 15 2022.tiff",
       dpi = 300)





####1392809 serial ID######

#first, filter out desired fish and just get core metrics
D353<-Escapees_high %>% filter(serial == '1392809') %>% 
  arrange(date_time) %>% print()
#933 x 30
D353.1<- D353
D353.1$date_time <-as.numeric(D353.1$date_time)
D353.1
view(D353)

#inflection point analysis for when the fish was eaten
D353_result<-inflection::findiplist(D353.1$date_time,D353.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392809') %>% print() 

#convert back to real time
D353_result$pred_time <-as_datetime(D353_result$pred_time)
D353_result$serial <-as.double(D353_result$serial)

D353_result


D353_combine<-D353 %>% left_join(D353_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1866 x 33

D353_combine$surv_duration

D353_combine.1 <-D353_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
D353_combine.1

ggplot(D353_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=D353_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 12:00:00'), 
           y = 35, label = "1392809", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392809 _endo_feb 15 2022.tiff",
       dpi = 300)




####1392810 serial ID######

#first, filter out desired fish and just get core metrics
E456<-Escapees_high %>% filter(serial == '1392810') %>% 
  arrange(date_time) %>% print()
#593 x 30
E456.1<- E456
E456.1$date_time <-as.numeric(E456.1$date_time)
E456.1
view(E456)

#inflection point analysis for when the fish was eaten
E456_result<-inflection::findiplist(E456.1$date_time,E456.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392810') %>% print() 

#convert back to real time
E456_result$pred_time <-as_datetime(E456_result$pred_time)
E456_result$serial <-as.double(E456_result$serial)

E456_result


E456_combine<-E456 %>% left_join(E456_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1186 x 33

E456_combine$surv_duration

E456_combine.1 <-E456_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
E456_combine.1

ggplot(E456_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=E456_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 12:00:00'), 
           y = 35, label = "1392810", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392810 _endo_feb 15 2022.tiff",
       dpi = 300)





####1392816 serial ID######

#first, filter out desired fish and just get core metrics
F367<-Escapees_high %>% filter(serial == '1392816') %>% 
  arrange(date_time) %>% print()
#315 x 30
F367.1<- F367
F367.1$date_time <-as.numeric(F367.1$date_time)
F367.1
view(F367)

#inflection point analysis for when the fish was eaten
F367_result<-inflection::findiplist(F367.1$date_time,F367.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392816') %>% print() 

#convert back to real time
F367_result$pred_time <-as_datetime(F367_result$pred_time)
F367_result$serial <-as.double(F367_result$serial)

F367_result


F367_combine<-F367 %>% left_join(F367_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#630 x 33


F367_combine.1 <-F367_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
F367_combine.1

ggplot(F367_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=F367_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-23 12:00:00'), 
           y = 35, label = "1392816", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392816 _endo_feb 15 2022.tiff",
       dpi = 300)



####1392817 serial ID######

#first, filter out desired fish and just get core metrics
g112<-Escapees_high %>% filter(serial == '1392817') %>% 
  arrange(date_time) %>% print()
#606 x 30
g112.1<- g112
g112.1$date_time <-as.numeric(g112.1$date_time)
g112.1

view(g112)

#inflection point analysis for when the fish was eaten
g112_result<-inflection::findiplist(g112.1$date_time,g112.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392817') %>% print() 

#convert back to real time
g112_result$pred_time <-as_datetime(g112_result$pred_time)
g112_result$serial <-as.double(g112_result$serial)

g112_result


g112_combine<-g112 %>% left_join(g112_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1212 x 33


g112_combine.1 <-g112_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
g112_combine.1

ggplot(g112_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=g112_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 18:00:00'), 
           y = 35, label = "1392817", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392817 _endo_feb 15 2022.tiff",
       dpi = 300)



####1392819 serial ID######

#first, filter out desired fish and just get core metrics
H777<-Escapees_high %>% filter(serial == '1392819') %>% 
  arrange(date_time) %>% print()
#351 x 30
H777.1<- H777
H777.1$date_time <-as.numeric(H777.1$date_time)
H777.1

view(H777)

#inflection point analysis for when the fish was eaten
H777_result<-inflection::findiplist(H777.1$date_time,H777.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392819') %>% print() 

#convert back to real time
H777_result$pred_time <-as_datetime(H777_result$pred_time)
H777_result$serial <-as.double(H777_result$serial)

H777_result


H777_combine<-H777 %>% left_join(H777_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#702 x 33


H777_combine.1 <-H777_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
H777_combine.1

ggplot(H777_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=H777_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 12:00:00'), 
           y = 35, label = "1392819", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392819 _endo_feb 15 2022.tiff",
       dpi = 300)



####1392821 serial ID######

#first, filter out desired fish and just get core metrics
I187<-Escapees_high %>% filter(serial == '1392821') %>% 
  arrange(date_time) %>% print()
#602 x 30
I187.1<- I187
I187.1$date_time <-as.numeric(I187.1$date_time)
I187.1

view(I187)
#inflection point analysis for when the fish was eaten
I187_result<-inflection::findiplist(I187.1$date_time,I187.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392821') %>% print() 

#convert back to real time
I187_result$pred_time <-as_datetime(I187_result$pred_time)
I187_result$serial <-as.double(I187_result$serial)

I187_result


I187_combine<-I187 %>% left_join(I187_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1204 x 33


I187_combine.1 <-I187_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
I187_combine.1

ggplot(I187_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=I187_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-26 06:00:00'), 
           y = 25, label = "1392821", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392821 _endo_feb 15 2022.tiff",
       dpi = 300)





####1392823 serial ID######

#first, filter out desired fish and just get core metrics
J684<-Escapees_high %>% filter(serial == '1392823') %>% 
  arrange(date_time) %>% print()
#111 x 30
J684.1<- J684
J684.1$date_time <-as.numeric(J684.1$date_time)
J684.1

#inflection point analysis for when the fish was eaten
J684_result<-inflection::findiplist(J684.1$date_time,J684.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392823') %>% print() 

#convert back to real time
J684_result$pred_time <-as_datetime(J684_result$pred_time)
J684_result$serial <-as.double(J684_result$serial)

J684_result


J684_combine<-J684 %>% left_join(J684_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#222 x 33


J684_combine.1 <-J684_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
J684_combine.1

ggplot(J684_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=J684_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 3:00:00'), 
           y = 35, label = "1392823", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392823 _endo_feb 15 2022.tiff",
       dpi = 300)



####1392825 serial ID######

#first, filter out desired fish and just get core metrics
K795<-Escapees_high %>% filter(serial == '1392825') %>% 
  arrange(date_time) %>% print()
#4201 x 30

view(K795)
#NOTE FOR THIS PLOT ONLY:
#Had what appeared to look like the tag was expelled from pred at 
#2021-09-26 18:20:40, first non 35C temp (back to 14C)

#Filtering everything below that

K795.1 <-K795 %>% filter(date_time < as_datetime('2021-09-26 18:20:40')) %>% print()
#519 x 30
K795.2<-K795.1

K795.2$date_time <-as.numeric(K795.2$date_time)
K795.2


#inflection point analysis for when the fish was eaten
K795_result<-inflection::findiplist(K795.2$date_time,K795.2$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392825') %>% print() 

#convert back to real time
K795_result$pred_time <-as_datetime(K795_result$pred_time)
K795_result$serial <-as.double(K795_result$serial)

K795_result


K795_combine<-K795.1 %>% left_join(K795_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1038 x 33


K795_combine.1 <-K795_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
K795_combine.1

ggplot(K795_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=K795_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 23:00:00'), 
           y = 25, label = "1392825", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392825 _endo_feb 15 2022.tiff",
       dpi = 300)


####1392826 serial ID######

#first, filter out desired fish and just get core metrics
L117<-Escapees_high %>% filter(serial == '1392826') %>% 
  arrange(date_time) %>% print()

#NOTE FOR THIS PLOT ONLY
#tag seemed to be explled at 2021-09-26 14:03:52 as temp goes from 35C to 14C

L117.1 <- L117%>% filter(date_time < as_datetime('2021-09-26 10:42:22')) %>% print()
#1,928 x 30


L117.2<- L117.1
L117.2$date_time <-as.numeric(L117.2$date_time)
L117.2

#inflection point analysis for when the fish was eaten
L117_result<-inflection::findiplist(L117.2$date_time,L117.2$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392826') %>% print() 

#convert back to real time
L117_result$pred_time <-as_datetime(L117_result$pred_time)
L117_result$serial <-as.double(L117_result$serial)

L117_result


L117_combine<-L117.1 %>% left_join(L117_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#3856 x 33


L117_combine.1 <-L117_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
L117_combine.1

ggplot(L117_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=L117_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 6:00:00'), 
           y = 35, label = "1392826", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392826 _endo_feb 15 2022.tiff",
       dpi = 300)






####1392827 serial ID######

#first, filter out desired fish and just get core metrics
M270<-Escapees_high %>% filter(serial == '1392827') %>% 
  arrange(date_time) %>% print()


#NOTE FOR THIS PLOT ONLY
#tag seemed to be explled at 2021-09-26 14:31:43 as temp goes from 35C to 14C

M270.1 <- M270%>% filter(date_time < as_datetime('2021-09-26 14:31:43')) %>% print()
#503 X 30


#7417 x 30
M270.2<- M270.1
M270.2$date_time <-as.numeric(M270.2$date_time)
M270.2

#inflection point analysis for when the fish was eaten
M270_result<-inflection::findiplist(M270.2$date_time,M270.2$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392827') %>% print() 

#convert back to real time
M270_result$pred_time <-as_datetime(M270_result$pred_time)
M270_result$serial <-as.double(M270_result$serial)

M270_result


M270_combine<-M270.1 %>% left_join(M270_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1,006 x 33


M270_combine.1 <-M270_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
M270_combine.1

ggplot(M270_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=M270_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 0:00:00'), 
           y = 25, label = "1392827", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392827 _endo_feb 15 2022.tiff",
       dpi = 300)






####1392828 serial ID######

#first, filter out desired fish and just get core metrics
N009<-Escapees_high %>% filter(serial == '1392828') %>% 
  arrange(date_time) %>% print()
#406 x 30
N009.1<- N009
N009.1$date_time <-as.numeric(N009.1$date_time)
N009.1

#inflection point analysis for when the fish was eaten
N009_result<-inflection::findiplist(N009.1$date_time,N009.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392828') %>% print() 

#convert back to real time
N009_result$pred_time <-as_datetime(N009_result$pred_time)
N009_result$serial <-as.double(N009_result$serial)

N009_result


N009_combine<-N009 %>% left_join(N009_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#812 x 33


N009_combine.1 <-N009_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
N009_combine.1

ggplot(N009_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=N009_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 12:00:00'), 
           y = 35, label = "1392828", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392828 _endo_feb 15 2022.tiff",
       dpi = 300)


####1392829 serial ID######



#first, filter out desired fish and just get core metrics
O123<-Escapees_high %>% filter(serial == '1392829') %>%
  arrange(date_time) %>% print()
#4310 x 35
#EXPELLED

O123.F<- O123%>% filter(date_time < as_datetime('2021-09-26 12:12:33')) %>% print()

O123.1 <-O123.F

O123.1$date_time <-as.numeric(O123.1$date_time)
O123.1

# #inflection point analysis for when the fish was eaten
O123_result<-inflection::findiplist(O123.1$date_time,O123.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392829') %>% print()

#convert back to real time
O123_result$pred_time <-as_datetime(O123_result$pred_time)
O123_result$serial <-as.double(O123_result$serial)

O123_result


O123_combine<-O123.F %>% left_join(O123_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#200 x 33


O123_combine.1 <-O123_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(sensor_value > 20 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
O123_combine.1

ggplot(O123_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=O123_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 3:00:00'),
           y = 25, label = "1392829", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392829 _endo_feb 15 2022.tiff",
       dpi = 300)





###1392830 serial ID######

#first, filter out desired fish and just get core metrics
P329<-Escapees_high %>% filter(serial == '1392830') %>% 
  arrange(date_time) %>% print()
#653 x 30
P329.1<- P329
P329.1$date_time <-as.numeric(P329.1$date_time)
P329.1

#inflection point analysis for when the fish was eaten
P329_result<-inflection::findiplist(P329.1$date_time,P329.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392830') %>% print() 

#convert back to real time
P329_result$pred_time <-as_datetime(P329_result$pred_time)
P329_result$serial <-as.double(P329_result$serial)

P329_result


P329_combine<-P329 %>% left_join(P329_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1218 x 33


P329_combine.1 <-P329_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
P329_combine.1

ggplot(P329_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=P329_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 0:00:00'), 
           y = 35, label = "1392830", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392830 _endo_feb 15 2022.tiff",
       dpi = 300)




###1392831 serial ID######

#first, filter out desired fish and just get core metrics
Q675<-Escapees_high %>% filter(serial == '1392831') %>% 
  arrange(date_time) %>% print()
#3610 x 30

#EXPELLED!!!!

Q675.F <-Q675 %>% filter(date_time < as_datetime('2021-10-01 20:18:54')) %>% print()
#647 33

Q675.1<- Q675.F
Q675.1$date_time <-as.numeric(Q675.1$date_time)
Q675.1

#inflection point analysis for when the fish was eaten
Q675_result<-inflection::findiplist(Q675.1$date_time,Q675.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392831') %>% print() 

#convert back to real time
Q675_result$pred_time <-as_datetime(Q675_result$pred_time)
Q675_result$serial <-as.double(Q675_result$serial)

Q675_result


Q675_combine<-Q675.F %>% left_join(Q675_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1202 x 33


Q675_combine.1 <-Q675_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
Q675_combine.1

ggplot(Q675_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=Q675_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 12:00:00'), 
           y = 25, label = "1392831", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392831 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392832 serial ID######

#first, filter out desired fish and just get core metrics
R445<-Escapees_high %>% filter(serial == '1392832') %>% 
  arrange(date_time) %>% print()
#3385 x 30

#EXPELLED!!!!

R445.F <-R445 %>% filter(date_time < as_datetime('2021-09-26 14:36:08')) %>% print()
#593 33

R445.1<- R445.F
R445.1$date_time <-as.numeric(R445.1$date_time)
R445.1

#inflection point analysis for when the fish was eaten
R445_result<-inflection::findiplist(R445.1$date_time,R445.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392832') %>% print() 

#convert back to real time
R445_result$pred_time <-as_datetime(R445_result$pred_time)
R445_result$serial <-as.double(R445_result$serial)

R445_result


R445_combine<-R445.F %>% left_join(R445_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1186 x 33


R445_combine.1 <-R445_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
R445_combine.1

ggplot(R445_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=R445_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 0:00:00'), 
           y = 35, label = "1392832", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392832 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392833 serial ID######

#first, filter out desired fish and just get core metrics
S979<-Escapees_high %>% filter(serial == '1392833') %>% 
  arrange(date_time) %>% print()
#513 x 30


S979.1<- S979
S979.1$date_time <-as.numeric(S979.1$date_time)
S979.1

#inflection point analysis for when the fish was eaten
S979_result<-inflection::findiplist(S979.1$date_time,S979.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392833') %>% print() 

#convert back to real time
S979_result$pred_time <-as_datetime(S979_result$pred_time)
S979_result$serial <-as.double(S979_result$serial)

S979_result


S979_combine<-S979 %>% left_join(S979_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1026 x 33


S979_combine.1 <-S979_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
S979_combine.1

ggplot(S979_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=S979_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-26 0:00:00'), 
           y = 25, label = "1392833", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392833 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392834 serial ID######






#first, filter out desired fish and just get core metrics
T002<-Escapees_high %>% filter(serial == '1392834') %>% 
  arrange(date_time) %>% print()
#394 x 35


T002.F <-T002 %>% filter(date_time < as_datetime('2021-09-25 05:11:37')) %>% print()
#127 35



T002.1<- T002.F
T002.1$date_time <-as.numeric(T002.1$date_time)
T002.1

#inflection point analysis for when the fish was eaten
T002_result<-inflection::findiplist(T002.1$date_time,T002.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392834') %>% print()

#convert back to real time
T002_result$pred_time <-as_datetime(T002_result$pred_time)
T002_result$serial <-as.double(T002_result$serial)

T002_result


T002_combine<-T002.F %>% left_join(T002_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#254 x 33


T002_combine.1 <-T002_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>%
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
T002_combine.1

ggplot(T002_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=T002_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 6:00:00'),
           y = 25, label = "1392834", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392834 _endo_feb 15 2022.tiff",
       dpi = 300)




###1392836 serial ID######



#first, filter out desired fish and just get core metrics
U343<-Escapees_high %>% filter(serial == '1392836') %>% 
  arrange(date_time) %>% print()
#1901 x 35


U343.F <-U343 %>% filter(date_time < as_datetime('2021-09-26 10:37:08')) %>% print()

U343.1<- U343.F
U343.1$date_time <-as.numeric(U343.1$date_time)
U343.1

#inflection point analysis for when the fish was eaten
U343_result<-inflection::findiplist(U343.1$date_time,U343.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392836') %>% print()

#convert back to real time
U343_result$pred_time <-as_datetime(U343_result$pred_time)
U343_result$serial <-as.double(U343_result$serial)

U343_result


U343_combine<-U343.F %>% left_join(U343_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#1026 x 33


U343_combine.1 <-U343_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>%
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
U343_combine.1

ggplot(U343_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=U343_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 20:00:00'),
           y = 25, label = "1392836", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392836 _endo_feb 15 2022.tiff",
       dpi = 300)


###1392837 serial ID######

#NOTE ONLY A SINGLE 35C POINT, FISH DISPPEARS AFTER THAT
#CANNOT COMPUTE INFLECTION VALUE

#MANUALLY RECOLOURING FIGURE

#first, filter out desired fish and just get core metrics
V798<-Escapees_high %>% filter(serial == '1392837') %>% 
  arrange(date_time) %>% print()
#582 x 30


V798.1<- V798
V798.1$date_time <-as.numeric(V798.1$date_time)
V798.1

#inflection point analysis for when the fish was eaten
V798_result<-inflection::findiplist(V798.1$date_time,V798.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392837') %>% print() 

#convert back to real time
V798_result$pred_time <-as_datetime(V798_result$pred_time)
V798_result$serial <-as.double(V798_result$serial)

V798_result


V798_combine<-V798 %>% left_join(V798_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1026 x 33


V798_combine.1 <-V798_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value > 30 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
V798_combine.1

ggplot(V798_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=V798_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 23:00:00'), 
           y = 35, label = "1392837", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392837 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392838 serial ID######

#NOTE ONLY A 3X 30ISH C POINT, FISH DISPPEARS AFTER THAT
#CANNOT COMPUTE INFLECTION VALUE

#MANUALLY CHANGING COLOUR FOR PLOT

#first, filter out desired fish and just get core metrics
W667<-Escapees_high %>% filter(serial == '1392838') %>% 
  arrange(date_time) %>% print()
#1283 x 30


W667.1<- W667
W667.1$date_time <-as.numeric(W667.1$date_time)
W667.1

#inflection point analysis for when the fish was eaten
W667_result<-inflection::findiplist(W667.1$date_time,W667.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392838') %>% print() 

#convert back to real time
W667_result$pred_time <-as_datetime(W667_result$pred_time)
W667_result$serial <-as.double(W667_result$serial)

W667_result


W667_combine<-W667 %>% left_join(W667_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1026 x 33


W667_combine.1 <-W667_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value > 22 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
W667_combine.1

ggplot(W667_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=W667_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 14:00:00'), 
           y = 35, label = "1392838", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392838 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392839 serial ID######


#first, filter out desired fish and just get core metrics
X555<-Escapees_high %>% filter(serial == '1392839') %>% 
  arrange(date_time) %>% print()
#378 x 30


X555.1<- X555
X555.1$date_time <-as.numeric(X555.1$date_time)
X555.1

#inflection point analysis for when the fish was eaten
X555_result<-inflection::findiplist(X555.1$date_time,X555.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392839') %>% print() 

#convert back to real time
X555_result$pred_time <-as_datetime(X555_result$pred_time)
X555_result$serial <-as.double(X555_result$serial)

X555_result


X555_combine<-X555 %>% left_join(X555_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#756 x 33


X555_combine.1 <-X555_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
X555_combine.1

ggplot(X555_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=X555_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 6:00:00'), 
           y = 35, label = "1392839", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392839 _endo_feb 15 2022.tiff",
       dpi = 300)




###1392841 serial ID######




#first, filter out desired fish and just get core metrics
Y215<-Escapees_high %>% filter(serial == '1392841') %>% 
  arrange(date_time) %>% print()
#6,969 x 30

#EXPELLED!


Y215.F<-Y215 %>% filter(date_time < as_datetime('2021-09-26 14:08:24')) %>% print()
#363 X 30

Y215.1<- Y215.F
Y215.1$date_time <-as.numeric(Y215.1$date_time)
Y215.1

#inflection point analysis for when the fish was eaten
Y215_result<-inflection::findiplist(Y215.1$date_time,Y215.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392841') %>% print() 

#convert back to real time
Y215_result$pred_time <-as_datetime(Y215_result$pred_time)
Y215_result$serial <-as.double(Y215_result$serial)

Y215_result


Y215_combine<-Y215.F %>% left_join(Y215_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#726 x 33


Y215_combine.1 <-Y215_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
Y215_combine.1

ggplot(Y215_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=Y215_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 20:00:00'), 
           y = 25, label = "1392841", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392841 _endo_feb 15 2022.tiff",
       dpi = 300)




###1392842 serial ID######




#first, filter out desired fish and just get core metrics
AA1234<-Escapees_high %>% filter(serial == '1392842') %>% 
  arrange(date_time) %>% print()
#3099 x 30

#EXPELLED!

AA1234.F<-AA1234 %>% filter(date_time < as_datetime('2021-09-26 16:22:55')) %>% print()
#301 X 30

AA1234.1<- AA1234.F
AA1234.1$date_time <-as.numeric(AA1234.1$date_time)
AA1234.1

#inflection point analysis for when the fish was eaten
AA1234_result<-inflection::findiplist(AA1234.1$date_time,AA1234.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392842') %>% print() 

#convert back to real time
AA1234_result$pred_time <-as_datetime(AA1234_result$pred_time)
AA1234_result$serial <-as.double(AA1234_result$serial)

AA1234_result


AA1234_combine<-AA1234.F %>% left_join(AA1234_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#602 x 33


AA1234_combine.1 <-AA1234_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
AA1234_combine.1

ggplot(AA1234_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=AA1234_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 20:00:00'), 
           y = 25, label = "1392842", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392842 _endo_feb 15 2022.tiff",
       dpi = 300)



###1392843 serial ID######




#first, filter out desired fish and just get core metrics
BB4321<-Escapees_high %>% filter(serial == '1392843') %>% 
  arrange(date_time) %>% print()
#279 x 30


BB4321.1<- BB4321
BB4321.1$date_time <-as.numeric(BB4321.1$date_time)
BB4321.1

#inflection point analysis for when the fish was eaten
BB4321_result<-inflection::findiplist(BB4321.1$date_time,BB4321.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392843') %>% print() 

#convert back to real time
BB4321_result$pred_time <-as_datetime(BB4321_result$pred_time)
BB4321_result$serial <-as.double(BB4321_result$serial)

BB4321_result


BB4321_combine<-BB4321 %>% left_join(BB4321_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#558 x 33


BB4321_combine.1 <-BB4321_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
BB4321_combine.1

ggplot(BB4321_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=BB4321_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 16:00:00'), 
           y = 35, label = "1392843", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392843 _endo_feb 15 2022.tiff",
       dpi = 300)




###1392844 serial ID######




#first, filter out desired fish and just get core metrics
CC9876<-Escapees_high %>% filter(serial == '1392844') %>% 
  arrange(date_time) %>% print()
#332 x 30


CC9876.1<- CC9876
CC9876.1$date_time <-as.numeric(CC9876.1$date_time)
CC9876.1

#inflection point analysis for when the fish was eaten
CC9876_result<-inflection::findiplist(CC9876.1$date_time,CC9876.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392844') %>% print() 

#convert back to real time
CC9876_result$pred_time <-as_datetime(CC9876_result$pred_time)
CC9876_result$serial <-as.double(CC9876_result$serial)

CC9876_result


CC9876_combine<-CC9876 %>% left_join(CC9876_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#664 x 33


CC9876_combine.1 <-CC9876_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >= 30 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
CC9876_combine.1

ggplot(CC9876_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=CC9876_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-26 8:00:00'), 
           y = 25, label = "1392844", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392844 _endo_feb 15 2022.tiff",
       dpi = 300)


###1392845 serial ID######

#first, filter out desired fish and just get core metrics
DD6789<-Escapees_high %>% filter(serial == '1392845') %>% 
  arrange(date_time) %>% print()
#594 x 30


DD6789.1<- DD6789
DD6789.1$date_time <-as.numeric(DD6789.1$date_time)
DD6789.1

#inflection point analysis for when the fish was eaten
DD6789_result<-inflection::findiplist(DD6789.1$date_time,DD6789.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392845') %>% print() 

#convert back to real time
DD6789_result$pred_time <-as_datetime(DD6789_result$pred_time)
DD6789_result$serial <-as.double(DD6789_result$serial)

DD6789_result


DD6789_combine<-DD6789 %>% left_join(DD6789_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1188 x 33


DD6789_combine.1 <-DD6789_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
DD6789_combine.1

ggplot(DD6789_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=DD6789_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 12:00:00'), 
           y = 35, label = "1392845", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392845 _endo_feb 15 2022.tiff",
       dpi = 300)

#CURIOUS ON THE CYCLIC PATTERN:
# P <-Telemetry_master_escapees_pred %>% filter(serial == '1392845', sensor == 'depth') %>% print()
# ggplot(P, aes (x = as.POSIXct(date_time), y =sensor_value))+
#   geom_point(size = 2)+
#   geom_line(size=.5)+
#   geom_vline(xintercept=DD6789_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
#   labs(x=expression('Date'),
#        y=expression('DEPTH'))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
#         axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
#         axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
#         text=element_text(family="Times"), legend.position = 'none')+
#   scale_colour_manual(values = c("royalblue2", "red2"))+
#   annotate('text', x = as.POSIXct('2021-09-24 8:00:00'), 
#            y = 35, label = "1392845", fontface ="bold", size = 4.5)+
#   scale_x_datetime(breaks = date_breaks("1 days"),
#                    labels = date_format("%m/%d"))+
#   theme(axis.text.x = element_text(angle = 45,  hjust=1))+
#   scale_y_reverse()



###1392846 serial ID######

#first, filter out desired fish and just get core metrics
EE1357<-Escapees_high %>% filter(serial == '1392846') %>% 
  arrange(date_time) %>% print()
#445 x 30


EE1357.1<- EE1357
EE1357.1$date_time <-as.numeric(EE1357.1$date_time)
EE1357.1

#inflection point analysis for when the fish was eaten
EE1357_result<-inflection::findiplist(EE1357.1$date_time,EE1357.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392846') %>% print() 

#convert back to real time
EE1357_result$pred_time <-as_datetime(EE1357_result$pred_time)
EE1357_result$serial <-as.double(EE1357_result$serial)

EE1357_result


EE1357_combine<-EE1357 %>% left_join(EE1357_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#890 x 33


EE1357_combine.1 <-EE1357_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
EE1357_combine.1

ggplot(EE1357_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=EE1357_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-23 19:00:00'), 
           y = 35, label = "1392846", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392846 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392847 serial ID######

#first, filter out desired fish and just get core metrics
FF2468<-Escapees_high %>% filter(serial == '1392847') %>% 
  arrange(date_time) %>% print()
#354 x 30


FF2468.1<- FF2468
FF2468.1$date_time <-as.numeric(FF2468.1$date_time)
FF2468.1

#inflection point analysis for when the fish was eaten
FF2468_result<-inflection::findiplist(FF2468.1$date_time,FF2468.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392847') %>% print() 

#convert back to real time
FF2468_result$pred_time <-as_datetime(FF2468_result$pred_time)
FF2468_result$serial <-as.double(FF2468_result$serial)

FF2468_result


FF2468_combine<-FF2468 %>% left_join(FF2468_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#708 x 33


FF2468_combine.1 <-FF2468_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
FF2468_combine.1

ggplot(FF2468_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=FF2468_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 6:00:00'), 
           y = 35, label = "1392847", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392847 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392849 serial ID######



#first, filter out desired fish and just get core metrics
G9753<-Escapees_high %>% filter(serial == '1392849') %>%
  arrange(date_time) %>% print()
#2553 x 30

G9753.F <-G9753 %>% filter(date_time < as_datetime('2021-09-26 08:42:18')) %>% print()

G9753.1<- G9753.F
G9753.1$date_time <-as.numeric(G9753.1$date_time)
G9753.1

#inflection point analysis for when the fish was eaten
G9753_result<-inflection::findiplist(G9753.1$date_time,G9753.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392849') %>% print()

#convert back to real time
G9753_result$pred_time <-as_datetime(G9753_result$pred_time)
G9753_result$serial <-as.double(G9753_result$serial)

G9753_result


G9753_combine<-G9753.F %>% left_join(G9753_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#4506 x 33


G9753_combine.1 <-G9753_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>%
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
G9753_combine.1

ggplot(G9753_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=G9753_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 2:00:00'),
           y = 25, label = "1392849", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392849 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392850 serial ID######



#first, filter out desired fish and just get core metrics
HH3456<-Escapees_high %>% filter(serial == '1392850') %>% 
  arrange(date_time) %>% print()
#1009 x 30


HH3456.1<- HH3456
HH3456.1$date_time <-as.numeric(HH3456.1$date_time)
HH3456.1

#inflection point analysis for when the fish was eaten
HH3456_result<-inflection::findiplist(HH3456.1$date_time,HH3456.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392850') %>% print() 

#convert back to real time
HH3456_result$pred_time <-as_datetime(HH3456_result$pred_time)
HH3456_result$serial <-as.double(HH3456_result$serial)

HH3456_result


HH3456_combine<-HH3456 %>% left_join(HH3456_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#2018 x 33


HH3456_combine.1 <-HH3456_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
HH3456_combine.1

ggplot(HH3456_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=HH3456_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-25 23:00:00'), 
           y = 35, label = "1392850", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("3 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392850 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392851 serial ID######



#first, filter out desired fish and just get core metrics
II2468<-Escapees_high %>% filter(serial == '1392851') %>% 
  arrange(date_time) %>% print()
#201 x 30



II2468.1<- II2468
II2468.1$date_time <-as.numeric(II2468.1$date_time)
II2468.1

#inflection point analysis for when the fish was eaten
II2468_result<-inflection::findiplist(II2468.1$date_time,II2468.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392851') %>% print() 

#convert back to real time
II2468_result$pred_time <-as_datetime(II2468_result$pred_time)
II2468_result$serial <-as.double(II2468_result$serial)

II2468_result


II2468_combine<-II2468 %>% left_join(II2468_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#402 x 33


II2468_combine.1 <-II2468_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
II2468_combine.1

ggplot(II2468_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=II2468_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-24 12:00:00'), 
           y = 35, label = "1392851", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392851 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392852 serial ID######



#first, filter out desired fish and just get core metrics
JJ8526<-Escapees_high %>% filter(serial == '1392852') %>% 
  arrange(date_time) %>% print()
#941 x 30


#EXPELLED!

JJ8526.F<-JJ8526 %>% filter(date_time < as_datetime('2021-10-02 11:20:37')) %>% print()
#923 X 30

JJ8526.1<- JJ8526.F
JJ8526.1$date_time <-as.numeric(JJ8526.1$date_time)
JJ8526.1

#inflection point analysis for when the fish was eaten
JJ8526_result<-inflection::findiplist(JJ8526.1$date_time,JJ8526.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392852') %>% print() 

#convert back to real time
JJ8526_result$pred_time <-as_datetime(JJ8526_result$pred_time)
JJ8526_result$serial <-as.double(JJ8526_result$serial)

JJ8526_result


JJ8526_combine<-JJ8526.F %>% left_join(JJ8526_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1846 x 33


JJ8526_combine.1 <-JJ8526_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
JJ8526_combine.1

ggplot(JJ8526_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=JJ8526_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-27 23:30:00'), 
           y = 35, label = "1392852", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392852 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392853 serial ID######



#first, filter out desired fish and just get core metrics
KK2222<-Escapees_high %>% filter(serial == '1392853') %>% 
  arrange(date_time) %>% print()
#54 x 30


KK2222.1<- KK2222
KK2222.1$date_time <-as.numeric(KK2222.1$date_time)
KK2222.1

#inflection point analysis for when the fish was eaten
KK2222_result<-inflection::findiplist(KK2222.1$date_time,KK2222.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392853') %>% print() 

#convert back to real time
KK2222_result$pred_time <-as_datetime(KK2222_result$pred_time)
KK2222_result$serial <-as.double(KK2222_result$serial)

KK2222_result


KK2222_combine<-KK2222 %>% left_join(KK2222_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#108 x 33


KK2222_combine.1 <-KK2222_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
KK2222_combine.1

ggplot(KK2222_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=KK2222_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-27 17:00:00'), 
           y = 35, label = "1392853", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392853 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392854 serial ID######



#first, filter out desired fish and just get core metrics
L0101<-Escapees_high %>% filter(serial == '1392854') %>% 
  arrange(date_time) %>% print()
#1735 x 30
L0101.F<-L0101%>% filter(date_time < as_datetime('2021-10-01 09:46:47')) %>% print()
#204 X 30

L0101.1<- L0101.F
L0101.1$date_time <-as.numeric(L0101.1$date_time)
L0101.1

#inflection point analysis for when the fish was eaten
L0101_result<-inflection::findiplist(L0101.1$date_time,L0101.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392854') %>% print() 

#convert back to real time
L0101_result$pred_time <-as_datetime(L0101_result$pred_time)
L0101_result$serial <-as.double(L0101_result$serial)

L0101_result


L0101_combine<-L0101.F %>% left_join(L0101_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#408 x 33


L0101_combine.1 <-L0101_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
L0101_combine.1

ggplot(L0101_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=L0101_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-28 3:00:00'), 
           y = 35, label = "1392854", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392854 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392855 serial ID######



#first, filter out desired fish and just get core metrics
M2727<-Escapees_high %>% filter(serial == '1392855') %>% 
  arrange(date_time) %>% print()
#3597 x 30

M2727.F<-M2727 %>% filter(date_time < as_datetime('2021-09-30 17:56:31')) %>% print()
#163 X 30

M2727.1<- M2727.F
M2727.1$date_time <-as.numeric(M2727.1$date_time)
M2727.1

#inflection point analysis for when the fish was eaten
M2727_result<-inflection::findiplist(M2727.1$date_time,M2727.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392855') %>% print() 

#convert back to real time
M2727_result$pred_time <-as_datetime(M2727_result$pred_time)
M2727_result$serial <-as.double(M2727_result$serial)

M2727_result


M2727_combine<-M2727.F %>% left_join(M2727_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#326 x 33


M2727_combine.1 <-M2727_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
M2727_combine.1

ggplot(M2727_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=M2727_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-29 0:00:00'), 
           y = 25, label = "1392855", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392855 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392856 serial ID######



#first, filter out desired fish and just get core metrics
NN4004<-Escapees_high %>% filter(serial == '1392856') %>% 
  arrange(date_time) %>% print()
#147 x 30


NN4004.1<- NN4004
NN4004.1$date_time <-as.numeric(NN4004.1$date_time)
NN4004.1

#inflection point analysis for when the fish was eaten
NN4004_result<-inflection::findiplist(NN4004.1$date_time,NN4004.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392856') %>% print() 

#convert back to real time
NN4004_result$pred_time <-as_datetime(NN4004_result$pred_time)
NN4004_result$serial <-as.double(NN4004_result$serial)

NN4004_result


NN4004_combine<-NN4004 %>% left_join(NN4004_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#294 x 33


NN4004_combine.1 <-NN4004_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
NN4004_combine.1

ggplot(NN4004_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=NN4004_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 6:00:00'), 
           y = 25, label = "1392856", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392856 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392857 serial ID######



#first, filter out desired fish and just get core metrics

OO9999<-Escapees_high %>% filter(serial == '1392857') %>% 
  arrange(date_time) %>% print()
#4751 x 30

OO9999.F<-OO9999 %>% filter(date_time < as_datetime('2021-09-30 18:04:56')) %>% print()


OO9999.1<- OO9999.F
OO9999.1$date_time <-as.numeric(OO9999.1$date_time)
OO9999.1

#inflection point analysis for when the fish was eaten
OO9999_result<-inflection::findiplist(OO9999.1$date_time,OO9999.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392857') %>% print() 

#convert back to real time
OO9999_result$pred_time <-as_datetime(OO9999_result$pred_time)
OO9999_result$serial <-as.double(OO9999_result$serial)

OO9999_result


OO9999_combine<-OO9999.F %>% left_join(OO9999_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#976 x 33


OO9999_combine.1 <-OO9999_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
OO9999_combine.1

ggplot(OO9999_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=OO9999_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-27 23:00:00'), 
           y = 35, label = "1392857", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392857 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392858 serial ID######



#first, filter out desired fish and just get core metrics

PP2121<-Escapees_high %>% filter(serial == '1392858') %>% 
  arrange(date_time) %>% print()
#802 x 30


PP2121.1<- PP2121
PP2121.1$date_time <-as.numeric(PP2121.1$date_time)
PP2121.1

#inflection point analysis for when the fish was eaten
PP2121_result<-inflection::findiplist(PP2121.1$date_time,PP2121.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392858') %>% print() 

#convert back to real time
PP2121_result$pred_time <-as_datetime(PP2121_result$pred_time)
PP2121_result$serial <-as.double(PP2121_result$serial)

PP2121_result


PP2121_combine<-PP2121 %>% left_join(PP2121_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1604 x 33


PP2121_combine.1 <-PP2121_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
PP2121_combine.1

ggplot(PP2121_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=PP2121_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-05 6:00:00'), 
           y = 25, label = "1392858", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392858 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392859 serial ID######



#first, filter out desired fish and just get core metrics

QQ7777<-Escapees_high %>% filter(serial == '1392859') %>% 
  arrange(date_time) %>% print()
#1132 x 30


QQ7777.1<- QQ7777
QQ7777.1$date_time <-as.numeric(QQ7777.1$date_time)
QQ7777.1

#inflection point analysis for when the fish was eaten
QQ7777_result<-inflection::findiplist(QQ7777.1$date_time,QQ7777.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392859') %>% print() 

#convert back to real time
QQ7777_result$pred_time <-as_datetime(QQ7777_result$pred_time)
QQ7777_result$serial <-as.double(QQ7777_result$serial)

QQ7777_result


QQ7777_combine<-QQ7777 %>% left_join(QQ7777_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#2264 x 33


QQ7777_combine.1 <-QQ7777_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
QQ7777_combine.1

ggplot(QQ7777_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=QQ7777_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-29 6:00:00'), 
           y = 35, label = "1392859", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392859 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392860 serial ID######



#first, filter out desired fish and just get core metrics

RR1337<-Escapees_high %>% filter(serial == '1392860') %>% 
  arrange(date_time) %>% print()
#4183 x 30

RR1337.F<-RR1337 %>% filter(date_time < as_datetime('2021-09-30 18:11:09')) %>% print()
#281


RR1337.1<- RR1337.F
RR1337.1$date_time <-as.numeric(RR1337.1$date_time)
RR1337.1

#inflection point analysis for when the fish was eaten
RR1337_result<-inflection::findiplist(RR1337.1$date_time,RR1337.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392860') %>% print() 

#convert back to real time
RR1337_result$pred_time <-as_datetime(RR1337_result$pred_time)
RR1337_result$serial <-as.double(RR1337_result$serial)

RR1337_result


RR1337_combine<-RR1337.F %>% left_join(RR1337_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#562 x 33


RR1337_combine.1 <-RR1337_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
RR1337_combine.1

ggplot(RR1337_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=RR1337_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-27 23:00:00'), 
           y = 35, label = "1392860", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392860 _endo_feb 15 2022.tiff",
       dpi = 300)





##1392861 serial ID######

#COULDN'T COMPUTE INFLECTION POINT. DOING A MANAUL RECOLOUR FOR THE FIGS

#first, filter out desired fish and just get core metrics

SS4598<-Escapees_high %>% filter(serial == '1392861') %>% 
  arrange(date_time) %>% print()
#948 x 34



SS4598.1<- SS4598
SS4598.1$date_time <-as.numeric(SS4598.1$date_time)
SS4598.1

#inflection point analysis for when the fish was eaten
SS4598_result<-inflection::findiplist(SS4598.1$date_time,SS4598.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392861') %>% print() 

#convert back to real time
SS4598_result$pred_time <-as_datetime(SS4598_result$pred_time)
SS4598_result$serial <-as.double(SS4598_result$serial)

SS4598_result


SS4598_combine<-SS4598 %>% left_join(SS4598_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#562 x 33


SS4598_combine.1 <-SS4598_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >20 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
SS4598_combine.1

ggplot(SS4598_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=SS4598_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-15 00:00:00'), 
           y = 25, label = "1392861", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("4 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392861 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392862 serial ID######



#first, filter out desired fish and just get core metrics

TT8510<-Escapees_high %>% filter(serial == '1392862') %>%
  arrange(date_time) %>% print()
#78 x 30



TT8510.1<- TT8510
TT8510.1$date_time <-as.numeric(TT8510.1$date_time)
TT8510.1

#inflection point analysis for when the fish was eaten
TT8510_result<-inflection::findiplist(TT8510.1$date_time,TT8510.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392862') %>% print()

#convert back to real time
TT8510_result$pred_time <-as_datetime(TT8510_result$pred_time)
TT8510_result$serial <-as.double(TT8510_result$serial)

TT8510_result


TT8510_combine<-TT8510 %>% left_join(TT8510_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#156 x 33


TT8510_combine.1 <-TT8510_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>%
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
TT8510_combine.1

ggplot(TT8510_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=TT8510_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-01 12:00:00'),
           y = 25, label = "1392862", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392862 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392863 serial ID######



#first, filter out desired fish and just get core metrics

UU5858<-Escapees_high %>% filter(serial == '1392863') %>% 
  arrange(date_time) %>% print()
#2680 x 30

UU5858.F <- UU5858 %>% filter(date_time < as_datetime('2021-09-30 18:07:22')) %>% print()
#159 X 30

UU5858.1<- UU5858.F
UU5858.1$date_time <-as.numeric(UU5858.1$date_time)
UU5858.1

#inflection point analysis for when the fish was eaten
UU5858_result<-inflection::findiplist(UU5858.1$date_time,UU5858.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392863') %>% print() 

#convert back to real time
UU5858_result$pred_time <-as_datetime(UU5858_result$pred_time)
UU5858_result$serial <-as.double(UU5858_result$serial)

UU5858_result


UU5858_combine<-UU5858.F %>% left_join(UU5858_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#318 x 33


UU5858_combine.1 <-UU5858_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
UU5858_combine.1

ggplot(UU5858_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=UU5858_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 2:00:00'), 
           y = 25, label = "1392863", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392863 _endo_feb 15 2022.tiff",
       dpi = 300)





##1392865 serial ID######



#first, filter out desired fish and just get core metrics

VV6677<-Escapees_high %>% filter(serial == '1392865') %>% 
  arrange(date_time) %>% print()
#120 x 30


#EXPELLED!

VV6677.F <- VV6677 %>% filter(date_time < as_datetime('2021-09-29 09:03:00
')) %>% print()
#71 X 30

VV6677.1<- VV6677.F
VV6677.1$date_time <-as.numeric(VV6677.1$date_time)
VV6677.1

#inflection point analysis for when the fish was eaten
VV6677_result<-inflection::findiplist(VV6677.1$date_time,VV6677.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392865') %>% print() 

#convert back to real time
VV6677_result$pred_time <-as_datetime(VV6677_result$pred_time)
VV6677_result$serial <-as.double(VV6677_result$serial)

VV6677_result


VV6677_combine<-VV6677.F %>% left_join(VV6677_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#142 x 33


VV6677_combine.1 <-VV6677_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
VV6677_combine.1

ggplot(VV6677_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=VV6677_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-28 3:30:00'), 
           y = 25, label = "1392865", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392865 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392867 serial ID######



#first, filter out desired fish and just get core metrics

WW3636<-Escapees_high %>% filter(serial == '1392867') %>% 
  arrange(date_time) %>% print()
#436 X 30


WW3636.1<- WW3636
WW3636.1$date_time <-as.numeric(WW3636.1$date_time)
WW3636.1

#inflection point analysis for when the fish was eaten
WW3636_result<-inflection::findiplist(WW3636.1$date_time,WW3636.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392867') %>% print() 

#convert back to real time
WW3636_result$pred_time <-as_datetime(WW3636_result$pred_time)
WW3636_result$serial <-as.double(WW3636_result$serial)

WW3636_result


WW3636_combine<-WW3636 %>% left_join(WW3636_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#872 x 33


WW3636_combine.1 <-WW3636_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
WW3636_combine.1

ggplot(WW3636_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=WW3636_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-28 1:00:00'), 
           y = 35, label = "1392867", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392867 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392868 serial ID######



#first, filter out desired fish and just get core metrics

XX2693<-Escapees_high %>% filter(serial == '1392868') %>% 
  arrange(date_time) %>% print()
#3862 X 30


XX2693.F<-XX2693 %>% filter(date_time < as_datetime('2021-09-30 17:42:58')) %>% print()
#540 X 30

XX2693.1<- XX2693.F
XX2693.1$date_time <-as.numeric(XX2693.1$date_time)
XX2693.1

#inflection point analysis for when the fish was eaten
XX2693_result<-inflection::findiplist(XX2693.1$date_time,XX2693.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392868') %>% print() 

#convert back to real time
XX2693_result$pred_time <-as_datetime(XX2693_result$pred_time)
XX2693_result$serial <-as.double(XX2693_result$serial)

XX2693_result


XX2693_combine<-XX2693.F %>% left_join(XX2693_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1080 x 33


XX2693_combine.1 <-XX2693_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
XX2693_combine.1

ggplot(XX2693_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=XX2693_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-27 23:00:00'), 
           y = 35, label = "1392868", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392868 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392871 serial ID######



#first, filter out desired fish and just get core metrics

YY8889<-Escapees_high %>% filter(serial == '1392871') %>% 
  arrange(date_time) %>% print()
#19036 X 30


YY8889.F<-YY8889 %>% filter(date_time < as_datetime('2021-10-04 01:59:34')) %>% print()
#1593 X 30

YY8889.1<- YY8889.F
YY8889.1$date_time <-as.numeric(YY8889.1$date_time)
YY8889.1

#inflection point analysis for when the fish was eaten
YY8889_result<-inflection::findiplist(YY8889.1$date_time,YY8889.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392871') %>% print() 

#convert back to real time
YY8889_result$pred_time <-as_datetime(YY8889_result$pred_time)
YY8889_result$serial <-as.double(YY8889_result$serial)

YY8889_result


YY8889_combine<-YY8889.F %>% left_join(YY8889_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#3186 x 33


YY8889_combine.1 <-YY8889_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
YY8889_combine.1

ggplot(YY8889_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=YY8889_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-28 12:00:00'), 
           y = 35, label = "1392871", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392871 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392872 serial ID######



#first, filter out desired fish and just get core metrics

ZZ3245<-Escapees_high %>% filter(serial == '1392872') %>% 
  arrange(date_time) %>% print()
#807 X 30

ZZ3245.1<- ZZ3245
ZZ3245.1$date_time <-as.numeric(ZZ3245.1$date_time)
ZZ3245.1

#inflection point analysis for when the fish was eaten
ZZ3245_result<-inflection::findiplist(ZZ3245.1$date_time,ZZ3245.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392872') %>% print() 

#convert back to real time
ZZ3245_result$pred_time <-as_datetime(ZZ3245_result$pred_time)
ZZ3245_result$serial <-as.double(ZZ3245_result$serial)

ZZ3245_result


ZZ3245_combine<-ZZ3245 %>% left_join(ZZ3245_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1614 x 33


ZZ3245_combine.1 <-ZZ3245_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
ZZ3245_combine.1

ggplot(ZZ3245_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=ZZ3245_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-05 6:00:00'), 
           y = 25, label = "1392872", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392872 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392873 serial ID######


#COULD NOT COMPUTE AS INFLECTION AFTER SHED VALUE REMOVED, NOT CARRYING THROUGH HERE

#manual recolour

#first, filter out desired fish and just get core metrics

AAA234<-Escapees_high %>% filter(serial == '1392873') %>%
  arrange(date_time) %>% print()
#273 X 30

AAA234.F<-AAA234 %>% filter(date_time < as_datetime('2021-10-01 02:42:44')) %>% print()
#272 X 30

AAA234.1<- AAA234.F
AAA234.1$date_time <-as.numeric(AAA234.1$date_time)
AAA234.1

#inflection point analysis for when the fish was eaten
AAA234_result<-inflection::findiplist(AAA234.1$date_time,AAA234.1$sensor_value,index=1) %>%
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
  add_column(method = c("ese", 'ede'), serial = '1392873') %>% print()

#convert back to real time
AAA234_result$pred_time <-as_datetime(AAA234_result$pred_time)
AAA234_result$serial <-as.double(AAA234_result$serial)

AAA234_result


AAA234_combine<-AAA234.F %>% left_join(AAA234_result) %>%
  mutate(surv_duration = pred_time-release_date_time) %>%
  print()
#544 x 33


AAA234_combine.1 <-AAA234_combine %>% filter(method == 'ede') %>%
  mutate(colour = case_when(sensor_value > 22 ~ 'red',
                            TRUE~ 'blue')) %>%
  mutate(as.POSIXct(date_time)) %>% print()




#simple plot to visually confirm
AAA234_combine.1

ggplot(AAA234_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=AAA234_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 0:00:00'),
           y = 20, label = "1392873", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392873 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392876 serial ID######

#first, filter out desired fish and just get core metrics

BBB11<-Escapees_high %>% filter(serial == '1392876') %>% 
  arrange(date_time) %>% print()
#305 X 30


BBB11.1<- BBB11
BBB11.1$date_time <-as.numeric(BBB11.1$date_time)
BBB11.1

#inflection point analysis for when the fish was eaten
BBB11_result<-inflection::findiplist(BBB11.1$date_time,BBB11.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392876') %>% print() 

#convert back to real time
BBB11_result$pred_time <-as_datetime(BBB11_result$pred_time)
BBB11_result$serial <-as.double(BBB11_result$serial)

BBB11_result


BBB11_combine<-BBB11 %>% left_join(BBB11_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#610 x 33


BBB11_combine.1 <-BBB11_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
BBB11_combine.1

ggplot(BBB11_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=BBB11_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-28 23:00:00'), 
           y = 35, label = "1392876", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392876 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392877 serial ID######

#first, filter out desired fish and just get core metrics

CCC45<-Escapees_high %>% filter(serial == '1392877') %>% 
  arrange(date_time) %>% print()
#102 X 30

CCC45.F<-CCC45 %>% filter(date_time < as_datetime('2021-10-09 03:42:28')) %>% print()
#72 X 30

CCC45.1<- CCC45.F
CCC45.1$date_time <-as.numeric(CCC45.1$date_time)
CCC45.1

#inflection point analysis for when the fish was eaten
CCC45_result<-inflection::findiplist(CCC45.1$date_time,CCC45.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392877') %>% print() 

#convert back to real time
CCC45_result$pred_time <-as_datetime(CCC45_result$pred_time)
CCC45_result$serial <-as.double(CCC45_result$serial)

CCC45_result


CCC45_combine<-CCC45.F %>% left_join(CCC45_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#144 x 33


CCC45_combine.1 <-CCC45_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
CCC45_combine.1

ggplot(CCC45_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=CCC45_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 11:00:00'), 
           y = 25, label = "1392877", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392877 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392879 serial ID######

#first, filter out desired fish and just get core metrics

DDD99<-Escapees_high %>% filter(serial == '1392879') %>% 
  arrange(date_time) %>% print()
#5300 X 30

DDD99.F<-DDD99 %>% filter(date_time < as_datetime('2021-09-30 18:10:16')) %>% print()
#128 X 30

DDD99.1<- DDD99.F
DDD99.1$date_time <-as.numeric(DDD99.1$date_time)
DDD99.1

#inflection point analysis for when the fish was eaten
DDD99_result<-inflection::findiplist(DDD99.1$date_time,DDD99.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392879') %>% print() 

#convert back to real time
DDD99_result$pred_time <-as_datetime(DDD99_result$pred_time)
DDD99_result$serial <-as.double(DDD99_result$serial)

DDD99_result


DDD99_combine<-DDD99.F %>% left_join(DDD99_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#256 x 33


DDD99_combine.1 <-DDD99_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
DDD99_combine.1

ggplot(DDD99_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=DDD99_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-29 17:00:00'), 
           y = 35, label = "1392879", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392879 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392881 serial ID######

#first, filter out desired fish and just get core metrics

EEE67<-Escapees_high %>% filter(serial == '1392881') %>% 
  arrange(date_time) %>% print()
#1764 X 30



EEE67.1<- EEE67
EEE67.1$date_time <-as.numeric(EEE67.1$date_time)
EEE67.1

#inflection point analysis for when the fish was eaten
EEE67_result<-inflection::findiplist(EEE67.1$date_time,EEE67.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392881') %>% print() 

#convert back to real time
EEE67_result$pred_time <-as_datetime(EEE67_result$pred_time)
EEE67_result$serial <-as.double(EEE67_result$serial)

EEE67_result


EEE67_combine<-EEE67 %>% left_join(EEE67_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#3528 x 33


EEE67_combine.1 <-EEE67_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
EEE67_combine.1

ggplot(EEE67_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=EEE67_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 23:00:00'), 
           y = 35, label = "1392881", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392881 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392883 serial ID######

#first, filter out desired fish and just get core metrics

FFF42<-Escapees_high %>% filter(serial == '1392883') %>% 
  arrange(date_time) %>% print()
#93 X 30

FFF42.F<-FFF42 %>% filter(date_time < as_datetime('2021-10-01 21:42:56')) %>% print()

FFF42.1<- FFF42.F
FFF42.1$date_time <-as.numeric(FFF42.1$date_time)
FFF42.1

#inflection point analysis for when the fish was eaten
FFF42_result<-inflection::findiplist(FFF42.1$date_time,FFF42.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392883') %>% print() 

#convert back to real time
FFF42_result$pred_time <-as_datetime(FFF42_result$pred_time)
FFF42_result$serial <-as.double(FFF42_result$serial)

FFF42_result


FFF42_combine<-FFF42.F %>% left_join(FFF42_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#168 x 33


FFF42_combine.1 <-FFF42_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
FFF42_combine.1

ggplot(FFF42_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=FFF42_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-30 10:00:00'), 
           y = 25, label = "1392883", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392883 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392884 serial ID######

#first, filter out desired fish and just get core metrics

GGG98<-Escapees_high %>% filter(serial == '1392884') %>% 
  arrange(date_time) %>% print()
#257 X 30

GGG98.1<- GGG98
GGG98.1$date_time <-as.numeric(GGG98.1$date_time)
GGG98.1

#inflection point analysis for when the fish was eaten
GGG98_result<-inflection::findiplist(GGG98.1$date_time,GGG98.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392884') %>% print() 

#convert back to real time
GGG98_result$pred_time <-as_datetime(GGG98_result$pred_time)
GGG98_result$serial <-as.double(GGG98_result$serial)

GGG98_result


GGG98_combine<-GGG98 %>% left_join(GGG98_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#514 x 33


GGG98_combine.1 <-GGG98_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
GGG98_combine.1

ggplot(GGG98_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=GGG98_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-07 1:00:00'), 
           y = 25, label = "1392884", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392884 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392886 serial ID######

# #NOTE EATEN AT THE FIRST POINT, NOT CONTINUING ANALYSIS
#
# #first, filter out desired fish and just get core metrics

# HHH17<-Escapees_high %>% filter(serial == '1392886') %>%
# arrange(date_time) %>% print()
# #661 X 30
#
# HHH17.F<-HHH17 %>% filter(date_time < as_datetime('2021-09-30 18:10:16')) %>% print()
# 
# HHH17.1<- HHH17.F
# HHH17.1$date_time <-as.numeric(HHH17.1$date_time)
# HHH17.1

# #inflection point analysis for when the fish was eaten
# HHH17_result<-inflection::findiplist(HHH17.1$date_time,HHH17.1$sensor_value,index=1) %>% 
#   as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
#   add_column(method = c("ese", 'ede'), serial = '1392886') %>% print() 
# 
# #convert back to real time
# HHH17_result$pred_time <-as_datetime(HHH17_result$pred_time)
# HHH17_result$serial <-as.double(HHH17_result$serial)
# 
# HHH17_result
# 
# 
# HHH17_combine<-HHH17 %>% left_join(HHH17_result) %>% 
#   mutate(surv_duration = pred_time-release_date_time) %>% 
#   print()
# #514 x 33
# 
# 
# HHH17_combine.1 <-HHH17_combine %>% filter(method == 'ede') %>% 
#   mutate(colour = case_when(date_time >= pred_time ~ 'red',
#                             TRUE~ 'blue')) %>% 
#   mutate(as.POSIXct(date_time)) %>% print()
# 
# 
# #simple plot to visually confirm
# HHH17_combine.1
# 
# ggplot(HHH17_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
#   geom_point(size = 2, aes(colour = colour, shape = colour))+
#   geom_line(size=.5)+
#   geom_vline(xintercept=HHH17_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
#   labs(x=expression('Date'),
#        y=expression('Temperature (\u00B0C)'))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
#         axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
#         axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
#         text=element_text(family="Times"), legend.position = 'none')+
#   scale_colour_manual(values = c("royalblue2", "red2"))+
#   annotate('text', x = as.POSIXct('2021-10-07 12:00:00'), 
#            y = 25, label = "1392886", fontface ="bold", size = 4.5)+
#   scale_x_datetime(breaks = date_breaks("1 days"),
#                    labels = date_format("%m/%d"))+
#   theme(axis.text.x = element_text(angle = 45,  hjust=1))
# 
# ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392886 _endo_feb 15 2022.tiff",
#        dpi = 300)
# 
# 

# ##1392887 serial ID######
# 
# #EATEN RIGHT OFF THE HOP, NO NEED FOR FURTHER ANALYSIS

#first, filter out desired fish and just get core metrics
# 
# III56<-Escapees_high %>% filter(serial == '1392887') %>%
#  arrange(date_time) %>% print()
# #793 X 30
# 
# III56.F<-III56 %>% filter(date_time < as_datetime('2021-09-30 18:10:16')) %>% print()
# 
# III56.1<- III56.F
# III56.1$date_time <-as.numeric(III56.1$date_time)
# III56.1
# 
# #inflection point analysis for when the fish was eaten
# III56_result<-inflection::findiplist(III56.1$date_time,III56.1$sensor_value,index=1) %>%
#   as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>%
#   add_column(method = c("ese", 'ede'), serial = '1392887') %>% print()
# 
# #convert back to real time
# III56_result$pred_time <-as_datetime(III56_result$pred_time)
# III56_result$serial <-as.double(III56_result$serial)
# 
# III56_result

# 
# III56_combine<-III56.F %>% left_join(III56_result) %>%
#   mutate(surv_duration = pred_time-release_date_time) %>%
#   print()
# #1586 x 33
# 
# 
# III56_combine.1 <-III56_combine %>% filter(method == 'ede') %>%
#   mutate(colour = case_when(sensor_value > 20 ~ 'red',
#                             TRUE~ 'blue')) %>%
#   mutate(as.POSIXct(date_time)) %>% print()
# 
# 
# #simple plot to visually confirm
# III56_combine.1
# 
# ggplot(III56_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
#   geom_point(size = 2, aes(colour = colour, shape = colour))+
#   geom_line(size=.5)+
#   geom_vline(xintercept=III56_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
#   labs(x=expression('Date'),
#        y=expression('Temperature (\u00B0C)'))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),
#         axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
#         axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
#         text=element_text(family="Times"), legend.position = 'none')+
#   scale_colour_manual(values = c("red2", "red2"))+
#   annotate('text', x = as.POSIXct('2021-09-30 12:00:00'),
#            y = 30, label = "1392887", fontface ="bold", size = 4.5)+
#   scale_x_datetime(breaks = date_breaks("1 days"),
#                    labels = date_format("%m/%d"))+
#   theme(axis.text.x = element_text(angle = 45,  hjust=1))+
#   scale_shape_manual(values= c(17,17))
# 
# ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392887 _endo_feb 15 2022.tiff",
#        dpi = 300)



##1392889 serial ID######

#INFLECTION NOT CALC'D
#MANUAL COLOURATION FOR FIGURE

#first, filter out desired fish and just get core metrics

JJJ36<-Escapees_high %>% filter(serial == '1392889') %>% 
  arrange(date_time) %>% print()
#1635 X 30


JJJ36.1<- JJJ36
JJJ36.1$date_time <-as.numeric(JJJ36.1$date_time)
JJJ36.1

#inflection point analysis for when the fish was eaten
JJJ36_result<-inflection::findiplist(JJJ36.1$date_time,JJJ36.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392889') %>% print() 

#convert back to real time
JJJ36_result$pred_time <-as_datetime(JJJ36_result$pred_time)
JJJ36_result$serial <-as.double(JJJ36_result$serial)

JJJ36_result


JJJ36_combine<-JJJ36 %>% left_join(JJJ36_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#3270 x 33


JJJ36_combine.1 <-JJJ36_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >20 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
JJJ36_combine.1

ggplot(JJJ36_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=JJJ36_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-21 12:00:00'), 
           y = 25, label = "1392889", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("3 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392889 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392890 serial ID######


#first, filter out desired fish and just get core metrics

KKM24<-Escapees_high %>% filter(serial == '1392890') %>% 
  arrange(date_time) %>% print()
#593 X 30


KKM24.1<- KKM24
KKM24.1$date_time <-as.numeric(KKM24.1$date_time)
KKM24.1

#inflection point analysis for when the fish was eaten
KKM24_result<-inflection::findiplist(KKM24.1$date_time,KKM24.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392890') %>% print() 

#convert back to real time
KKM24_result$pred_time <-as_datetime(KKM24_result$pred_time)
KKM24_result$serial <-as.double(KKM24_result$serial)

KKM24_result


KKM24_combine<-KKM24 %>% left_join(KKM24_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1186 x 33


KKM24_combine.1 <-KKM24_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
KKM24_combine.1

ggplot(KKM24_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=KKM24_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-05 12:00:00'), 
           y = 25, label = "1392890", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392890 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392892 serial ID######


#first, filter out desired fish and just get core metrics

LLL88<-Escapees_high %>% filter(serial == '1392892') %>% 
  arrange(date_time) %>% print()
#399 X 30


LLL88.1<- LLL88
LLL88.1$date_time <-as.numeric(LLL88.1$date_time)
LLL88.1

#inflection point analysis for when the fish was eaten
LLL88_result<-inflection::findiplist(LLL88.1$date_time,LLL88.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392892') %>% print() 

#convert back to real time
LLL88_result$pred_time <-as_datetime(LLL88_result$pred_time)
LLL88_result$serial <-as.double(LLL88_result$serial)

LLL88_result


LLL88_combine<-LLL88 %>% left_join(LLL88_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#798 x 33


LLL88_combine.1 <-LLL88_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
LLL88_combine.1

ggplot(LLL88_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=LLL88_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-24 0:00:00'), 
           y = 25, label = "1392892", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("4 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392892 _endo_feb 15 2022.tiff",
       dpi = 300)


##1392893 serial ID######


#first, filter out desired fish and just get core metrics

PPP36<-Escapees_high %>% filter(serial == '1392893') %>% 
  arrange(date_time) %>% print()
#581 X 30


PPP36.1<- PPP36
PPP36.1$date_time <-as.numeric(PPP36.1$date_time)
PPP36.1

#inflection point analysis for when the fish was eaten
PPP36_result<-inflection::findiplist(PPP36.1$date_time,PPP36.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392893') %>% print() 

#convert back to real time
PPP36_result$pred_time <-as_datetime(PPP36_result$pred_time)
PPP36_result$serial <-as.double(PPP36_result$serial)

PPP36_result


PPP36_combine<-PPP36 %>% left_join(PPP36_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1162 x 33


PPP36_combine.1 <-PPP36_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
PPP36_combine.1

ggplot(PPP36_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=PPP36_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-05 0:00:00'), 
           y = 25, label = "1392893", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392893 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392895 serial ID######

#COULDnt CAL EDE VALUE, USING MANUAL COLOURS FOR PLOT


#first, filter out desired fish and just get core metrics

OOO23<-Escapees_high %>% filter(serial == '1392895') %>% 
  arrange(date_time) %>% print()
#901 X 30


OOO23.1<- OOO23
OOO23.1$date_time <-as.numeric(OOO23.1$date_time)
OOO23.1

#inflection point analysis for when the fish was eaten
OOO23_result<-inflection::findiplist(OOO23.1$date_time,OOO23.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392895') %>% print() 

#convert back to real time
OOO23_result$pred_time <-as_datetime(OOO23_result$pred_time)
OOO23_result$serial <-as.double(OOO23_result$serial)

OOO23_result


OOO23_combine<-OOO23 %>% left_join(OOO23_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1802 x 33


OOO23_combine.1 <-OOO23_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >= 25 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
OOO23_combine.1

ggplot(OOO23_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=OOO23_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-15 18:00:00'), 
           y = 25, label = "1392895", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("4 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392895 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392896 serial ID######

#INFLECTION VALUE COULDN'T BE CALCULATED
#MANUAL COLOURING HAPPENING

#first, filter out desired fish and just get core metrics

QQQ87<-Escapees_high %>% filter(serial == '1392896') %>% 
  arrange(date_time) %>% print()
#439 X 30

QQQ87.F <- QQQ87 %>% filter(date_time < as_datetime('2021-10-03 23:21:29')) %>% print()
#435

QQQ87.1<- QQQ87.F
QQQ87.1$date_time <-as.numeric(QQQ87.1$date_time)
QQQ87.1

#inflection point analysis for when the fish was eaten
QQQ87_result<-inflection::findiplist(QQQ87.1$date_time,QQQ87.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392896') %>% print() 

#convert back to real time
QQQ87_result$pred_time <-as_datetime(QQQ87_result$pred_time)
QQQ87_result$serial <-as.double(QQQ87_result$serial)

QQQ87_result


QQQ87_combine<-QQQ87.F %>% left_join(QQQ87_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#870 x 33


QQQ87_combine.1 <-QQQ87_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >= 25 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
QQQ87_combine.1

ggplot(QQQ87_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=QQQ87_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-02 20:00:00'), 
           y = 25, label = "1392896", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392896 _endo_feb 15 2022.tiff",
       dpi = 300)




##1392897 serial ID######


#first, filter out desired fish and just get core metrics

RRR57<-Escapees_high %>% filter(serial == '1392897') %>% 
  arrange(date_time) %>% print()
#4950 X 30

RRR57.F <- RRR57 %>% filter(date_time < as_datetime('2021-09-30 18:03:24')) %>% print()
#194

RRR57.1<- RRR57.F
RRR57.1$date_time <-as.numeric(RRR57.1$date_time)
RRR57.1

#inflection point analysis for when the fish was eaten
RRR57_result<-inflection::findiplist(RRR57.1$date_time,RRR57.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392897') %>% print() 

#convert back to real time
RRR57_result$pred_time <-as_datetime(RRR57_result$pred_time)
RRR57_result$serial <-as.double(RRR57_result$serial)

RRR57_result


RRR57_combine<-RRR57.F %>% left_join(RRR57_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#388 x 33


RRR57_combine.1 <-RRR57_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
RRR57_combine.1

ggplot(RRR57_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=RRR57_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-9-30 6:00:00'), 
           y = 25, label = "1392897", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392897 _endo_feb 15 2022.tiff",
       dpi = 300)



##1392898 serial ID######


#first, filter out desired fish and just get core metrics

SSS33<-Escapees_high %>% filter(serial == '1392898') %>% 
  arrange(date_time) %>% print()
#643 X 30

SSS33.F <- SSS33 %>% filter(date_time < as_datetime('2021-09-30 18:03:24')) %>% print()
#72

SSS33.1<- SSS33
SSS33.1$date_time <-as.numeric(SSS33.1$date_time)
SSS33.1

#inflection point analysis for when the fish was eaten
SSS33_result<-inflection::findiplist(SSS33.1$date_time,SSS33.1$sensor_value,index=1) %>% 
  as.tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392898') %>% print() 

#convert back to real time
SSS33_result$pred_time <-as_datetime(SSS33_result$pred_time)
SSS33_result$serial <-as.double(SSS33_result$serial)

SSS33_result


SSS33_combine<-SSS33 %>% left_join(SSS33_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#1286 x 33


SSS33_combine.1 <-SSS33_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
SSS33_combine.1

ggplot(SSS33_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=SSS33_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-05 12:00:00'), 
           y = 25, label = "1392898", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Endotherm plots/Fish 1392898 _endo_feb 15 2022.tiff",
       dpi = 300)


#### Endotherm pred master data sheet #####

#combine all of the esa/ede results into one tibble

Inflection_combined<-bind_rows(A787_result,
                               B213_result,
                               C224_result,
                               D353_result,
                               E456_result,
                               F367_result,
                               g112_result,
                               H777_result,
                               I187_result,
                               J684_result,
                               K795_result,
                               L117_result,
                               M270_result,
                               N009_result,
                               O123_result,#
                               P329_result,
                               Q675_result,
                               R445_result,
                               S979_result,
                               T002_result,#
                               U343_result,#
                               V798_result,#
                               W667_result, #
                               X555_result,
                               Y215_result,
                               AA1234_result,
                               BB4321_result,
                               CC9876_result,#
                               DD6789_result,
                               EE1357_result,
                               FF2468_result,
                               G9753_result,#
                               HH3456_result,
                               II2468_result,
                               JJ8526_result,
                               KK2222_result,
                               L0101_result,
                               M2727_result,
                               NN4004_result,
                               OO9999_result,
                               PP2121_result,
                               QQ7777_result,
                               RR1337_result,
                               SS4598_result,#
                               TT8510_result,#
                               UU5858_result,
                               VV6677_result,
                               WW3636_result,
                               XX2693_result,
                               YY8889_result,
                               ZZ3245_result,
                               AAA234_result,#
                               BBB11_result,
                               CCC45_result,
                               DDD99_result,
                               EEE67_result,
                               FFF42_result,
                               GGG98_result,
                               
                               JJJ36_result,#
                               KKM24_result,
                               LLL88_result,
                               PPP36_result,
                               OOO23_result,#
                               QQQ87_result,#
                               RRR57_result,
                               SSS33_result) %>% print()
#132 x 3              


#merge these data with the endothermic predation df

Endo_pred_combine <-Escapees_high %>% left_join(Inflection_combined) %>%
  select(serial, date_time, pred_time,  everything(),) %>% print()

#247,182 x 36

length(unique(Endo_pred_combine$serial))
#68


#Calc time to predation event and filter out some non needed things: 

table(is.na(Endo_pred_combine$pred_time))

Endo_pred_combine.1 <-Endo_pred_combine %>% 
  mutate(EDE_survival_time = pred_time-release_date_time, units = "hours") %>% 
  rename(EDE_pred_time = pred_time) %>% 
  filter(method == "ede") %>% 
  print()

#122,864
length(unique(Endo_pred_combine.1$serial))

#66

write_csv((Endo_pred_combine.1 %>% select(serial, release_date_time) %>%
       group_by(serial) %>% slice(n=1)), 
       "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/release-date-time_april 28 2022.csv")




write_csv(Endo_pred_combine.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Endo_predation_full_summary_2021_Dec 9 2022.csv')

#extract out just single time value for each of the fish and clean things up

Endo_pred_combine.2 <- Endo_pred_combine.1 %>% 
  group_by(serial) %>% 
  slice(1) %>% 
  select(serial,transmitter, EDE_survival_time,total_length_mm, 
         weight_g, group, max_temp) %>% 
  as.tibble() %>% 
  print()
#66 x 7

Endo_pred_combine.2$EDE_survival_time <-as.double(Endo_pred_combine.2$EDE_survival_time)
Endo_pred_combine.2

write_csv(Endo_pred_combine.2, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Endo_time to predation_2021_Dec 9 2022.csv')


####Meso pred analysis######

Pred_master <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Predation_master_file_2021_Dec 9 2022.csv') %>% print()
#151,366 x 34



Meso_pred<-Pred_master %>% filter(temp_cat == 'mid') %>% print()
#1,665 x 34


#couple checks to make sure it all looks good

range(Meso_pred$sensor_value)
#[1]  12.9555 29.3355

range(Meso_pred$max_temp)
#[1] 18.7830 29.3355

length(unique(Meso_pred$serial))
#5, matches with original max temp stuff 

windowsFonts(Times=windowsFont("TT Times New Roman")) 


##### Time to predation event_meso



unique(Meso_pred$serial)

#[1] 1392804 1392812 1392820 1392835 1392840

#will be doing each fish as a breakpoint analysis to determine time of predation




##1392804 serial ID######



#first, filter out desired fish and just get core metrics

YY8889Y<-Meso_pred %>% filter(serial == '1392804') %>% 
  arrange(date_time) %>% print()
#231 x 31


YY8889Y.1<- YY8889Y
YY8889Y.1$date_time <-as.numeric(YY8889Y.1$date_time)
YY8889Y.1

#inflection point analysis for when the fish was eaten
YY8889Y_result<-inflection::findiplist(YY8889Y.1$date_time,YY8889Y.1$sensor_value,index=1) %>% 
  as_tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392804') %>% print() 

#convert back to real time
YY8889Y_result$pred_time <-as_datetime(YY8889Y_result$pred_time)
YY8889Y_result$serial <-as.double(YY8889Y_result$serial)

YY8889Y_result


YY8889Y_combine<-YY8889Y %>% left_join(YY8889Y_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#462 x 38


YY8889Y_combine.1 <-YY8889Y_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
YY8889Y_combine.1

ggplot(YY8889Y_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=YY8889Y_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-21 22:00:00'), 
           y = 35, label = "1392804", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Mesotherm plots/Fish 1392804 _meso_feb 17 2022.tiff",
       dpi = 300)





##1392812 serial ID######

#NOTE CANNOT DETERMINE A BREAKPOINT, RATHER GENTLE SLOPE UNTIL NEAR THE END
#OF WHERE THE FISH WAS eaten

#Likely, just enviro temps based on watershed data:
#https://nwis.waterdata.usgs.gov/nwis/uv/?ts_id=65434&format=img_default&site_no=01021050&set_arithscale_y=on&begin_date=20210926&end_date=20210929


#first, filter out desired fish and just get core metrics
# 
# ZQ269<-Meso_pred %>% filter(serial == '1392812') %>% 
 # arrange(date_time) %>% print()
# #230 x 31
# 
# 
# ZQ269.1<- ZQ269
# ZQ269.1$date_time <-as.numeric(ZQ269.1$date_time)
# ZQ269.1
# 
# #inflection point analysis for when the fish was eaten
# ZQ269_result<-inflection::findiplist(ZQ269.1$date_time,ZQ269.1$sensor_value,index=1) %>% 
#   as_tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
#   add_column(method = c("ese", 'ede'), serial = '1392812') %>% print() 
# 
# #convert back to real time
# ZQ269_result$pred_time <-as_datetime(ZQ269_result$pred_time)
# ZQ269_result$serial <-as.double(ZQ269_result$serial)
# 
# ZQ269_result
# 
# 
# ZQ269_combine<-ZQ269 %>% left_join(ZQ269_result) %>% 
#   mutate(surv_duration = pred_time-release_date_time) %>% 
#   print()
# #460 x 34
# 
# 
# ZQ269_combine.1 <-ZQ269_combine %>% filter(method == 'ede') %>% 
#   mutate(colour = case_when(date_time >= pred_time ~ 'red',
#                             TRUE~ 'blue')) %>% 
#   mutate(as.POSIXct(date_time)) %>% print()
# 
# 
# #simple plot to visually confirm
# ZQ269_combine.1
# 
# ggplot(ZQ269_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
#   geom_point(size = 2, aes(colour = colour, shape = colour))+
#   geom_line(size=.5)+
#   geom_vline(xintercept=ZQ269_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
#   labs(x=expression('Date'),
#        y=expression('Temperature (\u00B0C)'))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
#         axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
#         axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
#         text=element_text(family="Times"), legend.position = 'none')+
#   scale_colour_manual(values = c("royalblue2", "red2"))+
#   annotate('text', x = as.POSIXct('2021-09-22 22:00:00'), 
#            y = 35, label = "1392812", fontface ="bold", size = 4.5)+
#   scale_x_datetime(breaks = date_breaks("1 days"),
#                    labels = date_format("%m/%d"))+
#   theme(axis.text.x = element_text(angle = 45,  hjust=1))
# 
# ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Mesotherm plots/Fish 1392812 _meso_feb 17 2022.tiff",
#        dpi = 300)
# 
# 
# 
# 

##1392820 serial ID######



#first, filter out desired fish and just get core metrics

TUB34<-Meso_pred %>% filter(serial == '1392820') %>% 
  arrange(date_time) %>% print()
#209 x 35


TUB34.1<- TUB34
TUB34.1$date_time <-as.numeric(TUB34.1$date_time)
TUB34.1

#inflection point analysis for when the fish was eaten
TUB34_result<-inflection::findiplist(TUB34.1$date_time,TUB34.1$sensor_value,index=1) %>% 
  as_tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392820') %>% print() 

#convert back to real time
TUB34_result$pred_time <-as_datetime(TUB34_result$pred_time)
TUB34_result$serial <-as.double(TUB34_result$serial)

TUB34_result


TUB34_combine<-TUB34 %>% left_join(TUB34_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#314 x 34


TUB34_combine.1 <-TUB34_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
TUB34_combine.1

ggplot(TUB34_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=TUB34_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-22 4:00:00'), 
           y = 35, label = "1392820", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Mesotherm plots/Fish 1392820 _meso_feb 17 2022.tiff",
       dpi = 300)



##1392835 serial ID######



#first, filter out desired fish and just get core metrics

VR389<-Meso_pred %>% filter(serial == '1392835') %>% 
  arrange(date_time) %>% print()
#126 x 31


VR389.1<- VR389
VR389.1$date_time <-as.numeric(VR389.1$date_time)
VR389.1

#inflection point analysis for when the fish was eaten
VR389_result<-inflection::findiplist(VR389.1$date_time,VR389.1$sensor_value,index=1) %>% 
  as_tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392835') %>% print() 

#convert back to real time
VR389_result$pred_time <-as_datetime(VR389_result$pred_time)
VR389_result$serial <-as.double(VR389_result$serial)

VR389_result


VR389_combine<-VR389 %>% left_join(VR389_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#196 x 34



VR389_combine.1 <-VR389_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(date_time >= pred_time ~ 'red',
                            TRUE~ 'blue')) %>%  
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
VR389_combine.1

ggplot(VR389_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=VR389_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-09-23 18:00:00'), 
           y = 35, label = "1392835", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Mesotherm plots/Fish 1392835 _meso_feb 17 2022.tiff",
       dpi = 300)



##1392840 serial ID######

#NOTE!!!

#So for this fish, it seems to go between the ocean and the river (first inflection point)
#Returns back to the ocean second flat. So first going filter that first 
#'curve' out as it might be preventing getting the inflection number calc'd
#for the hot times, almost 24 h in the gut and never goes above 29C (visually inspected)
#so fairly confident this is not a mammal/bird pred


#Still couldn't calc inflection point so like the fish above, 
#going to manually colour the points for the fisuge

#first, filter out desired fish and just get core metrics

PLK99<-Meso_pred %>% filter(serial == '1392840') %>% 
  arrange(date_time) %>% print()
#772 x 31

PLK99.F <-PLK99 %>% filter(date_time >= as_datetime('2021-10-01 06:51:17')) %>% print()
#398 X 31


PLK99.1<- PLK99.F
PLK99.1$date_time <-as.numeric(PLK99.1$date_time)
PLK99.1

#inflection point analysis for when the fish was eaten
PLK99_result<-inflection::findiplist(PLK99.1$date_time,PLK99.1$sensor_value,index=1) %>% 
  as_tibble() %>% select(chi) %>% rename(pred_time = chi) %>% 
  add_column(method = c("ese", 'ede'), serial = '1392840') %>% print() 

#convert back to real time
PLK99_result$pred_time <-as_datetime(PLK99_result$pred_time)
PLK99_result$serial <-as.double(PLK99_result$serial)

PLK99_result


PLK99_combine<-PLK99.F %>% left_join(PLK99_result) %>% 
  mutate(surv_duration = pred_time-release_date_time) %>% 
  print()
#796 x 34


PLK99_combine.1 <-PLK99_combine %>% filter(method == 'ede') %>% 
  mutate(colour = case_when(sensor_value >= 20 ~ 'red',
                            TRUE~ 'blue')) %>% 
  mutate(as.POSIXct(date_time)) %>% print()


#simple plot to visually confirm
PLK99_combine.1

ggplot(PLK99_combine.1, aes (x = as.POSIXct(date_time), y =sensor_value))+
  geom_point(size = 2, aes(colour = colour, shape = colour))+
  geom_line(size=.5)+
  geom_vline(xintercept=PLK99_combine.1$pred_time, linetype="dotted", colour = 'red', size = .75)+
  labs(x=expression('Date'),
       y=expression('Temperature (\u00B0C)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "grey20", size = 13,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 16, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 16, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"), legend.position = 'none')+
  scale_colour_manual(values = c("royalblue2", "red2"))+
  annotate('text', x = as.POSIXct('2021-10-01 22:00:00'), 
           y = 35, label = "1392840", fontface ="bold", size = 4.5)+
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%m/%d"))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Mesotherm plots/Fish 1392840 _meso_feb 17 2022.tiff",
       dpi = 300)



####Meso pred master data sheet #####

#combine all of the esa/ede results into one tibble

#FOR THE RECORD:
#The fish that i have strong confidence in as being 'real' predation events by 
#mesothermic preds

#include: 1392804 (YY8), 1392820 (TUB34), 1392835 (VR389), 1392840 (PLK99)
#exclude: 1392812 (ZZ269)

Inflection_combined<-bind_rows(YY8889Y_result,
                               TUB34_result,
                               VR389_result,
                               PLK99_result) %>% print()
#8 x 3              



#merge these data with the endothermic predation df

Meso_pred_combine <-Meso_pred %>% left_join(Inflection_combined) %>% 
select(serial, date_time, pred_time,  everything(),) %>% print()
#3052 x 36




List_meso <-unique(Inflection_combined$serial) %>% print()

List_meso.1 <-as.tibble(List_meso) %>%  rename(serial = value) %>% print()

write_csv(List_meso.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Mesotherm_pred_list_2021_Dec 9 2022.csv')


#Calc time to predation event and filter out some non needed things: 

Meso_pred_combine.1 <-Meso_pred_combine %>% 
  mutate(EDE_survival_time = pred_time-release_date_time, units = "hours") %>% 
  rename(EDE_pred_time =pred_time) %>% 
  filter(method == "ede") %>% 
  print()

#1387 x 38

write_csv(Meso_pred_combine.1,'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Meso_predation_full_summary_2021_Dec 9 2022.csv')

write_csv((Meso_pred_combine.1 %>% select(serial, release_date_time) %>% 
              group_by(serial) %>% slice(N=1)),
           'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Meso_releasetimes_Dec 9 2022.csv') 



#extract out just single time value for each of the fish and clean things up

Meso_pred_combine.2 <- Meso_pred_combine.1 %>% 
  group_by(serial) %>% 
  slice(1) %>% 
  select(serial,transmitter, EDE_survival_time,total_length_mm, 
         weight_g, group, max_temp) %>% 
  as.tibble() %>% 
  print()
#4 x 7   

write_csv(Meso_pred_combine.2, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/Meso_time to predation_2021_Dec 9 2022.csv')



###### survival time_manual#####
#Did a manual check thru post addition of new reciever data, didn't appear to 
#change any of the values in the escappe summary sheet, which means then predator
#summary sheet is fine 

library(readxl)
Manual_time<-read_excel('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Currated_summary_sheets/Escapees_summary sheet_June 6 2022.xlsx') %>% print()
#102 x 25


Manual_time.1<-Manual_time %>% filter(Predator_type =='E'| Predator_type == 'M') %>% 
    print()
#72x25




Manual_time.2 <- Manual_time.1 %>% mutate(survival_time_manual = as.numeric(Last_enviro_time -Release_date_time, units = 'hours')) %>% print()



write_csv((Manual_time.2 %>% select(serial, Predator_type, Last_enviro_time, survival_time_manual)), 
          'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/survival_time_manual_Dec 14 2022.csv' )




####Combine all the predator info together#####

pred_all <-bind_rows(Endo_pred_combine.1, Meso_pred_combine.1) %>% print()
#124,251 x 38

#makes sense as meso is 1387 and endo is 122,864

#join in the manual time sutff 

Manual_time.3<-Manual_time.2 %>% select(serial,Release_date_time, Predator_type, Last_enviro_time, survival_time_manual) %>% print()
#72 x 5


pred_all.1<-pred_all %>% left_join(Manual_time.3) %>% print()
#124,251 x42

#Note this excludes the two eaten fish right away
#For sake of simplicity want to re-add in those fish
#That would be fish 1392886 and 1392887

#extract out those fish:

Kicked_fish<-Manual_time.3 %>% 
  filter(serial == '1392886' | serial == '1392887') %>% 
  print()


#manaully insert them into the pred.1 all

pred_all.2 <-pred_all.1 %>% bind_rows(Kicked_fish) %>% print()
#124,253 x 42



write_csv(pred_all.2, 
          'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/ALL_PREDATION_TIMING.csv' )


#####MANUAL time stats#####

# for doing ede vs manual time diffs, not needed anymore
# library(quantmod)
# Manual_time.m<-Manual_time %>% mutate(manual_ede_difference.1 = Delt(Surivival_time_inflection_h, Surivival_time_manual_h)*100)
# 
# Manual_time.m$manual_ede_difference.1
# sort(Manual_time.m$manual_ede_difference.1)
# 
#detach("package:quantmod", unload=TRUE)


#first identify how many fish were eaten by time brackets:
#going to use the 'manual' data as its more reflective i think of 
#how long the fish survived for. 
#Based on the differences in times, average in only ~3h with median of 1 h
#% change shows majority of differences are under 15% (above)


Manual_time<-read_csv(
  'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/ALL_PREDATION_TIMING.csv' ) %>% print()
#124,253 x 42


length(unique(Manual_time$serial))
#72
#makes sesne as of the 72 eaten,


#simplify it all to have a single fish per pred line

Manual_time.1<-Manual_time %>% select(serial,Predator_type, Last_enviro_time,
                                      survival_time_manual) %>% 
  group_by(serial) %>% slice(n=1) %>% print()

#72 x 4

#For this, just be aware that we don't have a specific time for the 
#two kicked fish but they appear to before 6 h. 
#While not in the below, there were manaully added to the tally sheet and in the imported CSV
#


#6 h:


Manual_time_6 <-Manual_time.1 %>% filter(survival_time_manual <= 6) %>% print()
#13 x 4 

#However


Manual_time_6 %>% group_by(Predator_type) %>% tally()

# # A tibble: 2 x 2
# Predator_type     n
# <chr>           <int>
#  E                12
#  M                 1


sort(unique(Manual_time_6$serial))

#[1] 1392825 1392829 1392834 1392836 1392842 1392844 1392849 1392862 1392863 1392865 1392890 1392896 1392835




#12 h:


Manual_time_12 <-Manual_time.1 %>% filter(survival_time_manual>6&survival_time_manual<=12) %>% print()
#9 x 4 

Manual_time_12 %>% group_by(Predator_type) %>% tally()

# # A tibble: 2 x 2
# Predator_type     n
# <chr>           <int>
#  E                9



unique(Manual_time_12$serial)

#[1] 1392821 1392853 1392855 1392856 1392877 1392883 1392893 1392897 1392898





#24 h:


Manual_time_24 <-Manual_time.1 %>% filter(survival_time_manual>12&survival_time_manual<=24) %>% print()
#10 x 4 

Manual_time_24 %>% group_by(Predator_type) %>% tally()

# # A tibble: 2 x 2
# Predator_type     n
# <chr>           <int>
#  E                9
#  M                1


unique(Manual_time_24$serial)

#[1] 1392827 1392831 1392833 1392841 1392846 1392858 1392868 1392879 1392884 1392804




#48 h:


Manual_time_48 <-Manual_time.1 %>% filter(survival_time_manual>24&survival_time_manual<=48) %>% print()
#12 x 4 

Manual_time_48 %>% group_by(Predator_type) %>% tally()

# # A tibble: 2 x 2
# Predator_type     n
# <chr>           <int>
#  E                11
#  M                1


unique(Manual_time_48$serial)

#[1] 1392803 1392823 1392826 1392830 1392832 1392854 1392857 1392861 1392867 1392872 1392873 1392820





#Greater 48 h:


Manual_time_G48 <-Manual_time.1 %>% filter(survival_time_manual>48) %>% print()
#26 x 4 

Manual_time_G48 %>% group_by(Predator_type) %>% tally()

# # A tibble: 2 x 2
# Predator_type     n
# <chr>           <int>
#  E                25
#  M                1


unique(Manual_time_G48$serial)

#[1] 1392805 1392806 1392809 1392810 1392816 1392817 1392819 1392828 1392837 1392838 1392839 1392840 1392843 1392845 1392847 1392850 1392851 1392852 1392859
#[20] 1392860 1392871 1392876 1392881 1392889 1392892 1392895

mean(Manual_time_G48$survival_time_manual, na.rm = T)
#128.8278

sd(Manual_time_G48$survival_time_manual,na.rm = T)
#79.25573


length(Manual_time_G48$survival_time_manual)
#26

###survival plots####


Surv_plot_data<-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Summary_plots/Summary_data_April 28 2022.csv') %>% print()

Surv_plot_data_no_perc <-Surv_plot_data %>% filter(group != "Percent Total") %>% print()





#for rewriting level orders:
#https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order


level_order <- c('0-6', '6-12', '12-24', '24-48', '48+') 



ggplot(Surv_plot_data_no_perc, aes (x = factor(time, level_order), y=fish, fill = as.factor(group)))+
geom_col( width = .75, position=position_dodge(0.75), colour="black") +
labs(x=expression('Timeframe (h)'),
       y=expression('Number of Fish'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
   labs(fill='Predation Type') +
 theme(axis.text.x = element_text(color = "grey20", size = 18,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 20, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 20, hjust = .5, vjust = 2, face = "bold"),
        text=element_text(family="Times"),
       legend.text=element_text(size=14),
       legend.title=element_text(size=16, face = 'bold'),
       plot.margin = margin(1,1,1.5,1.2, "cm"))+
scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0,80, by = 5))+
  scale_fill_manual(values = c("white",'grey23', "gray81", 'gray48',  'black'))+
  annotate("text", x = c(1.2575, 2.2575, 3.2575, 4.2575, 5.2575),
           y = c(17, 26,36, 48, 74), 
           label = c('20.8%', '33.3%', '47.2%', '63.9%', '100%'), size = 6, 
           family = 'Times', fontface  = 3)
  
ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Summary_plots/Predation_by_time_June 6 2022.tiff', 
       dpi = 300, width = 14, height = 10)
ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Predation_by_time_CSAS_dec 7 2022.tiff',
dpi = 300, width = 14, height = 10)


####last predation spot#####

#Want to see where the last spot they were before they died
#Will use location of the manual pred data as the last real point in time where
#they were



Pred_location.2 <-Manual_time.1 %>% 
  rename(date_time = Last_enviro_time) %>% left_join(Manual_time) %>%
  select(serial, date_time, station, area, 
                                         habitat, farm_name, Predator_type,
                                         Last_enviro_time) %>% 
   print()




Pred_location.3 <-Pred_location.2 %>% filter(farm_name != 'Fairhaven' |  is.na(farm_name)) %>% print()
#32 x 8

#changing area and habitat to 'fairhaven' for the purposes of the graph

fair_fish <-Pred_location.2 %>% filter(farm_name == 'Fairhaven') %>%
  select(-area, -habitat) %>%
  add_column(area = 'Fairhaven', habitat = 'Fairhaven') %>% print()
#40 x 8

Pred_location.4<-bind_rows(Pred_location.3, fair_fish) %>%
  drop_na(area) %>%  print() #drop NA ensures that the two eaten quickly fish are not in the plot
#70 x 8



area_order <-Pred_location.4 %>% select(area,  habitat) %>%
   group_by(habitat) %>% unique() %>% arrange(habitat) %>% print()

area_order$habitat<-sub("(.)", "\\U\\1",area_order$habitat,perl=TRUE) #first letter uppercase




area_order.1<-c('Fairhaven', "Western Passage - inner bay", 'Passamaquoddy Bay',
                "Doyle's Passage","Little L'etete Passage",
                 "Magaguadavic River",
                "Western Passage - outer bay", "Head Harbour Passage", 'Fundy Isles',
                "Limekiln Harbour", "Maces Bay" )

Pred_location.5 <-Pred_location.4 %>% group_by(area, habitat) %>% count() %>% print()

Pred_location.5 %>% filter(habitat == 'outer bay') #17

Pred_location.5 %>% filter(habitat == 'inner bay') #10
Pred_location.5 %>% filter(habitat == 'coastal') #2
Pred_location.5 %>% filter(habitat == 'estuary') #1


#Change up the naming of some of the locations

Pred_location.6<-Pred_location.5 %>%
  mutate( area = str_replace(area, "InnerWP", "Western Passage - inner bay")) %>%
  mutate( area = str_replace(area, "OuterW_P", "Western Passage - outer bay")) %>% 
  mutate( habitat = str_replace(habitat, "estuary", "inner bay")) %>%
  print() 
  
  



Pred_location.6$habitat<-sub("(.)", "\\U\\1",Pred_location.6$habitat,perl=TRUE)
Pred_location.6

ggplot(Pred_location.6, aes (x = factor(area, area_order.1), y=n, fill = as.factor(habitat)))+
  geom_col( width = .75, position=position_dodge(0.75), colour="black") +
  coord_flip()+
  labs(x=expression('Area:'),
       y=expression('Number of Fish'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill='Region') +
  theme(axis.text.x = element_text(color = "black", size = 18,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
        axis.title.x = element_text(color = "black", size = 20, hjust = .5, vjust = 0, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 26, hjust = 0, vjust = 1, angle = 360, face = "bold"),
        text=element_text(family="Times"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16, face = 'bold'),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = seq(0,50, by = 5))+
  scale_fill_manual(values = c('grey', "yellow2",'tan', 'royalblue1' ))


#ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Summary_plots/Predation_by_area_June 6 2022.tiff',
      # dpi = 300, width = 14, height = 10)



ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Predation_by_area_CSAS_dec 7 2022.tiff',
       dpi = 300, width = 14, height = 10)


# ####first predation spot#####
# 
# #Want to see where the first spot they were before they died
# 
# 
# 
# Manual_time.first<-Manual_time.1 %>%
#   select(serial, Predator_type, Last_enviro_time) %>%
#   print()
# 
# 
# 
# 
# #join predation time to main telem file
# Telemetry_master_escapees_pred_first <-Telemetry_master_escapees_pred %>% left_join(Manual_time.first) %>% print()
# 
# #add delimator to see when the manual time is before the first non-enviro time
# 
# 
# Telemetry_master_escapees_pred_first.1 <- Telemetry_master_escapees_pred_first %>%
#   mutate(Filter_time = case_when(Last_enviro_time >= date_time ~ "Filt",
#                                  T ~ 'Keep')) %>%
#   select(date_time, transmitter, station, area, habitat, farm_id, farm_name,
#          Last_enviro_time, release_date_time, Filter_time, sensor, sensor_value) %>%
#   drop_na(Last_enviro_time) %>% filter(sensor == 'temp') %>%
#   print()
# 
# length(unique(Telemetry_master_escapees_pred_first.1$transmitter))
# #70
# 
# 
# 
# Telemetry_master_escapees_pred_first.2 <- Telemetry_master_escapees_pred_first.1 %>%
#   filter(Filter_time == 'Keep') %>% print()
# 
# #get the first point past the last enviro temp
# First_pred <-Telemetry_master_escapees_pred_first.2 %>% group_by(transmitter) %>%
#   slice(n=1) %>% print()
# 
# range(First_pred$sensor_value)
# #[1] 15.1605 35.0055
# #seems reasonible
# 
# First_pred.1 <-First_pred %>% filter(farm_name != 'Fairhaven' |  is.na(farm_name)) %>% print()
# #33 x 12
# 
# #changing area and habitat to 'fairhaven' for the purposes of the graph
# 
# fair_fish_pred <-First_pred %>% filter(farm_name == 'Fairhaven') %>%
#   select(-area, -habitat) %>%
#   add_column(area = 'Fairhaven', habitat = 'Fairhaven') %>% print()
# #37 x 12
# 
# First_pred.2<-bind_rows(First_pred.1, fair_fish_pred) %>% print()
# #70 x 12
# 
# 
# 
# area_order_pred <-First_pred.2 %>% select(area,  habitat) %>%
#   group_by(habitat) %>% unique() %>% arrange(habitat) %>% print()
# 
# area_order_pred$habitat<-sub("(.)", "\\U\\1",area_order_pred$habitat,perl=TRUE) #first letter uppercase
# 
# area_order_pred.1<-c('Fairhaven', "Western Passage - inner bay", 'Passamaquoddy Bay',
#                 "Doyle's Passage", "Little L'etete Passage",
#                 "Magaguadavic River",
#                 "Western Passage - outer bay", "Head Harbour Passage", 'Fundy Isles',
#                 "Limekiln Harbour", "Maces Bay" )
# 
# Pred_location.5 <-area_order_pred %>% group_by(area, habitat) %>% count() %>% print()
# 
# 
# WP<-Pred_location.5 %>% filter(area == 'Western Passage') %>%
#   mutate(area = case_when(habitat == "inner bay"~ "Western Passage - inner bay",
#                           T ~ "Western Passage - outer bay")) %>% print()
# 
# Pred_location.6<-Pred_location.5 %>% filter(area != 'Western Passage') %>% print()
# 
# Pred_location.7<-Pred_location.6 %>% filter(area != 'Magaguadavic River') %>% print()
# 
# Magad <- Pred_location.6 %>% filter(area == 'Magaguadavic River') %>%
#   mutate(habitat = case_when(habitat == "estuary"~ "inner bay",
#                              T ~ "")) %>% print()
# 
# 
# 
# Pred_location.8<-bind_rows(Pred_location.7, WP, Magad) %>% print()
# 
# Pred_location.8$habitat<-sub("(.)", "\\U\\1",Pred_location.8$habitat,perl=TRUE)
# Pred_location.8
# 
# ggplot(Pred_location.8, aes (x = factor(area, area_order.1), y=n, fill = as.factor(habitat)))+
#   geom_col( width = .75, position=position_dodge(0.75), colour="black") +
#   coord_flip()+
#   labs(x=expression('Area:'),
#        y=expression('Number of Fish'))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   labs(fill='Region') +
#   theme(axis.text.x = element_text(color = "black", size = 18,  face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 20, hjust = .5, vjust = 0, face = 'bold'),
#         axis.title.y = element_text(color = "black", size = 26, hjust = 0, vjust = 1, angle = 360, face = "bold"),
#         text=element_text(family="Times"),
#         legend.text=element_text(size=14),
#         legend.title=element_text(size=16, face = 'bold'),
#         plot.margin = margin(1,1,1.5,1.2, "cm"))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = seq(0,50, by = 5))+
#   scale_fill_manual(values = c('grey', "yellow2",'tan', 'royalblue1', 'red' ))
# 
# 
# ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/plots/predation/Summary_plots/Predation_by_area_first_June 6 2022.tiff',
#        dpi = 300, width = 14, height = 10)
# 
# 




