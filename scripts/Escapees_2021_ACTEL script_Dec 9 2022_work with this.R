source('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/master_files/plotDot.R')


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
library(viridis)
library(actel)
library(gWidgets2)
library(RGtk2)
library(gWidgets2)





######Step 1: read in master files ######

  Telemetry_master_escapees<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/PB_telem_master_escapee_2021_Dec 8 2022.csv") %>% 
  print()
#305,570 x 32




####Step 2: clean up data set for downstream analyses, predation stuff######

#cleaning up the data to account for predation events 

#read in vector containing pred fish id's

Manual_time<-read_csv(
    'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/ALL_PREDATION_TIMING.csv' ) %>% print()
#124,253 x 42  

str(Manual_time)
  

#filter out predated fish in the master set
Predated_fish <-Telemetry_master_escapees %>% filter(serial %in% Manual_time$serial ) %>% print()
#252,095 x 32


length(unique(Predated_fish$serial))
#72, matches original predation numbers

#add in predation timing info

Manual_time.1<-Manual_time %>% select(serial,Last_enviro_time, Predator_type) %>% group_by(serial) %>% 
  unique() %>%  print()



Predated_fish.1<-Predated_fish %>% left_join(Manual_time.1) %>% print()
#252,095 x 34

#Make a col to identify what rows are before or after predation event
#then filter out predation timepoints 


Predation_filtered <-Predated_fish.1 %>%  
  mutate(filter_col = case_when(date_time <= Last_enviro_time  ~ 'keep',
                                TRUE ~ 'toss'))%>% 
  filter(filter_col == 'keep') %>%  print()

#52,094 x 35

length(unique(Predation_filtered$serial))
#70 fish



###Step 3 Addressing non predation fish clean up######

####Now, need to get all fish that weren't part of the predation list:

pred_list<-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/survival_time_manual_Dec 14 2022.csv' ) %>% 
print()


Not_predated_fish<-Telemetry_master_escapees %>% filter(!serial %in% pred_list$serial ) %>% print()
#51,475 x 32

length(unique(Not_predated_fish$serial))
#27
#Numbers add up as 72 predated fish, plus the 27 non-predated
#total of 99 fish

unique(Not_predated_fish$serial)


###Now want to see if there are any stationary tags or anything weird going on 

#going tag by tag and just validating the data 

#Things looking at, depth profiles, do they move between receivers, are they near bottom (they like surface waters)
#temp weirdness, etc. 


unique(Not_predated_fish$serial)

#1392802 


S1392802<-Not_predated_fish %>% filter(serial == '1392802') %>% print()

S1392802_D <-S1392802 %>% filter(sensor == 'depth') %>% print()
S1392802_T <-S1392802 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392802_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392802_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Only a few points, nothing unusual in temp/depth. Seems to be sticking around fairhaven

#1392807 

S1392807<-Not_predated_fish %>% filter(serial == '1392807') %>% print()

S1392807_D <-S1392807 %>% filter(sensor == 'depth') %>% print()
S1392807_T <-S1392807 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392807_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392807_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks fine from abacus and depth plots


#1392808 

S1392808<-Not_predated_fish %>% filter(serial == '1392808') %>% print()

S1392808_D <-S1392808 %>% filter(sensor == 'depth') %>% print()
S1392808_T <-S1392808 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392808_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392808_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Looks alright overall, did do a bit of deep time for a few days but ended up in river 
#supported by increased temp and abacus data 



#1392811 

S1392811<-Not_predated_fish %>% filter(serial == '1392811') %>% print()

S1392811_D <-S1392811 %>% filter(sensor == 'depth') %>% print()
S1392811_T <-S1392811 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392811_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392811_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks alright overall




#1392812 

S1392812<-Not_predated_fish %>% filter(serial == '1392812') %>% print()

S1392812_D <-S1392812 %>% filter(sensor == 'depth') %>% print()
S1392812_T <-S1392812 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392812_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392812_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks fine overall, ends up in river (hot temps near end)


#1392813 

S1392813<-Not_predated_fish %>% filter(serial == '1392813') %>% print()

S1392813_D <-S1392813 %>% filter(sensor == 'depth') %>% print()
S1392813_T <-S1392813 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392813_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392813_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks fine overall, stays near surface, enviro temps, ends up in the river 
#Minor concern is how much its moving around, but it is a salmon so not unreasonable



#1392814 

S1392814<-Not_predated_fish %>% filter(serial == '1392814') %>% print()

S1392814_D <-S1392814 %>% filter(sensor == 'depth') %>% print()
S1392814_T <-S1392814 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392814_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392814_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Slight amount of concern it does a dirunal dive pattern
#Overall, seems to be ok I think? spends move time near surface
#A good portion of time associating with the farms (shoaling???)
#Enviro temps


#1392815 

S1392815<-Not_predated_fish %>% filter(serial == '1392815') %>% print()

S1392815_D <-S1392815 %>% filter(sensor == 'depth') %>% print()
S1392815_T <-S1392815 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392815_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392815_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks alright. Dissappears for a while though and then returns ~ 5 days later


#1392818 

S1392818<-Not_predated_fish %>% filter(serial == '1392818') %>% print()

S1392818_D <-S1392818 %>% filter(sensor == 'depth') %>% print()
S1392818_T <-S1392818 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392818_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392818_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Really wonky looking fish. after 24 h, fish appears to flatline at a single reciever
#depth and temp stay more or less consitent
#probs a mort after 24 h
#EXCLUDE!!!!



#1392822 

S1392822<-Not_predated_fish %>% filter(serial == '1392822') %>% print()

S1392822_D <-S1392822 %>% filter(sensor == 'depth') %>% print()
S1392822_T <-S1392822 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392822_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392822_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Nothing that's a huge red flag, looks alright overall
#depth pattern gets a bit inconsistent past 48 h but continues to return to farms



#1392824 

S1392824<-Not_predated_fish %>% filter(serial == '1392824') %>% print()

S1392824_D <-S1392824 %>% filter(sensor == 'depth') %>% print()
S1392824_T <-S1392824 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392824_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392824_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Looks alright, staying near surface and moving around farms a fair bit


#1392848 

S1392848<-Not_predated_fish %>% filter(serial == '1392848') %>% print()

S1392848_D <-S1392848 %>% filter(sensor == 'depth') %>% print()
S1392848_T <-S1392848 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392848_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392848_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Nothing unusual and looks alright. Couple deeper dives but mostly near surface (<10m)



#1392864 

S1392864<-Not_predated_fish %>% filter(serial == '1392864') %>% print()

S1392864_D <-S1392864 %>% filter(sensor == 'depth') %>% print()
S1392864_T <-S1392864 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392864_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392864_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks good, warmer temps might be asscoiated with river foray



#1392866 

S1392866<-Not_predated_fish %>% filter(serial == '1392866') %>% print()

S1392866_D <-S1392866 %>% filter(sensor == 'depth') %>% print()
S1392866_T <-S1392866 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392866_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392866_T) + geom_point(aes(x = date_time, y = sensor_value) )


#Fish itself looks fine, although only a few data points (~18 h)
#Goes off grid in PB but initially hung around farm and then up thru West pass


#1392869 

S1392869<-Not_predated_fish %>% filter(serial == '1392869') %>% print()

S1392869_D <-S1392869 %>% filter(sensor == 'depth') %>% print()
S1392869_T <-S1392869 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392869_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392869_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Concerend as only 5 data points and theyre all near the farm 
#My guess it died and fell out of range of recievers 
#will be excluding this one from the analysis as I don't nessarily trust it
#EXCLUDE!!!!



#1392870 

S1392870<-Not_predated_fish %>% filter(serial == '1392870') %>% print()

S1392870_D <-S1392870 %>% filter(sensor == 'depth') %>% print()
S1392870_T <-S1392870 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392870_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392870_T) + geom_point(aes(x = date_time, y = sensor_value) )

#nothing unusual with this fish, does spend alot of time with farms


#1392874 

S1392874<-Not_predated_fish %>% filter(serial == '1392874') %>% print()

S1392874_D <-S1392874 %>% filter(sensor == 'depth') %>% print()
S1392874_T <-S1392874 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392874_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392874_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Ends up in the river
#Nothing unusual about it



#1392875 

S1392875<-Not_predated_fish %>% filter(serial == '1392875') %>% print()

S1392875_D <-S1392875 %>% filter(sensor == 'depth') %>% print()
S1392875_T <-S1392875 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392875_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392875_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Nothing appears out of places, looks good





#1392878 

S1392878<-Not_predated_fish %>% filter(serial == '1392878') %>% print()

S1392878_D <-S1392878 %>% filter(sensor == 'depth') %>% print()
S1392878_T <-S1392878 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392878_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392878_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Looks fine overall 


#1392880 

S1392880<-Not_predated_fish %>% filter(serial == '1392880') %>% print()

S1392880_D <-S1392880 %>% filter(sensor == 'depth') %>% print()
S1392880_T <-S1392880 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392880_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392880_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Fish defs died by oct 5, stationary that at a single receiver during that time
#and had more or less constant depth
#However, fish is making huge amounts of runs through receivers in the first 4 days
#Dunno if I entirely trust that it wasn't eaten by a predator 
#for now going to exclude from analysis as Im reasonably certain 
#it was eaten by a fish
#For now, leaving out of analysis


#1392882 

S1392882<-Not_predated_fish %>% filter(serial == '1392882') %>% print()

S1392882_D <-S1392882 %>% filter(sensor == 'depth') %>% print()
S1392882_T <-S1392882 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392882_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392882_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Nothing out of place it seems here 


#1392885 

S1392885<-Not_predated_fish %>% filter(serial == '1392885') %>% print()

S1392885_D <-S1392885 %>% filter(sensor == 'depth') %>% print()
S1392885_T <-S1392885 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392885_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392885_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Single data point, probably a mort?
#EXCLUDING this 


#1392888 

S1392888<-Not_predated_fish %>% filter(serial == '1392888') %>% print()

S1392888_D <-S1392888 %>% filter(sensor == 'depth') %>% print()
S1392888_T <-S1392888 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392888_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392888_T) + geom_point(aes(x = date_time, y = sensor_value) )

#Small number of points but the fish is clearly moving around, stays near surface 
#no reason to exclude this one as the fish is clearly alive in the array



#1392891 

S1392891<-Not_predated_fish %>% filter(serial == '1392891') %>% print()

S1392891_D <-S1392891 %>% filter(sensor == 'depth') %>% print()
S1392891_T <-S1392891 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392891_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392891_T) + geom_point(aes(x = date_time, y = sensor_value) )

#small number of points but once again moving around the bay.
#Nothing seems out of place here, keeping it in


#1392894 

S1392894<-Not_predated_fish %>% filter(serial == '1392894') %>% print()

S1392894_D <-S1392894 %>% filter(sensor == 'depth') %>% print()
S1392894_T <-S1392894 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392894_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392894_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks fine overall


#1392899 

S1392899<-Not_predated_fish %>% filter(serial == '1392899') %>% print()

S1392899_D <-S1392899 %>% filter(sensor == 'depth') %>% print()
S1392899_T <-S1392899 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392899_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392899_T) + geom_point(aes(x = date_time, y = sensor_value) )

#looks fine overall


#1392900 

S1392900<-Not_predated_fish %>% filter(serial == '1392900') %>% print()

S1392900_D <-S1392900 %>% filter(sensor == 'depth') %>% print()
S1392900_T <-S1392900 %>% filter(sensor == 'temp') %>% print()


ggplot(data = S1392900_D) + geom_point(aes(x = date_time, y = sensor_value) )
ggplot(data = S1392900_T) + geom_point(aes(x = date_time, y = sensor_value) )


#looks fine overall


###filtering out fish that were exlcuded
#includes the following fish: 


Not_predated_fish.1<-Not_predated_fish %>% filter(!grepl( '1392818|1392869|1392880|1392885', serial)) %>% print()
#23818 x 32

length(unique(Not_predated_fish.1$serial))
#23

#started with 27 so this makes sense as four were removed 

sort(unique(Not_predated_fish.1$serial))

#recombine these fish with predated tags and write up CSV 

Full_adjusted_telem <-bind_rows(Not_predated_fish.1, Predation_filtered) %>% print()

#75,912 x 35

length(unique(Full_adjusted_telem$serial))
#93

#70 predated + 23 not predated = 93 fish total.
#2 fish eaten right away and 4 wonky non-pred fish removed
#alright!



#write out CSV

write_csv(Full_adjusted_telem, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Telemetry_master_adjusted_2021_Dec 13 2022.csv')
  

######Prep for actel input ##########


Telem_actel_df <-read_csv(
  'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Telemetry_master_adjusted_2021_Dec 13 2022.csv') %>% print()

#75,912 x 35

#Actel requires three seperate CSV files: biometrics.csv, spatial.csv, deployments.csv
#making these below:


#####biometrics CSV#####

#Going to use 'site A' as release site to avoid conflicts with 'fairhaven', the 
#first array in the set. As per talk with brent, GPS coords for
#release are 44.96448, -67.01331



biometrics<-Telem_actel_df %>% 
  select(release_date_time, serial, tag_id, total_length_mm,weight_g) %>% 
  add_column(Release.site = 'Site A') %>% 
  rename(Release.date = release_date_time,
         Serial.nr = serial,
         Signal = tag_id,
         Length.mm = total_length_mm,
         Weight.g = weight_g) %>% 
  group_by(Serial.nr, Signal) %>% slice(n=1) %>% 
  print()
#186 x 6


biometrics.1<-biometrics %>% group_by(Serial.nr) %>% 
  mutate(Placeholder = paste0(Signal,collapse = "|" )) %>% 
  select(-Signal) %>% group_by(Serial.nr) %>% slice(n=1) %>% 
  rename(Signal = Placeholder) %>% add_column(Sensor.unit = "T|D") %>% print()

#93 x 7

unique(biometrics.1$Serial.nr)



write_csv(biometrics.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/biometrics.csv')


#####Spatial file####


Reciever_info<- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiver_metadata_refined_Dec 16 2022_USE THIS.csv') %>% print
#210 x 52


Reciever_info$ret_datetime_utc<-as_datetime(Reciever_info$ret_datetime_utc, tz ='UTC')
str(Reciever_info)

#Gave a quick visual inspection here and some matching of ideal vs actual lists
#on Dec 20 2022. 

Reciever_info.1<-Reciever_info %>%
  filter(ret_datetime_utc > as_datetime('2021-09-21 01:00:00'))%>% 
  select(receiver, station, lat_deploy_dd, long_deploy_dd, area, farm_name,
         deploy_datetime_utc, ret_datetime_utc_3) %>% 
  rename(Receiver = receiver, Station.name =station, Latitude = lat_deploy_dd,
         Longitude = long_deploy_dd, Section = area, 
         Start= deploy_datetime_utc, Stop = ret_datetime_utc_3) %>% 
  mutate(Type = "Hydrophone") %>% 
  print()
#132 x 9

write_csv(Reciever_info.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/G_earth_list_2021 Escapee_Dec 9 2022.csv' )




#reading in spatial file
Spatial_file <- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Array_info_google_earth_Dec 20 2022.csv') %>% print()
#133 x 7

#write as CSV

write_csv(Spatial_file, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/spatial.csv')



#####Deployments file######

#read in currated file

#for this file, had to manaully make the name adjustments from dec 16 and add in the 'new' receiver WR-O1B
deployments_A <- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/G_earth_list_2021 Escapee_Dec 9 2022.csv') %>% print()
#132 x 9

deployments_A.1 <- deployments_A %>% select(Receiver, Station.name, Start, Stop) %>% print()
#132 x 4

write_csv(deployments_A.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/deployments.csv')



####Detections CSV###


Telem_actel_df
#75,912 x 35

#need to get receiver SN into the file so doing some quick kermin erenderfing 

Reciever_info.2 <-Reciever_info.1 %>% select(Station.name, Receiver) %>% print()
#132 x 2
names(Telem_actel_df)



detec_join.1 <-Telem_actel_df %>% 
  select(date_time, station, CodeSpace, transmitter, tag_id, sensor_value, sensor) %>% 
  rename( Timestamp = date_time, Signal = tag_id, Sensor.Value = sensor_value,
         Sensor.Unit = sensor, Station.name = station) %>% 
  left_join(Reciever_info.2) %>% 
  filter(Station.name != 'VR-100') %>%  #for now leaving out VR-100 stuff
select(-Station.name) %>% 
  select(Timestamp, Receiver, CodeSpace, Signal, Sensor.Value, Sensor.Unit) %>% 
  print()
#75,604 x 6

write_csv(detec_join.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/detections.csv')


######Actel analysis######



setwd('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022')


###visual check of array
# dot <- readDot(input = 'spatial.txt')
# 
# spatial<-loadSpatial()
# plotDot(dot = dot, spatial =spatial, coord.x = 'Longitude', coord.y = 'Latitude',
#         expand = 5, file = "test.svg")
# 
# P<-ggplot(data = spatial, aes(x = Longitude, y = Latitude, color = Array))
# P = P+geom_point()
# P
# 
# Q<-ggplot(data = dot.coords, aes(x = x, y = y, color = Array))
# Q = Q+geom_point()
# Q




####residency analysis#####

#wanted to have a 2 step jump warning in place and get res times in hours
#rather than default days
#Set section error and warning to zero as we have fish showing up on single 
#detection, no need to have it constantly ask for warnings
#As per convo with Hugo yesterday, actel with automatically pull the needed files
#straight from the directory 

Res_df<-residency(tz = "UTC", jump.warning = 2, 
                  timestep = "hours",
                  save.detections = T, 
                  save.tables.locally = TRUE, report = T,  max.interval = 15,
                  section.error = 0,
                  section.warning = 0)



#####Data frame prep#####


#checking tranmitter ID's

names(Res_df$residency.list)
length(names(Res_df$residency.list)) #93

#tags with only at one section 
N<-sapply(Res_df$residency.list, nrow)
names(N)[N ==1] 

length(names(N)[N ==1]) #29 with single array visit

#Note: all inter-array movements are even numbers


###Residency master file#####

##extract resdiency data into a dataframe

#my way of doing it
Master_res<-as_tibble(map_df(Res_df$residency.list, ~as.data.frame(.x), .id="Transmitter")) %>% 
                   select(-Index) %>% print()
#2423x4



# # #Hugo way modifed
# # 
# Master_H <- lapply(names(Res_df$residency.list), function(tag) {
# 
#   output <- data.frame(Transmitter = tag,
#                        Section = Res_df$residency.list[[tag]]$Section[],
#                        First.time = Res_df$residency.list[[tag]]$First.time[],
#                        Last.time = Res_df$residency.list[[tag]]$Last.time[])
#   return(output)
# })
# 
# Master_H.1<- as.data.frame(data.table::rbindlist(Master_H)) %>% print()
# Master_H.2<-Master_H.1 %>% as.tibble() %>% print()
# 
# 
# intersect(Master_res, Master_H.2)
# intersect(Master_H.2, Master_res)

# #Same results, all rows intersect

##extract section info

Master_section<-as_tibble(map_df(Res_df$section.movements, ~as.data.frame(.x), .id="Transmitter")) %>% 
  select(Transmitter, Section, First.array, First.station,
         Last.array, Last.station, First.time, Last.time) %>% 
 print()

#1,258x8

#Vector for station names if needed later on

QQ <- Res_df$spatial$stations %>%
  select(Standard.name, Station.name,Array, Section,  Latitude, Longitude) %>% 
print()




##Join the two files 
Master_all <- left_join(Master_res,Master_section) %>% print()

#2,423 x 8 

#write out the master file 

write_csv(Master_all, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv')



#For the sake of argument, apparently the section movements list has time in section
#Could've saved myself lot of trouble using just that as a tibble. 
#Making a seperate file just to see if its all good.
#I think I just wanted to make sure that the interlude peices were still there
#hence using the res list rather than just section movements

Master_section_easy<-as_tibble(map_df(Res_df$section.movements, ~as.data.frame(.x), .id="Transmitter")) %>% 
  print()
#1258 x 13

write_csv(Master_section_easy, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency easyway_Dec 13 2022.csv')


Master_section_easy.1 <-Master_section_easy 

Master_section_easy.1$Time.in.section <-hms(Master_section_easy.1$Time.in.section)

Master_section_easy.2<-Master_section_easy.1 %>% mutate(Hours = as.numeric(Time.in.section)) %>% 
mutate(Hours = Hours/3600) %>% print()

view(Master_section_easy.2)


my_res<-Res_times %>% drop_na(First.array) %>% mutate(Hours_og =  as.numeric(difftime(Last.time, First.time), units="hours")) %>% print()
#1258

my_res %>% filter(Hours_og %in% Master_section_easy.2$Hours) %>% print()

#prelim quick looking makes it look the same?


######Time to leave#####

#read in datafile 
Master_res_all <- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv') %>% 
  print()
#2,423 x 8


#determine fish that left Fairhaven and use those to determine time to leave



###loop function developed with Hugo
#basically, asks if a fish has less than one row and if it does, adds an NA
#looks at first row and row three as this would be site one and the next residence site 

Time_to_leave <- lapply(names(Res_df$residency.list), function(tag) {
  
  output <- data.frame(Transmitter = tag,
                       First.section = Res_df$residency.list[[tag]]$Section[1],
                       First.time = Res_df$residency.list[[tag]]$First.time[1],
                       Last.time = Res_df$residency.list[[tag]]$Last.time[1]
                          )
  if (nrow( Res_df$residency.list[[tag]]) > 1) {
    output$Next.section <- Res_df$residency.list[[tag]]$Section[3]
    output$Next.time <- Res_df$residency.list[[tag]]$First.time[3]
  } else {
    output$Next.section <- NA
    output$Next.time <- as.POSIXct(NA)
  }
  
  return(output)
}) %>% print()

Time_to_leave_df <- as.data.frame(data.table::rbindlist(Time_to_leave)) %>% print()
Time_to_leave_df_trim <- Time_to_leave_df[!is.na(Time_to_leave_df$Next.section),] %>% print()
dim(Time_to_leave_df_trim) #64 x 6 

#The remaining fish are thos that didn't make it to a next section
#They all have 'last.times' but this is explained by them last 
#appearing at the site and were probably eaten by a seal during their time at fairhaven
#or shortly after them leaving (see below)


Time_to_leave_df_trim.1 <-as_tibble(Time_to_leave_df_trim) %>% print()

#join with release time: 

A<-Res_df$status.df$Transmitter %>% as_tibble() %>% 
  rename(Transmitter = value) %>%  print()

B<-Res_df$status.df$Release.date %>% as_tibble() %>% 
  rename(Release.date = value) %>%  print()

Release_info <- cbind(A, B) %>% print()

length(match(Release_info$Transmitter, Res_df$status.df$Transmitter)) #93
length(match(Release_info$Release.date, Res_df$status.df$Release.date)) #93

Time_to_leave_df_trim.2 <- Time_to_leave_df_trim.1 %>% 
  left_join(Release_info) %>% 
  mutate (Leave_time = as.numeric(difftime(Last.time, Release.date), units="hours")) %>% 
    print()


#filter out those fish who didn't start at fairhaven:

Just_fairhaven <-Time_to_leave_df_trim.2 %>% filter(First.section == 'Fairhaven') %>% 
  print()
#60 x 8 
#so 4 fish were at non-fairhaven places to start 
#6750 not included in this list as it didn't reach anywhere else? 



mean(Just_fairhaven$Leave_time)
#[1] 9.15531

median(Just_fairhaven$Leave_time)
#[1] 6.46

range(Just_fairhaven$Leave_time)
# [1]  0.1830556 38.2591667

sd(Just_fairhaven$Leave_time)
#[1] 9.361713



####predation accounted time to leave####

#Some of the fish may have been eaten during their stay at fairhaven
#so it might look like they 'departed' the system but its really just because they
#were eaten (ie no detections beyond that)

#read in predation data

Manual_time<-read_csv(
  'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Predator files/ALL_PREDATION_TIMING.csv' ) %>% print()
#124,253 x 42  


Pred_time.1 <- Manual_time %>% select(serial, transmitter, Last_enviro_time,
    survival_time_manual) %>% 
  group_by(transmitter) %>% unique() %>% rename(Transmitter = transmitter) %>%  print()
#72 x 2



Res_time_indy_first_pred <- Just_fairhaven %>% left_join(Pred_time.1) %>% 
  mutate(predated = case_when(Last_enviro_time <= Last.time ~ 'yes',
                               T ~ 'no')) %>% 
  print()
#60 x 12

#How many NA's are with serial number
sum(is.na(Res_time_indy_first_pred$serial))
#19, out of the 27, 4 excluded and one never made it anywhere elese for non eaten

#manually checking them to make sure they are all 'not eaten fish', visual check confirms that
View(Full_adjusted_telem %>% group_by(serial) %>% select(transmitter, serial, Predator_type) %>% unique())




Last_time_predated <- Res_time_indy_first_pred %>% filter(predated == "no") %>%
    unique() %>% print()
#60 x 12

#So only 60 fish were not eaten at the site right away


mean(Last_time_predated$Leave_time )
#9.15531

median(Last_time_predated$Leave_time)
#[1] 6.46

range(Last_time_predated$Leave_time)
# [1]  0.1830556 38.2591667

sd(Last_time_predated$Leave_time)
# 9.361713

length(Last_time_predated$Leave_time )
#60

#Results appeared the same as the above analysis fo the removals likely accounted for all of the
#eaten fish inat the fair site


####Non-fairhaven first starts####

#all reached WP first before fairhaven (ie no detections from get go)
#want to see what their ave time is
#leave time in this case will be from release time to frist time as prersumably they
#zoomed right off the site in this case

Not_fair <- Time_to_leave_df_trim.2 %>% filter(First.section != 'Fairhaven') %>% 
  mutate (Leave_time = as.numeric(difftime( First.time, Release.date), units="hours")) %>% 
  
   print()
# 4 x 8 
#All WP fish


mean(Not_fair$Leave_time)
#[1]3.149

median(Not_fair$Leave_time)
#[1] 2.468

range(Not_fair$Leave_time)
#[1] [1] 1.633056 6.027500

sd(Not_fair$Leave_time)
#[1] 2.007336


####Directions Post-release#####

#want to see the sequence of where the fish are traveling as they move from the fairhaven sight

#read in datafile 
Master_res_all <- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv') %>% 
  print()

#first spot was fairhaven for all released fish 
#just making an easy df to combine that info with later hits
#We know all fish were released at fairhaven for sure weather or not they hit 
#a subsequent array

 First_hit<-as_tibble(unique(Master_res_all$Transmitter)) %>% 
   rename(Transmitter = value) %>% 
  add_column(Order = 1) %>% add_column(Section = "Fairhaven") %>% 
   add_column(Direction = "Fairhaven - Release") %>% 
   add_column(First.array = "A") %>% print()
#93 x 5
 
 
 ###Second hit####
 
#two steps here, the fairhaven start fish's second hit will be correct (ie their first array)
#Non fair fish first hit is technically their second hit, need to isolate
 
#first drop all of the interarray travel
#as they don't have stations or arrays, can use NA drop


Master_res_all.1<-Master_res_all%>% drop_na(First.array) %>% print()
 #1258 x 8, which matches section movements file (ie no interlude points)
  
 





###For fairhaven starting fish first
 
 
#Isolate just the fish who started at fairhaven

#Now just want the second row for each fish as that would be hit #2 

Second_hit_fair_start <-Master_res_all.1 %>% 
  filter(Transmitter %in% Just_fairhaven$Transmitter) %>% 
 group_by(Transmitter) %>% 
  slice(2) %>%  add_column(Order = 2) %>%
  select(Transmitter, Section, Order, First.array) %>% 
  print()
#60 x 4  

length(unique(Second_hit_fair_start$Transmitter))
#60

#For non-fair fish, their first hit will be array number 1 as they weren't detected at fair first
#So just need to select the first value here

Second_hit_not_fair <-Master_res_all.1 %>% 
  filter(Transmitter %in% Not_fair$Transmitter) %>% 
  group_by(Transmitter) %>% slice(1) %>% 
  add_column(Order = 2) %>% 
  select(Transmitter, Section, Order, First.array) %>% 
  print()
  #4 x 4


length(unique(Second_hit_not_fair$Transmitter))
#4, matches above


#Bind the two two sets together

Second_hit_all <- bind_rows(Second_hit_fair_start, Second_hit_not_fair) %>% 
  print()
#64 x 4

Second_hit_all %>%group_by(Section) %>%  count(Section)

# # A tibble: 3 x 2
# # Groups:   Section [3]
# Section                  n
# <chr>                <int>
#   1 Head_Harbour_Passage     3
# 2 InnerWP                 19
# 3 OuterW_P                42
# 
# 


#Need to also add in a label for which direction they're going

unique(Second_hit_all$Section)

Second_hit_all.1 <- Second_hit_all %>% 
  mutate(Direction = case_when(Section == "Head_Harbour_Passage" ~ "Fundy",
                              Section == "OuterW_P" ~ "Fundy",
                               TRUE ~ 'Passamaquoddy Bay'))%>% 
           print()




#####Third spot####


#repeat as above

#third hit is thrid array for fair start fish
Third_hit_Fair<- Master_res_all.1 %>% 
  filter(Transmitter %in% Just_fairhaven$Transmitter) %>% 
  group_by(Transmitter) %>% 
  slice(3) %>%  add_column( Order = 3) %>%
  select(Transmitter, Section, Order, First.array) %>% 
  print()
#58 x 4

#Second hit is thrid array for non-fair fish
third_hit_not_fair <-Master_res_all.1 %>% 
  filter(Transmitter %in% Not_fair$Transmitter) %>% 
  group_by(Transmitter) %>% slice(2) %>% 
  add_column(Order = 3) %>% 
  select(Transmitter, Section, Order, First.array) %>% 
  print()
#4 x 4


#combine and add in first direction info

Third_hit_all <- bind_rows(Third_hit_Fair, third_hit_not_fair) %>% 
  left_join(Second_hit_all.1 %>% select(Transmitter, Direction)) %>% 
  print()
#62 x 5




####fourth hit#####



#repeat as above

fourth_hit_Fair<- Master_res_all.1 %>% 
  filter(Transmitter %in% Just_fairhaven$Transmitter) %>% 
  group_by(Transmitter) %>% 
  slice(4) %>%  add_column( Order = 4) %>%
  select(Transmitter, Section, Order, First.array) %>% 
  print()
#55 x 4


fourth_hit_not_fair <-Master_res_all.1 %>% 
  filter(Transmitter %in% Not_fair$Transmitter) %>% 
  group_by(Transmitter) %>% slice(3) %>% 
  add_column(Order = 4) %>% 
  select(Transmitter, Section, Order, First.array) %>% 
  print()
#4 x 4


#combine and add in first direction info

fourth_hit_all <- bind_rows(fourth_hit_Fair, fourth_hit_not_fair) %>% 
  left_join(Second_hit_all.1 %>% select(Transmitter, Direction)) %>% 
  print()
#59 x 5



###all direction together####

All_direction<-bind_rows(First_hit, Second_hit_all.1, Third_hit_all, fourth_hit_all) %>% print()
#278 x 5

write_csv(All_direction, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Salmon_direction_post_release_dec 21 2022.csv')


###counts

All_direction <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Salmon_direction_post_release_dec 21 2022.csv') %>% print()


All_direction_counts <-All_direction %>% 
  group_by(Order,Section, Direction )%>% 
  count() %>% 
  print()



####Direction Plot out#####

windowsFonts(Times=windowsFont("TT Times New Roman")) 



#renaming the sites

unique(All_direction_counts$Section)

All_direction_counts.1 <-All_direction_counts %>% 
  mutate( Section = str_replace(Section, "InnerWP", "Western Passage - Passamaquoddy")) %>%
  mutate( Section = str_replace(Section, "OuterW_P", "Western Passage - Fundy")) %>% 
  mutate( Section = str_replace_all(Section, "_", " ")) %>% 
  print()
  

All_direction_counts.2 <-All_direction_counts.1

unique(All_direction_counts.1$Section)

   
Array_labs <- c("", "First array", "Second array", 'Third array')
names(Array_labs) <- c("1", "2", "3", '4')
 
Array_labs


ggplot(All_direction_counts.2, aes(x = Section, y = n, fill = as.factor(Direction), colour="black"))+
  geom_col(alpha = 0.8, position = position_dodge2(width = 9, preserve = "single")) +
  coord_flip()+
  facet_grid(rows = vars(Order), scales = "free_y", switch = "y", space = "free_y",
             labeller = labeller(Order = Array_labs))+
  labs(fill='Intial direction', y = 'Total number of fish') +
  theme_bw()+
   theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    strip.text.y = element_text(angle = 270, face = "bold.italic", size = 14, vjust =1, family = 'Times',),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),                           
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),                           
    axis.title.y = element_blank(),                       
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank()
    )+

theme(axis.text.x = element_text(color = "black", size = 12,  face = "plain"),
      axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
      axis.title.x = element_text(color = "black", size = 20,  vjust = 0.5, face = 'bold'),
      axis.title.y = element_text(color = "black", size = 28,  face = "bold", vjust = 2, angle = 90),
      
      text=element_text(family="Times"),
      legend.text=element_text(size=14),
      legend.title=element_text(size=16, face = 'bold'),
      plot.margin = margin(1,1,1.5,1.2, "cm"))+
scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0,100, by = 10))+
 scale_fill_manual(values = c( "#D55E00", 'black', 'grey87'))+
scale_color_manual(values =c('black'))+
guides( color = "none")+
  annotation_custom(grid::linesGrob(y = c(0, 0), gp = grid::gpar(lwd = 2))) +
  annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 2))) 


ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Direction_plot_dec 22 2023.tiff',
       dpi = 300, height = 10, width = 16)





########River fish##########

Master_res_all <- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv') %>% print()
  #2,423 x 8

#Want to see which fish made it to the river and to get residency times there

unique(Master_res_all$Section)

#Identify fish that were resident at rivers

river_fish <-Master_res_all %>% filter(grepl('River', Section)) %>% 
     print()


#Get out the indy fishes
river_fish.1 <-as_tibble(unique(river_fish$Transmitter)) %>% print()

#14

#Isolate just those fish from the full data set to be able to do analyses

River_fish_df <- Master_res_all %>% filter(Transmitter %in% river_fish.1$value) %>% print()
#580

#drop interlude sections, get just river sections, and total time spent there

River_fish_df.1 <- River_fish_df %>%  filter(!is.na(First.array)) %>% 
  filter(grepl('River', Section)) %>%
  mutate(Total_time = as.numeric(difftime(Last.time,First.time, ), units="hours")) %>% 
  print()
  
#23 x 9




#how many unique fish went to each


MM<-River_fish_df.1 %>% group_by(Section, Transmitter) %>% count() %>% print()


# # A tibble: 19 x 3
# # Groups:   Section, Transmitter [19]
# Section            Transmitter       n
# <chr>              <chr>         <int>
#   1 Bocabec_River      A69-9007-6756     1
# 2 Bocabec_River      A69-9007-6770     1
# 3 Bocabec_River      A69-9007-6928     1
# 4 Digdeguash_River   A69-9007-6756     1
# 5 Digdeguash_River   A69-9007-6770     1
# 6 Digdeguash_River   A69-9007-6928     1
# 7 Lepreau_River      A69-9007-6768     1
# 8 Lepreau_River      A69-9007-6934     1
# 9 Magaguadavic_River A69-9007-6770     1
# 10 Magaguadavic_River A69-9007-6772     3
# 11 Magaguadavic_River A69-9007-6784     1
# 12 Magaguadavic_River A69-9007-6826     2
# 13 Magaguadavic_River A69-9007-6894     1
# 14 Magaguadavic_River A69-9007-6896     1
# 15 Saint_Croix_River  A69-9007-6762     1
# 16 Saint_Croix_River  A69-9007-6780     1
# 17 Saint_Croix_River  A69-9007-6864     1
# 18 Saint_Croix_River  A69-9007-6874     2
# 19 Saint_Croix_River  A69-9007-6934     1
# 

#on a per river basis

MM.1<-MM %>% group_by(Section) %>% count() %>% print()

# # A tibble: 5 x 2
# # Groups:   Section [5]
# Section                n
# <chr>              <int>
#   1 Bocabec_River          3
# 2 Digdeguash_River       3
# 3 Lepreau_River          2
# 4 Magaguadavic_River     6
# 5 Saint_Croix_River      5
# > 


#Total time by receiver:
options(pillar.sigfig=6)
River_times<-River_fish_df.1 %>% group_by(Section) %>% tally(Total_time) %>% print()

# # A tibble: 5 x 2
# Section                    n
# <chr>                  <dbl>
#   1 Bocabec_River        16.8664
# 2 Digdeguash_River     36.6958
# 3 Lepreau_River        14.0583
# 4 Magaguadavic_River 4608.94  
# 5 Saint_Croix_River   373.872 

#Res times on a per fish baiss:


River_times.1 <-River_times %>% rename(time = n) %>% left_join(MM.1) %>% 
  mutate(per_fish = time/n) %>% print()


# Joining, by = "Section"
# # A tibble: 5 x 4
# Section                 time     n  per_fish
# <chr>                  <dbl> <int>     <dbl>
#   1 Bocabec_River        16.8664     3   5.62213
# 2 Digdeguash_River     36.6958     3  12.2319 
# 3 Lepreau_River        14.0583     2   7.02917
# 4 Magaguadavic_River 4608.94       6 768.157  
# 5 Saint_Croix_River   373.872      5  74.7745 
# > 


#want to see who all went to more than one river
Rivers_visited.1<-River_fish_df.1 %>% group_by(Transmitter ) %>% count() %>% print()

Rivers_visited.1 %>% filter(n>1)  %>%  print()

# Transmitter       n
# <chr>         <int>
#   1 A69-9007-6756     2
# 2 A69-9007-6770     3
# 3 A69-9007-6772     3
# 4 A69-9007-6826     2
# 5 A69-9007-6874     2
# 6 A69-9007-6928     2
# 7 A69-9007-6934     2



#Magadavic has one fish that was there forever, just want to see the breakdown:


River_fish_df.1 %>% filter(grepl('Magaguadavic_River', Section)) %>% print()
#fish A69-9007-6894 entered in Sept and left March following year

River_fish_df.1 %>% filter(Transmitter == 'A69-9007-6894') %>% 
  filter(grepl('Magaguadavic_River', Section)) %>% print()




#Want to get fish  A69-9007-6770 for an example in the results:

River_fish_df.1 %>% filter(Transmitter == 'A69-9007-6770')


#####First river hit timing#####

#want to see how long after release fish took to get to a fiver 

#first, filter out river fish from the master file

River_timing<-Master_res_all %>% filter(Transmitter %in% River_fish_df.1$Transmitter) %>% print()

#Add in release time

Telem_actel_df.1<-Telem_actel_df %>% 
  select(transmitter, release_date_time) %>% 
  rename(Transmitter = transmitter) %>% 
  group_by(Transmitter) %>% 
  unique() %>%
  print()

River_timing.1 <-River_timing %>% left_join(Telem_actel_df.1) %>% print()

#isolate river hits, drop out interperoids and select out first river

River_timing.2<-River_timing.1 %>% filter(grepl('River', Section)) %>% 
  group_by(Transmitter) %>% drop_na(First.array) %>% slice(n=1) %>% 
  print()
#14 x 9


#Time diff from release to first 

River_timing.3 <-River_timing.2 %>% 
  mutate(First_river = as.numeric(difftime(First.time, release_date_time), units="hours")) %>% 
  print()
#14 x 10


#basic stats 

mean(River_timing.3$First_river)
#[1] 87.40929

median(River_timing.3$First_river)
#[1] 70.05528

range(River_timing.3$First_river)
# 24.78472 222.67028

sd(River_timing.3$First_river)
#59.31196

######Depth Distribution ######

#read in the master telementry data, predator adjusted


Telem_master_file_adjust <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Telemetry_master_adjusted_2021_Dec 13 2022.csv') %>% print()
#75912 x 35

#Filter out the depth tags

Telem_master_file_adjust_depth <- Telem_master_file_adjust %>% filter(station == 'FI-11') %>% print()#38248 X 35

unique(Telem_master_file_adjust_depth$area)

#simplify dataframe

depth_df <-Telem_master_file_adjust_depth %>% 
  select(date_time,transmitter,sensor_value, receiver, release_date_time) %>% print()
#38248 x 5


#next, get time difference between ping and release. Will be used for some binning 
#at a later timepoint 

depth_df.1 <- depth_df %>% 
  mutate(time_diff = as.numeric(difftime( date_time, release_date_time), units="hours")) %>% 
  print()
 

#Binning the data on set intervals


depth_df.2<-depth_df.1 %>% mutate(Bin_time = cut(time_diff, 
                                     breaks=c(0,1,2,4, 6, 8, 12, 18, 24, 
                                              36,48,72,96))) %>% 
  print()
#38,248


#get fish depth means by bin group

depth_df.3<-depth_df.2 %>% group_by(transmitter, Bin_time) %>% 
  dplyr::summarize(Mean = mean(sensor_value , na.rm=T)) %>% print()

#719 x 3


#Determine average depth by time for all fish per bin

depth_df.4_mean <-depth_df.3 %>% group_by(Bin_time) %>% 
  dplyr::summarize(Mean_collective = mean(Mean, na.rm=TRUE), 
                   SD = sd(Mean, na.rm=TRUE),
                   Count =n()
                   ) %>% drop_na(Bin_time) %>% 
   print()

#make labels for graph
Bin_time_labels <- as_tibble(c('0-1', '1-2', '2-4', '4-6', '6-8', '8-12', '12-18',
                     '18-24', '24-36', '36-48','48-72', '72-96')) %>% 
  rename(Time_range =value) %>% print()


#bind the two together
depth_df.4_mean.1 <-bind_cols(depth_df.4_mean, Bin_time_labels) %>% print()

depth_df.4_mean.1$Time_range<-as.factor(depth_df.4_mean.1$Time_range)
depth_df.4_mean.1

#plot it out 

depth_df.4_mean.1$Time_range <- factor(depth_df.4_mean.1$Time_range, levels=c('0-1', '1-2', '2-4', '4-6', '6-8', '8-12', '12-18',
                                                                                '18-24', '24-36', '36-48','48-72', '72-96'))  



windowsFonts(Times=windowsFont("TT Times New Roman")) 


ggplot(depth_df.4_mean.1, aes (x = Time_range, y=Mean_collective, fill = Time_range))+
  geom_col( width = .75, position=position_dodge(0.75), colour="black") +
  geom_errorbar(aes(ymin=Mean_collective, ymax=Mean_collective+SD), width=.2,
                position=position_dodge(.9))+
  labs(x=expression('Time Range (h)'),
       y=expression('Depth (m)'))+
   theme_bw()+
   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "black", size = 18,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain"),
        axis.title.x = element_text(color = "black", size = 24, hjust = .5, vjust = 0, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 26,  vjust = 2,  face = "bold"),
   text=element_text(family="Times"),
        legend.position="none",
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_reverse(expand = c(0, 0), breaks = seq(0,30, by = 5), limits = c(30, 0) )+
    scale_x_discrete(position = 'top')+
scale_fill_grey()+
annotate("text", x = 1.2, y = 29, label = "Mean Depth",
         fontface ="bold", colour = 'black',size = 8,  family  = 'Times')
  
#ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Plots/Mean depth_binned_June 15 2022.tiff",
       #dpi = 300, height = 10, width = 16)



ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Mean depth_binned_Dec 8 2022.tiff",
       dpi = 300, height = 10, width = 16)


#### min and Max depth values####


depth_df.range<-depth_df.1 %>% mutate(Bin_time = cut(time_diff, 
                                                 breaks=c(0,1,2,4, 6, 8, 12, 18, 24, 
                                                          36,48,72,96))) %>% 
  print()
#38,248


#get fish depth max/min values by bin group

depth_df.range.2<-depth_df.range %>% group_by(transmitter, Bin_time) %>% 
  dplyr::summarize(Max_val = max(sensor_value , na.rm=TRUE), 
                   Min_val = min(sensor_value, na.rm= T)) %>% print()

#719 x 4


Range_mean <-depth_df.range.2 %>% group_by(Bin_time) %>% 
  dplyr::summarize(Mean_max = mean(Max_val , na.rm=TRUE), 
                   SD_max = sd(Max_val, na.rm=TRUE),
                   Mean_min = mean(Min_val , na.rm=TRUE), 
                   SD_min = sd(Min_val, na.rm=TRUE),
                   Count =n()
  ) %>% drop_na(Bin_time) %>% 
  print()



#make labels for graph
Bin_time_labels <- as_tibble(c('0-1', '1-2', '2-4', '4-6', '6-8', '8-12', '12-18',
                               '18-24', '24-36', '36-48','48-72', '72-96') ) %>% 
  rename(Time_range =value) %>% print()
che <-as_tibble(c('(0,1]', '(1,2]', '(2,4]', '(4,6]', '(6,8]', '(8,12]', 
  '(12,18]','(18,24]', '(24,36]', '(36,48]','(48,72]', 
  '(72,96]')) %>% 
  rename(Bin_time =value) %>% print()

labels_ranges <-bind_cols(Bin_time_labels, che) %>% print()


Range_mean.1 <-Range_mean %>% left_join(labels_ranges) %>% print()

depth_df.range.3 <-depth_df.range.2 %>% left_join(labels_ranges) %>% 
  drop_na(Time_range) %>% print()
#691


#Plot it out as a scatter plot
                                                       


####### Max figure

windowsFonts(Times=windowsFont("TT Times New Roman")) 


depth_df.range.3$Time_range <- factor(depth_df.range.3$Time_range, levels=c('0-1', '1-2', '2-4', '4-6', '6-8', '8-12', '12-18',
                                                                              '18-24', '24-36', '36-48','48-72', '72-96'))  


ggplot(data= depth_df.range.3, aes (x = Time_range, y=Max_val, fill = Time_range))+
  geom_point(position=position_jitter(width = .05, height = 0), colour="black")+
  geom_pointrange(data = Range_mean.1,aes (x = Time_range, y=Mean_max, 
                                           ymin = Mean_max, ymax = Mean_max+SD_max),shape = 17, size =.5,colour = 'red',
             position=position_nudge(x = -.15, y = 0))+
  labs(x=expression('Time Range (h)'),
       y=expression('Depth (m)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "black", size = 18,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain"),
        axis.title.x = element_text(color = "black", size = 24, hjust = .5, vjust = 0, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 26,  vjust = 2,  face = "bold"),
        text=element_text(family="Times"),
        legend.position="none",
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_reverse(expand = c(0, 0), breaks = seq(0,100, by = 10), limits = c(100, 0) )+
  scale_x_discrete(position = 'top')+
  annotate("text", x = 1.1, y = 97, label = "Max Depth",
           fontface ="bold", colour = 'black',size = 8,  family  = 'Times')

  

#ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Plots/max depth_binned_June 15 2022.tiff",
 #      dpi = 300, height = 10, width = 16)

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/max depth_binned_CSAS_dec 8 2022.tiff",
       dpi = 300, height = 10, width = 16)


####Min figure

windowsFonts(Times=windowsFont("TT Times New Roman")) 


ggplot(data= depth_df.range.3, aes (x = Time_range, y=Min_val, fill = Time_range))+
  geom_point(position=position_jitter(width = .05, height = 0), colour="black")+
  geom_pointrange(data = Range_mean.1,aes (x = Time_range, y=Mean_min, 
                                           ymin = Mean_min, ymax = Mean_min+SD_min),shape = 17, size =.5,colour = 'red',
                  position=position_nudge(x = -.15, y = 0))+
  labs(x=expression('Time Range (h)'),
       y=expression('Depth (m)'))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(color = "black", size = 18,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 18, face = "plain"),
        axis.title.x = element_text(color = "black", size = 24, hjust = .5, vjust = 0, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 26,  vjust = 2,  face = "bold"),
        text=element_text(family="Times"),
        legend.position="none",
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_reverse(expand = c(0, 0), breaks = seq(0,50, by = 5), limits = c(50, 0) )+
  scale_x_discrete(position = 'top')+ 
   annotate("text", x = 1.1, y = 48, label = "Min Depth",
           fontface ="bold", colour = 'black',size = 8,  family  = 'Times')

#ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Plots/Min depth_binned_June 15 2022.tiff",
 #      dpi = 300, height = 10, width = 16)

ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Min depth_binned_CSAS_Dec 8 2022.tiff",
       dpi = 300, height = 10, width = 16)


####Depth at fairhaven#####

Fair_only_depth<-Telem_master_file_adjust_depth %>% filter(area == 'Fairhaven') %>% print()
#23924


#median depth
Fair_only_depth.1 <-Fair_only_depth %>% group_by(transmitter) %>% 
dplyr::summarize(Mean_fish = mean(sensor_value , na.rm=TRUE), 
                min_fish = min(sensor_value, na.rm=TRUE),
                Max_fish = max(sensor_value, na.rm = T),
                median_fish = median(sensor_value, na.rm = T)) %>% 
  print()

# # A tibble: 92 x 5
# transmitter   Mean_fish    min_fish Max_fish median_fish
# <chr>             <dbl>       <dbl>    <dbl>       <dbl>
#   1 A69-9007-6751   7.60466 2.7082       11.4343      6.6199
# 2 A69-9007-6753   9.45469 0.000100000  24.0721      9.9298
# 3 A69-9007-6755   8.21981 1.8055       21.0631      7.2217
# 4 A69-9007-6757   1.71530 0.000100000  14.7442      1.5046
# 5 A69-9007-6759   3.18349 1.5046        8.4253      2.1064
# 6 A69-9007-6761   3.60826 0.301        22.8685      2.7082
# 7 A69-9007-6763  10.5905  0.6019       32.7982      8.7262
# 8 A69-9007-6765   2.29757 0.000100000  13.5406      2.1064
# 9 A69-9007-6767   1.49381 0.301        15.346       0.9028
# 10 A69-9007-6769  10.3511  8.1244       12.337      11.7352
# # ... with 82 more rows
# > 




Fair_only_depth.2.<-Fair_only_depth.1 %>% dplyr::summarize(Mean_fish_ave = mean(Mean_fish , na.rm=TRUE), 
                                                           min_ave = mean(min_fish, na.rm=TRUE),
                                                           Max_ave = mean(Max_fish, na.rm = T),
                                                           median_ave = mean(median_fish, na.rm = T),
                                                           
                                                           SD_Mean_fish = sd(Mean_fish, na.rm=TRUE), 
                                                           SD_min_ave = sd(min_fish, na.rm=TRUE),
                                                           SD_Max_ave = sd(Max_fish, na.rm = T),
                                                           SD_median_ave = sd(median_fish, na.rm = T)) %>% 
  print()

# 
# # A tibble: 1 x 8
# Mean_fish_ave     min_ave     Max_ave     median_ave    SD_Mean_fish    SD_min_ave    SD_Max_ave    SD_median_ave
# <dbl>               <dbl>     <dbl>       <dbl>           <dbl>           <dbl>         <dbl>         <dbl>
#6.10876            1.54385     20.6445     5.20371                4.58500       3.31972         12.0100       5.11613


######Residence time by site######

Res_time_indy<- read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv') %>% 
  print()

#2423 x 8


#below file is the alternate way of getting the timing using actel itself 
# Master_section_easy<-read_csv( 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency easyway_Dec 13 2022.csv') %>% print()
# 
# Master_section_easy.1 <-Master_section_easy 
# 
# Master_section_easy.1$Time.in.section <-hms(Master_section_easy.1$Time.in.section)
# 
# Master_section_easy.2<-Master_section_easy.1 %>% mutate(Hours = as.numeric(Time.in.section)) %>% 
#   mutate(Hours = Hours/3600) %>% print()


Telem_actel_df <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Telemetry_master_adjusted_2021_Dec 13 2022.csv') %>% print()
#75912 x 8


#First need to get some additional info included in data frame

Telem_actel_df.1<-Telem_actel_df %>% 
  select(transmitter, release_date_time) %>% 
  rename(Transmitter = transmitter) %>% 
 group_by(Transmitter) %>% 
  unique() %>%
  
    print()
#186 x 2


#adding in the release time so we can segergate by time frame
Res_times <-Res_time_indy %>%  left_join(Telem_actel_df.1) %>% print()
#2423 x 9


Res_times.1 <-Res_times %>% 
  mutate(time_diff_block = as.numeric(difftime( Last.time, release_date_time), units="hours")) %>% 
  mutate(Time_block = case_when(time_diff_block <= 24 ~ "24",
                                time_diff_block <= 48 ~ "48",
                                time_diff_block <=96 ~'96',
                                T ~ 'Above')) %>% 
  print()
#2,423 x 11


#an alternative way of doing it without using the case when function
#Produces same result as the case when, did a quick test on Jan 9 2022


# Res_times.1 <-Res_times %>%
#   mutate(time_diff_block = as.numeric(difftime( Last.time, release_date_time), units="hours")) %>% 
#   mutate(Bin_time = cut(time_diff_block, 
#                                                  breaks=c(0, 24, 
#                                                           48,96))) %>% 
#   print()
  

#same as above but with the 'easy' approach

# Master_section_easy.3 <-Master_section_easy.2 %>%
#   left_join(Telem_actel_df.1) %>%
#   mutate(time_diff_block = as.numeric(difftime( Last.time, release_date_time), units="hours")) %>%
#   mutate(Time_block = case_when(time_diff_block <= 24 ~ "24",
#                                 time_diff_block <= 48 ~ "48",
#                                 time_diff_block <=96 ~'96',
#                                 T ~ 'Above')) %>%
#   print()
# #1258x 11



  
 
Res_times.1 %>% filter(Time_block <= '48') #1339 x 11



Res_times.2<-Res_times.1 %>% filter(Time_block != 'Above') %>% print()
#1683 x 11

Res_times.2$Time_block<-as.factor(Res_times.2$Time_block)



options(pillar.sigfig = 6)
options(digits = 4)


#removing the inbetween cases, they don't have aarays so can use NA drop
#also getting the res time for each row
Res_times.3<-Res_times.2 %>% drop_na(First.array) %>% 
  mutate(Res_time = as.numeric(difftime( Last.time, First.time), units="hours") ) %>% 
  print()
#880 x 11



####Total Residence time by site#####


#Want to split this up into three chunks to get at 24, 48, and 96 h post-reease


####24 h fish only####


total_24_res <-Res_times.3 %>% filter(Time_block == 24) %>% print()
#466 x 12




#getting total time and the amount of fish at each receiver for that time

total_24_res.1 <-total_24_res %>% group_by(Section) %>% 
  summarise(Fish_numb = n_distinct(Transmitter), Total_time = sum(Res_time)) %>% 
  print()  
  

#getting the total time on per fish basis and adding in some extra stuff
total_24_res.2 <-total_24_res.1 %>% 
  mutate(Fish_time = Total_time/Fish_numb) %>% 
  add_column(Time = 24) %>% 
  print()
#8


####Easy approach
# 
# T24_easy <-Master_section_easy.3 %>% filter(Time_block == 24) %>%
#   group_by(Section) %>%
#   summarise(Fish_numb = n_distinct(Transmitter), Total_time = sum(Hours)) %>% print()
#   mutate(Fish_time = Total_time/Fish_numb) %>% 
#   add_column(Time = 24) %>% print()


#Came out the same as my original way so nothing to be concerned about, same result, two different cal ways
#The bonus with my approach is you do get the inter-section peorids if they are needed


####48 h fish only####


total_48_res <-Res_times.3 %>% filter(Time_block == 48) %>% print()
#236 x 11


#getting total time and the amount of fish at each receiver for that time

total_48_res.1 <-total_48_res %>% group_by(Section) %>% 
  summarise(Fish_numb = n_distinct(Transmitter), Total_time = sum(Res_time)) %>% 
  print()  


#getting the total time on per fish basis and adding in some extra stuff
total_48_res.2 <-total_48_res.1 %>% 
  mutate(Fish_time = Total_time/Fish_numb) %>% 
  add_column(Time = 48) %>% 
  print()
#13

  

####96 h fish only####


total_96_res <-Res_times.3 %>% filter(Time_block == 96) %>% print()
#178 x 11


#getting total time and the amount of fish at each receiver for that time

total_96_res.1 <-total_96_res %>% group_by(Section) %>% 
  summarise(Fish_numb = n_distinct(Transmitter), Total_time = sum(Res_time)) %>% 
  print()  


#getting the total time on per fish basis and adding in some extra stuff
total_96_res.2 <-total_96_res.1 %>% 
  mutate(Fish_time = Total_time/Fish_numb) %>% 
  add_column(Time = 96) %>% 
  print()
#16


#combine all three together

All_res_times <-bind_rows(total_24_res.2, total_48_res.2, total_96_res.2) %>% 
  print()
#37 x 5


#write out the file

write_csv(All_res_times, "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Total_residence_times_Jan 3 2023.csv")


All_res_times %>% filter(Section == 'Fairhaven')

####residency graphs#####

All_res_times<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Total_residence_times_Jan 3 2023.csv") %>% print()
#37 x 5

#first, want to rename some of the sections to their full name


All_res_times.1 <- All_res_times %>%
  mutate( Section = str_replace(Section, "InnerWP", "Western Passage - Passamaquoddy")) %>%
  mutate( Section = str_replace(Section, "OuterW_P", "Western Passage - Fundy")) %>% 
  mutate( Section = str_replace_all(Section, "_", " ")) %>% 
  print()

All_res_times.1$Time <- as.factor(All_res_times.1$Time)
All_res_times.2 <-All_res_times.1 %>% group_by(Time) %>% arrange(Time, Section) %>% print()


windowsFonts(Times=windowsFont("TT Times New Roman")) 



#below used for making some labels on the panels 
Time_labs <- c( "0-24 h", "24-48 h", '48-96 h')
names(Time_labs) <- c("24", "48", "96")

#below is the dummy dataframe needed for labels 

ann_text <- data.frame(Total_time = c(650,650,650),
                       Section = c("Fairhaven", "Fairhaven", "Fairhaven"),
                       lab = c( "0-24 h", "24-48 h", '48-96 h'),
                       Time = factor(c(24,48,96), levels = c(24,48,96))) %>% print()
        


#Use this to manually reorder the sites

All_res_times.2$Section <- factor(All_res_times.2$Section, levels = c(
"The Wolves","Quoddy Narrows",                                                         
 "Lubec Narrows",  "Friar's Bay",  "Limekiln Harbour","Little L'etete Passage",                                                                        
 "Big L'etete Passage","Fundy Isles","Head Harbour Passage","Western Passage - Fundy",                          
 "Doyle's Passage" ,"Magaguadavic River","Digdeguash River" ,"Bocabec River" ,
"Saint Croix River","Passamaquoddy Bay","Western Passage - Passamaquoddy","Fairhaven"))


length(unique(All_res_times.2$Section))
#Need 18 colours....



library(grafify)

####Total time plot by site

ggplot(All_res_times.2, aes(x = Section, y = Total_time, fill = as.factor(Section), colour="black"))+
  geom_col(alpha = 0.8, width = 0.85, position = "dodge") +
  coord_flip()+

  facet_grid(rows = vars(Time), scales = "free_y", switch = "y", space = "free_y",
             labeller = labeller(Order = Time_labs))+
  labs(fill='Section', y = 'Total residency time (h)') +
  theme_bw()+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    strip.text.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),                           
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),                           
    axis.title.y = element_blank(),                       
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )+
  
  theme(axis.text.x = element_text(color = "black", size = 16,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
        axis.title.x = element_text(color = "black", size = 20,  vjust = 0.5, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 28,  face = "bold", vjust = 2, angle = 90),
        
        text=element_text(family="Times"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16, face = 'bold'),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 700), breaks = seq(0,700, by = 50))+
  scale_color_manual(values =c('black'))+
  guides( color = "none")+
  annotation_custom(grid::linesGrob(y = c(0, 0), gp = grid::gpar(lwd = 2))) +
  annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 2))) +
   geom_text(data = ann_text,label = ann_text$lab, 
             aes(family = 'Times', fontface = 'bold'), size = 8)+
  scale_fill_grafify(palette = 'kelly')


ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Total_Res_Time_Jan 4 2023.tiff',
       dpi = 300, height = 12, width = 16)





####Total time plot by site per fish basis

#below used for making some labels on the panels 
Time_labs.1 <- c( "0-24 h", "24-48 h", '48-96 h')
names(Time_labs.1) <- c("24", "48", "96")

ann_text.1 <- data.frame(Fish_time = c(55,55,55),
                       Section = c("Fairhaven", "Fairhaven", "Fairhaven"),
                       lab = c( "0-24 h", "24-48 h", '48-96 h'),
                       Time = factor(c(24,48,96), levels = c(24,48,96))) %>% print()




ggplot(All_res_times.2, aes(x = Section, y = Fish_time, fill = as.factor(Section), colour="black"))+
  geom_col(alpha = 0.8, width = 0.85, position = "dodge") +
  coord_flip()+
  facet_grid(rows = vars(Time), scales = "free_y", switch = "y", space = "free_y",
             labeller = labeller(Order = Time_labs))+
  labs(fill='Section' ) +
  ylab(bquote(bold( 'Total residency time per fish '(h~fish^-1))))+
  theme_bw()+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    strip.text.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),                           
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),                           
    axis.title.y = element_blank(),                       
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )+
  
  theme(axis.text.x = element_text(color = "black", size = 16,  face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, face = "plain"),
        axis.title.x = element_text(color = "black", size = 20,  vjust = 0.5, face = 'bold'),
        axis.title.y = element_text(color = "black", size = 28,  face = "bold", vjust = 2, angle = 90),
        
        text=element_text(family="Times"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16, face = 'bold'),
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks = seq(0,60, by = 10))+
  scale_color_manual(values =c('black'))+
  guides( color = "none")+
  annotation_custom(grid::linesGrob(y = c(0, 0), gp = grid::gpar(lwd = 2))) +
  annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 2))) +
  geom_text(data = ann_text.1,label = ann_text.1$lab, 
            aes(family = 'Times', fontface = 'bold'), size = 8)+
  scale_fill_grafify(palette = 'kelly')


ggsave('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Total_Res_per fish_Jan 4 2023.tiff',
       dpi = 300, height = 12, width = 16)


####Plots as maps####


#First, need to make representive GPS coordinates for each of the sections
# did this manually of google earth and put into csv

Gen_coords<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Section_general_GPS_coords_Jan 4 2023.csv") %>% print()

#join with the aforrmentioned data


All_res_times_GPS <-All_res_times.2 %>% left_join(Gen_coords) %>% print()




#read in map data 
coastline <- readOGR("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021/MAPPING/QUODDY_REGION+MAG.shp") 
coastline.1 <- fortify(coastline) %>% as_tibble() %>% print()

#title for the legend 
legend_title <- "Total Residence Time"


#first make functions for compass and scale bar
#avialable from here: https://egallic.fr/en/scale-bar-and-north-arrow-on-a-ggplot2-map/



library(maps)
library(maptools)
library(ggplot2)
library(grid)



create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}


create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}


scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}


















#####24 h fish###

#filter out 24 h only fish
F24_map<-All_res_times_GPS %>% filter(Time == 24) %>% print()


#plot it out

A<-ggplot()+ 
  geom_polygon(fill="grey83", color="black", coastline.1, mapping = aes(long, lat, group=group)) + #first part of map making
  geom_path(colour="grey67", coastline.1, mapping = aes(long, lat, group=group)) + #second part of map making
  
  labs(x=expression(longitude), #adds these labs to the plot (removed later one, there vestigial)
       y=expression(latitude)) +
coord_map(xlim = c(-67.17, -66.6), ylim = c(44.8,45.18 ))  +
geom_point(F24_map, mapping = aes(x=Longitude, y= Latititude, #specifies the actual bubble part (i.e.)
                                  size=Total_time, color=Total_time,
                                  group = Section), alpha =0.9) +
scale_color_viridis(legend_title,label = function(x) sprintf("%.2f", x), option ='C')+  #adds in a log based colour scale using package 'viridis'
  #label = function makes it to 2 decimals
  
  theme_void() +  guides( colour = "colorbar", size = "none")+
  theme(legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8))+ #change legend text font size
  ggtitle("0-24 h")+
  theme(plot.title = element_text(hjust = 0.5, size=18, family = 'Times', face = 'bold'))+ #adjust title formatting
  geom_text(F24_map, family = 'Times', fontface = 'bold', nudge_y = .007,nudge_x = -0.01, #Add in labels on points
            
            mapping =aes(x=Longitude, y= Latititude,
                label = Short))+
  theme(text=element_text(family="Times"))#makes all text as TNR

#add in scale bar
A+ scale_bar(lon = -66.75, lat = 44.8, 
              distance_lon = 5, distance_lat = .5, distance_legend = 2, 
              dist_unit = "km")
ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Total_time_map_24.tiff",
       dpi = 300, height = 10, width = 12 )




#####48 h fish###

#filter out 48 h only fish
F48_map<-All_res_times_GPS %>% filter(Time == 48) %>% print()
#13

#plot it out

B<-ggplot()+ 
  geom_polygon(fill="grey83", color="black", coastline.1, mapping = aes(long, lat, group=group)) + 
  geom_path(colour="grey67", coastline.1, mapping = aes(long, lat, group=group)) + 
  
  labs(x=expression(longitude),
       y=expression(latitude)) +
  coord_map(xlim = c(-67.17, -66.6), ylim = c(44.8,45.18 ))  +
  geom_point(F48_map, mapping = aes(x=Longitude, y= Latititude, #specifies the actual bubble part
                                    size=Total_time, color=Total_time,
                                    group = Section), alpha =0.9) +
  scale_color_viridis(legend_title,  label = function(x) sprintf("%.2f", x), option ='C')+  #adds in a log based colour scale using package 'viridis'
  #label = function makes it to 2 decimals
  
  theme_void() +  guides( colour = "colorbar", size = "none")+
  theme(legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8))+ #change legend text font size
  ggtitle("24-48 h")+
  theme(plot.title = element_text(hjust = 0.5, size=18, family = 'Times', face = 'bold'))+ #adjust title formatting
  geom_text(F48_map, family = 'Times', fontface = 'bold', nudge_y = .007,nudge_x = -0.01, #Add in labels on points
            
            mapping =aes(x=Longitude, y= Latititude,
                         label = Short))+
  theme(text=element_text(family="Times"))#makes all text as TNR

#add in scale bar
B+ scale_bar(lon = -66.75, lat = 44.8, 
             distance_lon = 5, distance_lat = .5, distance_legend = 2, 
             dist_unit = "km")
ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Total_time_map_48.tiff",
       dpi = 300, height = 10, width = 12 )

#####96 h fish###

#filter out 96 h only fish
F96_map<-All_res_times_GPS %>% filter(Time == 96) %>% print()
#16

#plot it out

C<-ggplot()+ 
  geom_polygon(fill="grey83", color="black", coastline.1, mapping = aes(long, lat, group=group)) + 
  geom_path(colour="grey67", coastline.1, mapping = aes(long, lat, group=group)) + 
  
  labs(x=expression(longitude),
       y=expression(latitude)) +
  coord_map(xlim = c(-67.17, -66.6), ylim = c(44.8,45.18 ))  +
  geom_point(F96_map, mapping = aes(x=Longitude, y= Latititude, #specifies the actual bubble part
                                    size=Total_time, color=Total_time,
                                    group = Section), alpha =0.9) +
  scale_color_viridis(legend_title, label = function(x) sprintf("%.2f", x), option ='C')+  #adds in a log based colour scale using package 'viridis'
  #label = function makes it to 2 decimals
  
  theme_void() +  guides( colour = "colorbar", size = "none")+
  theme(legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8))+ #change legend text font size
  ggtitle("48-96 h")+
  theme(plot.title = element_text(hjust = 0.5, size=18, family = 'Times', face = 'bold'))+ #adjust title formatting
  geom_text(F96_map, family = 'Times', fontface = 'bold', nudge_y = .007,nudge_x = -0.01, #Add in labels on points
            
            mapping =aes(x=Longitude, y= Latititude,
                         label = Short))+
  theme(text=element_text(family="Times"))#makes all text as TNR font

#add in scale bar
C+ scale_bar(lon = -66.75, lat = 44.8, 
             distance_lon = 5, distance_lat = .5, distance_legend = 2, 
             dist_unit = "km")
ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Total_time_map_96.tiff",
       dpi = 300, height = 10, width = 12 )




####CRUDE DISTANCE DRAWN########
library(geosphere)

Master_res <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/data_outputs/Residency stuff/Residency complete_Dec 13 2022.csv') %>% print()

crude_lats <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Distance_Lat_long.csv') %>% print()


#change name to make joinable
crude_lats.1 <-crude_lats %>% 
  rename(First.array = Array) %>% print()





#Put in a col with a representaive start point 
crude_lats.2<-crude_lats.1 %>%mutate(Start_long = -67.0133, Start_lat = 44.9645) %>% 
print() 
  

#calcuate distance between start point and array point
#default output is metres, adding in one for km

crude_lats.3<-crude_lats.2 %>% mutate(  Distance_m=distHaversine( cbind(Long_rep, Lat_rep ), cbind(Start_long, Start_lat )  )) %>% 
  mutate(Distance_km = Distance_m/1000) %>% 
  print()

length(unique(crude_lats.3$Distance_km))
length(unique(crude_lats.3$First.array))

#ok above looks fine. Also did a couple manual distances to make sure distances
#are being computed properly. That was done in G Earth



#merge distances with the residency data:

Master_res_max_dist <-Master_res %>% left_join(crude_lats.3) %>% print()



#give max distances 
Master_res_max_dist.1<-Master_res_max_dist %>% group_by(Transmitter) %>% 
  slice(which.max(Distance_km)) %>% 
  print()
  
  
#stats output for overall trends
Master_res_max_dist.1 %>% ungroup() %>% 
  summarize(median(Distance_km),
            mean(Distance_km))


Master_res_max_dist.1 %>% ungroup() %>% 
  summarize(range(Distance_km))

?hist
hist(Master_res_max_dist.1$Distance_km)
