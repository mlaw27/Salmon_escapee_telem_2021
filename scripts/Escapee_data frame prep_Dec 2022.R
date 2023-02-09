

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




#________________________________________________________________________________________________________________
#DATA IMPORT & QAQC####
#*INDIVIDUAL VR2 RECEIVER HIT DATA####
#receivers connected and downloaded through VUE, files imported with VRL file inspector and time corrected
#export raw VR2 .csv files directly from VUE (Vemco software) and store copies ONLY to be accessed by R
#!files opened and saved in Excel beforehand will have incorrect time data!
setwd("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Raw receiver data_current June 2022 retrival")

##combine data from all receivers into one master file

receiver_file_names <- dir("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Raw receiver data_current June 2022 retrival") %>% print()
Receiverhits <- do.call(bind_rows, lapply(receiver_file_names, read.csv))

# 'data.frame':	5760808 obs. of  12 variables:
#   $ ï..Date.and.Time..UTC.: chr  "2021-05-06 20:13:43" "2021-05-06 20:13:51" "2021-05-06 20:19:47" "2021-05-06 20:20:30" ...
# $ Receiver              : chr  "VR2AR-545684" "VR2AR-545684" "VR2AR-545684" "VR2AR-545684" ...
# $ Transmitter           : chr  "A69-1601-60148" "A69-1601-60148" "A69-1601-60649" "A69-1601-61216" ...
# $ Transmitter.Name      : logi  NA NA NA NA NA NA ...
# $ Transmitter.Serial    : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Sensor.Value          : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Sensor.Unit           : chr  "" "" "" "" ...
# $ Station.Name          : chr  "LLP-I-01" "LLP-I-01" "LLP-I-01" "LLP-I-01" ...
# $ Latitude              : num  44 44 44 44 44 ...
# $ Longitude             : num  -66.9 -66.9 -66.9 -66.9 -66.9 ...
# $ Transmitter.Type      : logi  NA NA NA NA NA NA ...
# $ Sensor.Precision      : logi  NA NA NA NA NA NA ...
# > 

#the above line of code took forever to run on this PC, saving as a CSV for easier read-in 

#write_csv(Receiverhits, "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiverhits_2021 salmon escapees_Dec 9 2022.csv")





####Step 1: read in and clean up receiver hits######


Receiverhits<-read_delim("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiverhits_2021 salmon escapees_Dec 9 2022.csv", 
                         skip = 1, col_names = F) %>% print()
#5,760,808 x 12

str(Receiverhits)

#used the 'skip = 1' argument as there were some issues reading in the headers of the rows
#it eliminates the headers from the df
#col_names = F makes ure the first row doesn't become the header name, 
#puts in dummy col names instead

#putting headers back in:


#vector containing column names
Col_names<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Outputs/Reciever_col_names_Jan 31 2022.csv", col_names = F) %>% print()

#adding col names 

Receiverhits.1 <-Receiverhits %>%
rename_at(1:12, funs(paste0(Col_names$X1))) %>% print()
# 5,760,808 x 12

#rename and reorganize columns in receiver data master

Receiverhits.2<-Receiverhits.1 %>% 
         rename(Date_Time = Date_Time_UTC, Serial = Transmitter_Serial, Station = Station_Name) %>% print()

# 5,760,808 x 12

#R seems to work in UTC for all date times, going to keep everything as UTC
#to avoid issues when joining and reading in CSV's 



#extracts tag ID from complete tad ID (code space + ID)
Receiverhits.2$tag_id <- substr(Receiverhits.2$Transmitter, 10, 15)

Receiverhits.2$tag_id 


#added designation for tag sensor calibration (Y/N)
#identifies if the units been converted using their respective line equations
Receiverhits.2$Calibrated <- case_when(
  Receiverhits.2$Sensor_Unit == "ADC" ~ "N",
  Receiverhits.2$Sensor_Unit == "m" ~ "Y",
  Receiverhits.2$Sensor_Unit == "Â°C" ~ "Y",
  Receiverhits.2$Sensor_Unit == "NA" ~ "NA",
  TRUE ~as.character(NA))

Receiverhits.2
# 5,760,808 x 14

Receiverhits.2$Calibrated


#adds data source (differentiates from manual tracking data)
Receiverhits.2$Source <- ("receiver")

#cleaning up the labels for consistency using clean names
#outputs into lower case with snake case

Receiverhits.3<-Receiverhits.2 %>% clean_names() %>% print()

#adding in codespace fopr later use in the Actel package
Receiverhits.4 <- Receiverhits.3 %>% mutate(transmitter.2 = transmitter) %>% 
  separate(transmitter.2, sep = '-', c("A", "B", "C")) %>% 
  unite('CodeSpace', A:B, remove =T, sep = "-") %>% select(-C) %>% print()


#5,760,808 x 16


#### Step 2: merge receiver deployment data with hits#####


setwd("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021")

Receiverdeploy <- read_csv("RECEIVER METADATA 2021.csv") %>% print()

Receiverdeploy.1<-as_tibble(Receiverdeploy)  %>% print() #Making as a tibble for ease of use
#210 x 55

str(Receiverdeploy.1)

#personal preference to work in lower case with '_'
Receiverdeploy.2<-Receiverdeploy.1 %>% clean_names() %>% print()

str(Receiverdeploy.2)
#210 x 55

unique(Receiverdeploy.2$target)


#adding in new column of type to further delineate point from aquaculture sites

aqua_IDs <- Receiverdeploy.2 %>% filter(target == "aqua") %>% 
             mutate(type = replace(type, type == "point", "aqua"))%>% 
  print()  #42x55

Receiverdeploy.3 <-Receiverdeploy.2 %>% filter(target != "aqua") %>% print()
#168 x 55

Receiverdeploy.4<- bind_rows(Receiverdeploy.3, aqua_IDs) %>% print()
#210 x 55

unique(Receiverdeploy.4$station)
length(unique(Receiverdeploy.4$station)) #210


Receiverdeploy.5 <- Receiverdeploy.4 %>% rename(habitat = alewife_habitat) %>% 
  select(-smolt_habitat, -alewife_rec_order, -smolt_rec_order ) %>% print()
#210 x 52

write_csv(Receiverdeploy.5, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiver_metadata_refined_Dec 9 2022.csv')






#looks good visually
#NOTE had to manually curate some of the receiver section names/array info 
#use the dec 16th file as it reflects thse changes 
#There were some issues with the naming not being consistent with things
#Also did another manual/visual chekc of arrays to make sure receivers were 
#associated with correct arrays

Receiver_info <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiver_metadata_refined_Dec 16 2022_USE THIS.csv') %>% print()
#210 x 52

#replaces coordinate data provided in VUE (planned locations) during receiver initialization with actual deployment locations
Receiverhits.5 <- left_join(Receiverhits.4, select(Receiver_info, station, area, lat_deploy_dd, 
                                                   long_deploy_dd, habitat), by=c("station")) %>% print()
#5,760,808 x 20

names(Receiverhits.5)

#select desired columns from receiver master

Receiverhits.6 <- Receiverhits.5 %>% 
  select(-latitude, -longitude) %>% #getting rid of VUE coordinates
 rename(latitude = lat_deploy_dd, longitude = long_deploy_dd) %>% #renaming actual coordinates
  select(date_time, transmitter,CodeSpace, tag_id, sensor_value, calibrated, receiver, 
         station, area,source, latitude, longitude, habitat) %>% print()
#5,760,808 x 13

#writing out for quicker import later on 
write_csv(Receiverhits.6, "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Reciever_hits_compiled_Dec 9 2022.csv")


####Step 3: compile manual tracking data######

#again, download data with VR100HS and leave copies of the raw .csv file untouched
#here, multiple VR100 units were used during manual tracking
setwd("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021/MANUAL TRACKING/R MANUAL TRACKING")

##combine data from all receivers into one master file
manualtracking_file_names <- dir("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021/MANUAL TRACKING/R MANUAL TRACKING") 
Manual_tracking <- do.call(bind_rows, lapply(manualtracking_file_names, read.csv)) %>% print()

Manual_tracking.1 <-as_tibble(Manual_tracking)%>% clean_names() %>% print()
#235,022 x 22



#concatenate data and time 
Manual_tracking.1$date_time <- ymd_hms(paste(Manual_tracking.1$date, Manual_tracking.1$time), tz = "UTC")

Manual_tracking.1
#235,022 x 25

#truncate datetime data to seconds (excludes milliseconds)
Manual_tracking.1$date_time <- format(Manual_tracking.1$date_time, format='%Y-%m-%d %H:%M:%S')

#adds column with receiver ID as source of telemetry data
Manual_tracking.1["receiver"] <- "VR-100"
Manual_tracking.1 <-Manual_tracking.1 %>% add_column( habitat = 'manual' ) %>% print()


#checking what values are present in the tag sensor calibration
unique(Manual_tracking.1$units1)
#[1] ""    "ms"  " m"  " °C" NA  

#added designation for tag sensor calibration (Y/N)
#identifies if the units been converted using their respective line equations
Manual_tracking.1$calibrated <- case_when(
  Manual_tracking.1$units1== " °C" ~ "Y",
  Manual_tracking.1$units1== " m" ~ "Y",
  Manual_tracking.1$units1== "ms" ~ "NA",
  TRUE ~as.character("N"))

Manual_tracking.1
#235,022 x 26


#rename and reorganize columns in receiver data master

Manual_tracking.3 <- Manual_tracking.1 %>% 
  rename(tag_id = id, serial= s_n, sensor_value = data1, CodeSpace = code_space) %>% 
  print()
#235,022 x 26

names (Manual_tracking.3)

#add categorical variable columns
Manual_tracking.3["station"] <- "VR-100"
Manual_tracking.3["area"] <- "manual tracking"
Manual_tracking.3["source"] <- "manual tracking"

Manual_tracking.3
#235,022 x 29

#combines code space and tag ID to form complete tag ID (code space + ID)
Manual_tracking.3["transmitter"] <- paste(Manual_tracking.3$CodeSpace, Manual_tracking.3$tag_id, sep="-") %>% print()

#select desired columns from master manual tracking data
Manual_tracking.4 <- Manual_tracking.3 %>% 
  select(date_time, transmitter, CodeSpace, tag_id,
         sensor_value, calibrated, 
         receiver, station, area, 
         source,latitude, longitude, habitat) %>% 
  print()

#235,022 x 13
              

#removes incomplete cases where tag ID data, or lat/long, is not recorded (i.e., NAs)

Manual_tracking.5 <- Manual_tracking.4 %>% drop_na(tag_id, latitude, longitude) %>% print()
#64,900 x 13



#removes detections before active tracking was undertaken for target species
Manual_tracking.6 <- subset(Manual_tracking.5, date_time > as.POSIXct('2021-09-21 13:00:00', tz="UTC")) %>% print()

#7,173 x 13


#removes detections that may have be duplicated between multiple VR100 downloads or from monitoring multiple channels on an individual VR100 unit
Manual_tracking.7 <- summarize(group_by(Manual_tracking.6, date_time, CodeSpace, transmitter, tag_id,
                                        sensor_value, calibrated, 
                                        receiver, station, area, 
                                        source,latitude, longitude,habitat)) %>% print()
#1930 x 13                                        
                                        


#Writing out the manual data for ease later on 

write_csv(Manual_tracking.7, "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022//Manual_tracking_compiled_Dec 9 2022.csv")



###Step 4: Read in and organize tagging data #####


#TAGGED FISH DATA
setwd("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021")

Fish_data <- read.csv("ESCAPEE TAG DATA 2021.csv", na.strings=c("", "NA")) %>% print()

Fish_data.1 <-as_tibble(Fish_data)%>% clean_names() %>% 
  filter (species == "Salmo salar" ) %>% 
  select(-x) %>% print()

#198 x 15 

Fish_data.1$tag_id <- as.character(Fish_data.1$tag_id)

str(Fish_data.1)

Fish_data.1 <-Fish_data.1 %>% mutate(date_released=release_date) %>% print() 


#concatenate date and time for release


Fish_data.1$release_date_time <- mdy_hms(paste(Fish_data.1$release_date, Fish_data.1$release_time), tz = "Canada/Atlantic")
Fish_data.1$release_date_time



#setting time zone as UTC to make it comparable to everything else
Fish_data.1$release_date_time <-with_tz(Fish_data.1$release_date_time, tz = "UTC")

Fish_data.1$release_date_time



Fish_data.1$weight_g <-as.integer(Fish_data.1$weight_g)
Fish_data.1$weight_g

#data check for fish metadata
options(pillar.sigfig=6)


Fish_data.3<-Fish_data.1 %>% group_by(serial) %>% slice(n=1)%>%
  ungroup(serial) %>%  print()

Fishstats_all <- summarize(Fish_data.3, 
                        count=length(total_length_mm),
                        length_mean=mean(total_length_mm, na.rm=T), 
                       length_sd =sd(total_length_mm, na.rm=T), 
                        length_min =min(total_length_mm, na.rm=T),
                        length_max=max(total_length_mm, na.rm=T),
                        weight_mean=mean(weight_g, na.rm=T), 
                        weight_sd=sd(weight_g, na.rm=T), 
                        weight_min =min(weight_g, na.rm=T),
                        weight_max=max(weight_g, na.rm=T))






Fishstats_all

# # A tibble: 1 x 9
# count l     ength_mean length_sd length_min length_max weight_mean weight_sd weight_min weight_max
# <int>       <dbl>     <dbl>      <int>      <int>       <dbl>     <dbl>      <int>      <int>
#   1    99     370.909   24.7164        310        435     601.770   118.495        321        849


write_csv(Fishstats_all, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/weight_length_info_Dec 9 2022 2022.csv')
write_csv(Fish_data.1, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Fish_tagging_info_Dec 9 2022.csv')




Fish_data.1 %>% filter(sensor == 'temp') %>% group_by(release_date, group) %>% summarise(n = n())


#####Step 5: generate master files#####

#read in necessary files:


Fish_data.2<-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Fish_tagging_info_Dec 9 2022.csv') %>% print()
#198 x 17

Receiver_hits_data<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Reciever_hits_compiled_Dec 9 2022.csv") %>% print()
#5,760,808 x 13

Manual_tracking_data<-read_csv("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Manual_tracking_compiled_Dec 9 2022.csv") %>% print()
#1,930 x 13

Receiverdeploy_data<-read_csv( 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Receiver_metadata_refined_Dec 16 2022_USE THIS.csv') %>% print()
#210 x 52





#combine data from receiver and manual tracking a master telemetry file
Telem_master <- bind_rows(Receiver_hits_data, Manual_tracking_data) %>% print()
#5,762,738 x 13

#matches each separately as it was 5651470 and 1930 rows, sum is 5535580

names(Fish_data.2)



#First, get the fish data with its full tag ID
Fish_data.3<-Fish_data.2 %>% add_column(CodeSpace.2 = 'A69-9007-')%>% 
  add_column(CodeSpace = 'A69-9007')%>% 
  unite("transmitter", c('CodeSpace.2', 'tag_id'), remove = FALSE, sep = "") %>%
  select(-CodeSpace.2)%>% print()

Fish_data.3$CodeSpace

#198 x 19

Fish_data.3$transmitter
 write_csv(Fish_data.3, 'C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/Fish_tagging_info_TAG_ID_ADJUST_Dec 9 2022.csv')

#Next, filter the data file for only fish that were in the escapee list
#Using the full transmitter id to do so

Telemetry_master_escapee <-Telem_master %>% 
  filter(transmitter %in% Fish_data.3$transmitter) %>% print()

#307,114 x 13

length(unique(Telemetry_master_escapee$transmitter))

#198 unique fish ID's 
#matches the fish data length

#Next join with fish data

Telemetry_master_escapee.1 <-Telemetry_master_escapee %>% 
  left_join(Fish_data.3) %>% 
  select( -surgeon , -lice_count, -comments, -release_date, -release_time) %>% 
  print()
#307,114 x 24

#calibrate sensor data using temperature and depth line equations (slope and intercept)

Rec_temp <- subset(Telemetry_master_escapee.1, sensor=="temp") %>% print()
#153,096 x 24

sort(unique(Rec_temp$tag_id))



Rec_temp["sensor_value"] <- (0.1575 * Rec_temp["sensor_value"]) + -5.157
#153,096 x 24

Rec_temp %>% filter(calibrated == 'N')
#no 'yes' fish 

range(Rec_temp$sensor_value)
#1.6155 35.0055
#looks fine here, reasonable values


Rec_depth <- subset(Telemetry_master_escapee.1, sensor=="depth") %>% print()
#154,018 x 24

Rec_depth["sensor_value"] <- (0.3009 * Rec_depth["sensor_value"]) + -1.2035
Rec_depth$sensor_value[Rec_depth$sensor_value < 0] <- 0
Rec_depth$sensor_value[Rec_depth$sensor_value > 100] <- 0


Rec_depth
Rec_depth$sensor_value


Telemetry_master_escapee.2 <- rbind(Rec_temp, Rec_depth) %>% print()
rm("Rec_temp", "Rec_depth")


#307,114 x 24

view(Telemetry_master_escapee.2)


Telemetry_master_escapee.3 <- Telemetry_master_escapee.2 %>% 
  mutate(released = ifelse(date_time > release_date_time, "released", NA_character_)) %>% print()

#307,114 x 25

unique(Telemetry_master_escapee.3$released)
#"released" NA   


Telemetry_master_escapee.4 <- subset(Telemetry_master_escapee.3, released=="released") %>% print()
#303,570 x 25

released <- Telemetry_master_escapee.3 %>% filter(is.na(released)) %>% print() 
#3,544 x 25

#the above plus telem.3 match up to og df. Means 3544 NA's removed 


#organize telemetry data sequentially by tag and time
Telemetry_master_escapee.5 <- arrange(Telemetry_master_escapee.4, tag_id, date_time) %>% print()
#303,570 x 25

Telemetry_master_escapee.5$order <- sequence(rle(Telemetry_master_escapee.5$tag_id)$lengths) 

names(Telemetry_master_escapee.5)

#join with some additonal reciever info including farm sites

Telemetry_master_escapee.6 <-Telemetry_master_escapee.5 %>% 
  left_join(Receiverdeploy_data %>% select(station, area, habitat, type, target, bma, farm_id, farm_name,ret_datetime_utc)) %>% 
   print()
#303,570 x 32

Telemetry_master_escapee.6$ret_datetime_utc

Telemetry_master_escapee.6$ret_datetime_utc <- ymd_hms(Telemetry_master_escapee.6$ret_datetime_utc, tz = 'UTC')

Telemetry_master_escapee.6$ret_datetime_utc


unique(Telemetry_master_escapee.6$ret_datetime_utc)

#write file
write_csv(Telemetry_master_escapee.6, 
            "C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/Prepped_raw_data_files_Dec 8 2022/PB_telem_master_escapee_2021_Dec 8 2022.csv")





######Receiver array plot######



library(maps)
library(maptools)
library(ggplot2)
library(grid)


#load in shapefile

#read in map data 
coastline <- readOGR("R:/Science/CESD/BES_MarcTrudel/Passamaquoddy telemetry/Raw data/2021/MAPPING/QUODDY_REGION+MAG.shp") 
coastline.1 <- fortify(coastline) %>% as_tibble() %>% print()




#first make functions for compass and scale bar
#avialable from here: https://egallic.fr/en/scale-bar-and-north-arrow-on-a-ggplot2-map/



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



#Read in receiver positions

Receive_loc <-read_csv('C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/Actel analyses/Dec 2022_work with this/Master_files_Dec 2022/spatial.csv') %>% print()

Receive_loc

Receive_loc.1 <- Receive_loc %>% filter(Type == 'Release') %>% print()


###Plot it out 


A<-ggplot()+ 
  geom_polygon(fill="grey83", color="black", coastline.1, mapping = aes(long, lat, group=group)) + #first part of map making
  geom_path(colour="grey67", coastline.1, mapping = aes(long, lat, group=group)) + #second part of map making
  
  labs(x=expression(Longitude), #adds these labs to the plot (removed later one, there vestigial)
       y=expression(Latitude))+
  coord_map(xlim = c(-67.3, -66.5), ylim = c(44.5,45.25 ))  +
  geom_point(Receive_loc, mapping = aes(x=Longitude, y= Latitude, #specifies the actual bubble part (i.e.)
                                    color=Type), alpha =0.9) +
  theme_void() +  
  theme(legend.position = 'none')+ #change legend text font size
  theme(text=element_text(family="Times"))+#makes all text as TNR
scale_colour_manual(values = c('black','red'))+
  scale_shape_manual(values = c(20, 17))+
  geom_point(Receive_loc.1, mapping = aes(x=Longitude, y= Latitude #specifies the actual bubble part (i.e.)
                                        ), colour = 'red', shape =17, size = 3) 
#add in scale bar
A+ scale_bar(lon = -67.2, lat = 44.6, 
             distance_lon = 5, distance_lat = .5, distance_legend = 2, 
             dist_unit = "km")
ggsave("C:/Users/LAWRENCEM/OneDrive - DFO-MPO/Documents/Telelmetry stuff/2021 escapee_data/CSAS_ready_files/Receiver_array.tiff",
       dpi = 300, height = 10, width = 7.75 )



  