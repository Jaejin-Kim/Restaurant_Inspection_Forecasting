##MDML Final Project
##Shannon Kay, Jaejin Kim, & Jessica Spencer
##December 13, 2019
##Preprocess Chicago Restaurant Data 

##Load required packages and data
library(tidyverse)
chicago <- read_csv("../Food_Inspections.csv")

#1. Drop unnecessary columns and rename variables
chicago <- chicago %>% 
  select(-`DBA Name`, 
         -Latitude, 
         -Longitude, 
         -Location, 
         -Risk, 
         -Address, 
         -State) %>% 
  rename(Name = `AKA Name`, 
         Restaurant_ID = `License #`, 
         Inspection_Type = `Inspection Type`,
         Inspection_ID = `Inspection ID`, 
         Zip_code = Zip)

#2. Drop rows with 'results' that are not relevant, filter 'facility type' and 'inspection type' to only have relevant types, create Date, Year, Month and Weekday, and filter to relevant years
chicago <- chicago %>% 
  filter(!Results %in% c("Out of Business", "Not Ready"))%>% 
  mutate(Facility_Type = tolower(`Facility Type`), 
         Inspection_Type = tolower(Inspection_Type))%>% 
  select(-`Facility Type`) %>% 
  filter(Facility_Type %in% c("bakery", "cafe", "restaurant", "tavern", "deli", "ice cream", "paleteria"), 
         Inspection_Type %in% c("canvass", "complaint", "license", "suspected food poisoning")) %>% 
  mutate(Date = lubridate::mdy(`Inspection Date`),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         Weekday = weekdays(Date)) %>% 
  filter(Year %in% c(2015, 2016, 2017)) %>% 
  select(-`Inspection Date`)


#3. calculate # of violations per inspection for each restaurant
##grouping by Inspection ID because restaurants with the same name may have multiple locations
##Also documenting the presence of violations #1-14, which are identified as critical violations in the Chicago data dictionary

chicago <- chicago %>% 
  separate_rows(Violations, sep = "\\|") %>% 
  group_by(Inspection_ID) %>% 
  mutate(Number_Violations = n(),
         violation_num = as.numeric(substr(Violations,1,3)), 
         flag = ifelse(violation_num < 15, 1, 0), 
         critical_flag = sum(flag)) %>% 
  select(Inspection_ID,
         Name, 
         Restaurant_ID, 
         City, 
         Zip_code, 
         Date, 
         Year,
         Month,
         Weekday,
         Inspection_Type,
         Results,
         Facility_Type, 
         Number_Violations, 
         critical_flag) %>% 
  unique()

#4. Change critical flag from the sum of critical violations to an indicator variable
chicago <- chicago %>%
  group_by(Inspection_ID) %>%
  mutate(critical_flag = ifelse(critical_flag > 0, 1,0)) %>% 
  ungroup()

#5. NA's didn't get changed to 0's to changed them separately 
chicago$critical_flag <- ifelse(is.na(chicago$critical_flag), 0, chicago$critical_flag)

#6. Create outcome variable- fail is 1
chicago$outcome <- ifelse(chicago$Results %in% "Pass", 0, 1)

#7. Standardize dataset
chicago <- chicago %>% 
  select(Restaurant_ID, 
         Name, 
         City, 
         Zip_code, 
         Date, 
         Year, 
         Month, 
         Weekday, 
         Inspection_Type, 
         Number_Violations, 
         critical_flag,
         outcome)
   
#8. write final data to csv
write_csv(chicago, path = "data/pre-processed_chicago_final.csv")