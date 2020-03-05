##MDML Final Project
##Shannon Kay, Jaejin Kim, & Jessica Spencer
##December 13, 2019
##Preprocess NYC Restaurant Data

#1. Load required packages and dataset
library(tidyverse)
nyc <- read_csv("../DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

#2. transform 'inspection date' to date and year, filter the data to relevant timeframe and drop unnecessary columns
nyc <- nyc %>% 
  mutate(Date = lubridate::mdy(`INSPECTION DATE`), 
         Year = lubridate::year(Date)) %>% 
  filter(Year %in% c(2015, 2016, 2017)) %>% 
  select(-Latitude, 
         -Longitude, 
         -`Community Board`, 
         -`Council District`, 
         -`Census Tract`, 
         -BIN, 
         -BBL, 
         -NTA, 
         -BORO, 
         -BUILDING, 
         -STREET, 
         -`CUISINE DESCRIPTION`, 
         -`RECORD DATE`, 
         -PHONE, 
         -`INSPECTION DATE`, 
         -`GRADE DATE`, 
         -GRADE) %>% 
  rename(Restaurant_ID = CAMIS,
         Name = DBA,
         Action=ACTION,
         code=("VIOLATION CODE"),
         critical_flag=("CRITICAL FLAG"),
         score=SCORE,
         Inspection_Type=("INSPECTION TYPE"), 
         Zip_code = ZIPCODE)

#3. Recode values in 'Action' to be more easily understandable
nyc <- nyc %>%
  mutate(Action = case_when(Action=="Establishment Closed by DOHMH. Violations were cited in the following area(s) and those requiring immediate action were addressed." ~ "closed",
                            Action=="Establishment re-closed by DOHMH" ~ "re-closed",
                            Action=="Establishment re-opened by DOHMH" ~ "re-opened",
                            Action=="No violations were recorded at the time of this inspection." ~ "no violations",
                            Action=="Violations were cited in the following area(s)." ~ "violations"),
         Inspection_Type = case_when(Inspection_Type=="Cycle Inspection / Compliance Inspection" ~ "cycle-compliance",
                                     Inspection_Type=="Cycle Inspection / Initial Inspection" ~ "cycle-initial",
                                     Inspection_Type=="Cycle Inspection / Re-inspection" ~ "cycle-re-inspection",
                                     Inspection_Type=="Cycle Inspection / Reopening Inspection" ~ "cycle-reopening",
                                     Inspection_Type=="Cycle Inspection / Second Compliance Inspection" ~ "cycle-second-compliance",
                                     Inspection_Type=="Pre-permit (Non-operational) / Compliance Inspection" ~ "pre-permit-nonop-compliance",
                                     Inspection_Type=="Pre-permit (Non-operational) / Initial Inspection" ~ "pre-permit-nonop-initial",
                                     Inspection_Type=="Pre-permit (Non-operational) / Re-inspection" ~ "pre-permit-nonop-re-inspection",
                                     Inspection_Type=="Pre-permit (Non-operational) / Second Compliance Inspection" ~ "pre-permit-nonop-second-compliance",
                                     Inspection_Type=="Pre-permit (Operational) / Compliance Inspection" ~ "pre-permit-op-compliance",
                                     Inspection_Type=="Pre-permit (Operational) / Initial Inspection" ~ "pre-permit-op-initial",
                                     Inspection_Type=="Pre-permit (Operational) / Re-inspection" ~ "pre-permit-op-re-inspection",
                                     Inspection_Type=="Pre-permit (Operational) / Reopening Inspection" ~ "pre-permit-op-reopening",
                                     Inspection_Type=="pre-permit-op-second-compliance" ~ "Pre-permit (Operational) / Second Compliance
Inspection",
                                     TRUE ~ Inspection_Type)) 

#4. Reduce to inspection types that seem comparable to Chicago
comparable_inspections = c("cycle-compliance", 
                           "cycle-initial", 
                           "cycle-re-inspection", 
                           "cycle-second-compliance")

#5. Filter the data to only include non-nas, score above 0 and relevant inspection types
nyc = nyc %>% 
  filter(!is.na(score), 
         score >= 0,
         Inspection_Type %in% comparable_inspections)

#6. Add Number_Violations, Month and Weekday column
nyc <- nyc %>% 
  group_by(Restaurant_ID, Date) %>%
  mutate(score = max(score), 
         Number_Violations = n(), 
         Month = lubridate::month(Date),
         Weekday = weekdays(Date)) %>% 
  ungroup() %>% 
  distinct()

#7. Create outcome variable
#1 is fail
nyc$outcome <- ifelse(nyc$score >= 28, 1, 0)

#8. Create Critical flag variable
nyc <- nyc %>% 
  mutate(critical_flag = case_when(critical_flag == "Y"~1,
                                               critical_flag == "N" ~0), 
         City = "NYC")

nyc$critical_flag <- ifelse(is.na(nyc$critical_flag), 0, nyc$critical_flag)

#9. Standardize & take unique so that inspections collapse to one row per inspection
nyc <- nyc %>% 
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
         outcome) %>% 
  unique()

#10. write final data to csv
write_csv(nyc, path = "data/pre-processed_nyc_final.csv")
