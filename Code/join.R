##MDML Final Project
##Shannon Kay, Jaejin Kim, & Jessica Spencer
##December 13, 2019
##Joining preprocessed data to create final dataset

##Load required packages and dataset
library(tidyverse)
nyc <- read_csv("data/pre-processed_nyc_final.csv")
chicago <- read_csv("data/pre-processed_chicago_final.csv")
IRS_data <- read_csv("data/IRS_demographics_by_zipcode.csv")


chicago$City <- "CHICAGO"

#2. Join cities with rbind
restaurant_data <- rbind(nyc, chicago)

#3. Join Neighborhood & average adjusted gross income to restaurant data by zip code & year
restaurant_data <- inner_join(restaurant_data, IRS_data, by = c("Zip_code", "Year"))

#4. Transform categorical variables to factors
restaurant_data <- restaurant_data %>%
  drop_na() %>% 
  arrange(avg_AGI) %>% 
  mutate(Neighborhood = factor(Neighborhood, levels = unique(Neighborhood)),
         City = factor(City, levels = c("CHICAGO", "NYC")),
         Year = factor(Year, levels = c(2015, 2016, 2017)),
         Month = factor(Month, levels = c(1:12)),
         Weekday = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday", "Saturday",
                                              "Sunday")),
         critical_flag = factor(critical_flag, levels = c(0,1)))

#5. write final data to csv
write_csv(restaurant_data, path = "data/final_restaurant_data.csv")
