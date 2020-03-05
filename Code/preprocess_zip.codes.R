##MDML Final Project
##Shannon Kay, Jaejin Kim, & Jessica Spencer
##December 13, 2019
##Preprocess IRS Individual Income Tax Zip Code Data
##Create a measure of neighborhood value

#1. Load required packages
require(tidyverse)

#2. Read in Illinois Tax Return Files
illinois_2015 <- read_csv("../2015_illinois.csv")
illinois_2016 <- read_csv("../2016_illinois.csv")
illinois_2017 <- read_csv("../2017_illinois.csv")

#3. 2015- select relevant columns
illinois_2015 <- illinois_2015 %>% 
  select(ILLINOIS,X2,X3,X17, X18, X19) %>% 
  mutate(Year = 2015)

#4. Fix column names & delete top 5 rows (oddly formatted, don't need)
colnames(illinois_2015) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")

illinois_2015 <- illinois_2015[-(1:5),]

#5. 2016- select relevant columns
illinois_2016 <- illinois_2016 %>% 
  select(ILLINOIS,X2,X3,X17, X18, X19) %>% 
  mutate(Year = 2016)

#6. Fix column names & delete top 5 rows (oddly formatted, don't need)
colnames(illinois_2016) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")

illinois_2016 <- illinois_2016[-(1:5),]

#7. 2017- select relevant columns 
illinois_2017 <- illinois_2017 %>% 
  select(ILLINOIS,X2,X3,X19, X20, X21) %>% 
  mutate(Year = 2017)

#8. Fix column names & delete top 5 rows (oddly formatted, don't need)
colnames(illinois_2017) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")

illinois_2017 <- illinois_2017[-(1:5),]

#9. Read in NY Tax Return Files
ny_2015 <- read_csv("../2015_ny.csv")
ny_2016 <- read_csv("../2016_ny.csv")
ny_2017 <- read_csv("../2017_ny.csv")

#10. 2015- select relevant columns and fix column names, drop top 5 rows
ny_2015 <- ny_2015 %>% 
  select(`NEW YORK`,X2,X3,X17, X18, X19) %>% 
  mutate(Year = 2015)

ny_2015 <- ny_2015[-(1:5),]

colnames(ny_2015) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")

#11. 2016- select relevant columns and fix column names, drop top 5 rows
ny_2016 <- ny_2016 %>% 
  select(`NEW YORK`, X2, X3, X17, X18, X19) %>% 
  mutate(Year = 2016)

ny_2016 <- ny_2016[-(1:5),]

colnames(ny_2016) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")

#12. 2017- select relevant columns and fix column names, drop top 5 rows
ny_2017 <- ny_2017 %>% 
  select(`NEW YORK`, X2, X3, X19, X20, X21) %>% 
  mutate(Year = 2017)

ny_2017 <- ny_2017[-(1:5),]

colnames(ny_2017) <- c("Zip_code", "Size of Adjusted Gross Income", "Number of Returns", "Adjusted Gross Income", "Total Income- Number of Returns", "Total Income- Amount", "Year")


#13. Combine zipcode data 2015-2017
zip_code_demographics <- rbind(illinois_2015, illinois_2016, illinois_2017, ny_2015, ny_2016, ny_2017)

#14. Summarise the first row of each zip code (totals, not income brackets)
zip_code_demographics <- zip_code_demographics %>%
  group_by(Zip_code, Year) %>% 
  summarise(Num_Returns = first(`Number of Returns`), 
            AGI = first(`Adjusted Gross Income`), 
            Num_Returns_TotalIncome = first(`Total Income- Number of Returns`), 
            Total_Income = first(`Total Income- Amount`))

#15. Convert columns from character to numeric 
zip_code_demographics <- zip_code_demographics %>% 
  mutate(Num_Returns = as.numeric(gsub(",", "", Num_Returns)),
         AGI = as.numeric(gsub(",", "", AGI)), 
         Num_Returns_TotalIncome = as.numeric(gsub(",", "", Num_Returns_TotalIncome)), 
         Total_Income = as.numeric(gsub(",", "", Total_Income))) %>% 
  filter(!is.na(AGI))

#16. Chicago zip code to neighborhood 
far_north_side = c(60625, 60626, 60630, 60631, 60640, 60645, 60646, 60656, 60659, 60660, 60666)
northwest_side = c(60634, 60639, 60641, 60707)
north_side = c(60613, 60614, 60618, 60647, 60657)
west_side = c(60607, 60608, 60612, 60622, 60623, 60626, 60644, 60651, 60661)
central = c(60601, 60602, 60603, 60604, 60605, 60606, 60610, 60611, 60654)
southwest_side = c(60609, 60621, 60629, 60632, 60636, 60638)
south_side = c(60615, 60616, 60637, 60649, 60653)
far_southwest_side = c(60620, 60643, 60652, 60655)
far_southeast_side = c(60617, 60619, 60627, 60828, 60633)

#17. NYC zip code to neighborhood
##Used https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm
##neighborhood vectors
##bronx
central_bronx <- c(10453,10457,10460)
bp_fordham <- c(10458,10467,10468)
high_bridge <- c(10451,10452,10456)
hunts_point <- c(10454,10455,10459,10474)
kingsbridge <- c(10463,10471)
northeast_bronx <- c(10466,10469,10470,10475)
southeast_bronx <- c(10461,10462,10464,10465,10472,10473)
##brooklyn
central_brooklyn <- c(11212,11213,11216,11233,11238)
southwest_brooklyn <- c(11209,11214,11228)
borough_park <- c(11204,11218,11219,11230)
canarsie <- c(11234,11236,11239)
southern_brooklyn <- c(11223,11224,11229,11235)
northwest_brooklyn <- c(11201,11205,11215,11217,11231,11242,11241)
flatbush <- c(11203,11210,11225,11226)
new_lots <- c(11207,11208)
greenpoint <- c(11211,11222)
sunset_park <- c(11220,11232)
bushwick <- c(11206,11221,11237,11249)
##manhattan
central_harlem <- c(10026,10027,10030,10037,10039)
chelsea <- c(10001,10011,10018,10019,10020,10036,10121)
east_harlem <- c(10029,10035)
gramercy <- c(10010,10016,10017,10022,10168,10118,10178,10158)
soho <- c(10012,10013,10014)
midtown <- c(10174,10167,10103,10112,10111,10169,10119,10174,10167,10106,10179,10165,10154,10123,10176,10177,10105,10172,10173,10055,10171,10170,10107)
lower_manhattan <- c(10004,10005,10006,10007,10038,10280,10041,10282,10281,10279,10080,10048,10271,10285)
lower_eastside <- c(10002,10003,10009)
upper_eastside <- c(10021,10028,10044,10065,10075,10128,10153)
upper_westside <- c(10023,10024,10025,10115,10069)
inwood <- c(10031,10032,10033,10034,10040)
##queens
northeast_queens <- c(11361,11362,11363,11364)
north_queens <- c(11354,11355,11356,11357,11358,11359,11360)
central_queens <- c(11365,11366,11367)
jamaica <- c(11412,11423,11432,11433,11434,11435,11436, 11430)
northwest_queens <- c(11101,11102,11103,11104,11105,11106)
westcentral_queens <- c(11374,11375,11379,11385)
rockaways <- c(11691,11692,11693,11694,11695,11697)
southeast_queens <- c(11004,11005,11411,11413,11422,11426,11427,11428,11429)
southwest_queens <- c(11414,11415,11416,11417,11418,11419,11420,11421)
west_queens <- c(11368,11369,11370,11372,11373,11377,11378)
long_island_city <- 11109
##staten island
port_richmond <- c(10302,10303,10310)
south_shore <- c(10306,10307,10308,10309,10312)
stapleton <- c(10301,10304,10305)
mid_island <- 10314
central_park <- 10000

all_zips <- c(far_north_side, northwest_side, north_side, west_side,central, southwest_side, south_side, far_southwest_side, far_southeast_side, central_bronx, bp_fordham, high_bridge, hunts_point, kingsbridge, northeast_bronx, southeast_bronx, central_brooklyn, southwest_brooklyn, borough_park, canarsie, southern_brooklyn, northwest_brooklyn, flatbush, new_lots, greenpoint, sunset_park, bushwick, central_harlem, chelsea, east_harlem, gramercy, soho, midtown, lower_manhattan, lower_eastside, upper_eastside, upper_westside, inwood, northeast_queens, north_queens, central_queens, jamaica, northwest_queens, westcentral_queens, rockaways, southeast_queens, southwest_queens, west_queens, long_island_city, port_richmond, south_shore, stapleton, mid_island, central_park)

zip_code_demographics <- zip_code_demographics %>% 
  filter(Zip_code %in% all_zips)

# Add neighborhood column to Chicago data
zip_code_demographics <- zip_code_demographics %>% 
  mutate(Neighborhood = case_when(Zip_code %in% far_north_side ~ 'far_north_side',
                                  Zip_code %in% northwest_side ~ 'northwest_side',
                                  Zip_code %in% north_side ~ 'north_side',
                                  Zip_code %in% west_side ~ 'west_side',
                                  Zip_code %in% central ~ 'central',
                                  Zip_code %in% southwest_side ~ 'southwest_side',
                                  Zip_code %in% south_side ~ 'south_side',
                                  Zip_code %in% far_southwest_side ~ 'far_southwest_side',
                                  Zip_code %in% far_southeast_side ~ 'far_southeast_side', 
                                  Zip_code %in% central_bronx ~ "Central Bronx",
                                  Zip_code %in% bp_fordham ~ "Bronx Park & Fordham", 
                                  Zip_code %in% high_bridge  ~ "High Bridge & Morrisania", 
                                  Zip_code %in% hunts_point  ~ "Hunts Point and Mott Haven", 
                                  Zip_code %in% kingsbridge  ~ "Kingsbridge and Riverdale", 
                                  Zip_code %in% northeast_bronx ~ "Northeast Bronx", 
                                  Zip_code %in% southeast_bronx ~ "Southeast Bronx", 
                                  Zip_code %in% central_brooklyn ~ "Central Brooklyn", 
                                  Zip_code %in% southwest_brooklyn  ~ "Southwest Brooklyn", 
                                  Zip_code %in% borough_park ~ "Borough Park", 
                                  Zip_code %in% canarsie ~ "Canarsie and Flatlands", 
                                  Zip_code %in% southern_brooklyn  ~ "Southern Brooklyn", 
                                  Zip_code %in% northwest_brooklyn ~ "Northwest Brooklyn", 
                                  Zip_code %in% flatbush ~ "Flatbush", 
                                  Zip_code %in% new_lots ~ "East New York & New Lots", 
                                  Zip_code %in% greenpoint  ~ "Greenpoint", 
                                  Zip_code %in% sunset_park ~ "Sunset Park", 
                                  Zip_code %in% bushwick ~ "Bushwick and Williamsburg", 
                                  Zip_code %in% central_harlem ~ "Central Harlem", 
                                  Zip_code %in% chelsea ~ "Chelsea & Clinton", 
                                  Zip_code %in% east_harlem  ~ "East Harlem", 
                                  Zip_code %in% gramercy ~ "Gramercy Park and Murray Hill", 
                                  Zip_code %in% soho ~ "Greenwich Village and Soho",
                                  Zip_code %in% midtown ~ "Midtown",
                                  Zip_code %in% lower_manhattan ~ "Lower Manhattan", 
                                  Zip_code %in% lower_eastside ~ "Lower East Side", 
                                  Zip_code %in% upper_eastside ~ "Upper East Side", 
                                  Zip_code %in% upper_westside ~ "Upper West Side", 
                                  Zip_code %in% inwood ~ "Inwood and Washington Heights", 
                                  Zip_code %in% northeast_queens ~ "Northeast Queens", 
                                  Zip_code %in% north_queens ~ "North Queens", 
                                  Zip_code %in% central_queens ~ "Central Queens", 
                                  Zip_code %in% jamaica ~ "Jamaica", 
                                  Zip_code %in% northwest_queens ~ "Northwest Queens", 
                                  Zip_code %in% westcentral_queens ~ "West Central Queens", 
                                  Zip_code %in% rockaways ~ "Rockaways", 
                                  Zip_code %in% southeast_queens ~ "Southeast Queens", 
                                  Zip_code %in% southwest_queens ~ "Southwest Queens", 
                                  Zip_code %in% west_queens ~ "West Queens",
                                  Zip_code %in% long_island_city ~ "Long Island City",
                                  Zip_code %in% port_richmond ~ "Port Richmond", 
                                  Zip_code %in% south_shore ~ "South Shore", 
                                  Zip_code %in% stapleton ~ "Stapleton and St. George", 
                                  Zip_code %in% mid_island ~ "Mid-Island", 
                                  Zip_code %in% central_park ~ "Central Park"))

#18. average AGI & total income by neighborhood
zip_code_demographics <- zip_code_demographics %>% 
  group_by(Neighborhood) %>% 
  group_by(Neighborhood, Year) %>% 
  mutate(avg_AGI = AGI/Num_Returns, 
         avg_Total_Income = Total_Income/Num_Returns_TotalIncome) %>% 
  select(Zip_code, Year, Neighborhood, avg_AGI, avg_Total_Income)

#19. save data
write_csv(zip_code_demographics, path = "data/IRS_demographics_by_zipcode.csv")