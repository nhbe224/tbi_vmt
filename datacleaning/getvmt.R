### Phase 1 Analysis for TBI VMT

# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/datacleaning")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(data.table)
library(spatstat)
options(scipen = 999)


# Load in Data ------------------------------------------------------------
## Person Data ------------------------------------------------------------
person_2019 <- fread("../inputs/wave1_v2/travelBehaviorInventory2019Person.csv")
person_2021 <- fread("../inputs/wave2_v2/travelBehaviorInventory2021Person.csv")
person_2023 <- fread("../inputs/wave3_v2/travelBehaviorInventory2023Person.csv")

## Are column names equal?
print(paste0(sum(colnames(person_2019) != colnames(person_2021)), " column names are different."))
print(paste0(sum(colnames(person_2023) != colnames(person_2021)), " column names are different."))

# Since no column names are different, stack them up.
person_all <- rbind(person_2019, person_2021, person_2023)
rm(person_2019, person_2021, person_2023)

## Trip Data --------------------------------------------------------------
linkedtrip_2019 <- fread("../inputs/wave1_v2/travelBehaviorInventory2019LinkedTrip.csv")
linkedtrip_2021 <- fread("../inputs/wave2_v2/travelBehaviorInventory2021LinkedTrip.csv")
linkedtrip_2023 <- fread("../inputs/wave3_v2/travelBehaviorInventory2023LinkedTrip.csv")

## Are column names equal?
print(paste0(sum(colnames(linkedtrip_2019) != colnames(linkedtrip_2021)), " column names are different."))
print(paste0(sum(colnames(linkedtrip_2023) != colnames(linkedtrip_2021)), " column names are different."))

## Convert depart_time_reported column to POSIXct so rbind can happen.
linkedtrip_2021$depart_time_reported <- as.POSIXct(linkedtrip_2021$depart_time_reported)
linkedtrip_2023$depart_time_reported <- as.POSIXct(linkedtrip_2023$depart_time_reported)

## Stack dataframes
linkedtrip_all <- rbind(linkedtrip_2019, linkedtrip_2021, linkedtrip_2023)
rm(linkedtrip_2019, linkedtrip_2021, linkedtrip_2023)


## Household Data ----------------------------------------------------------
household_2019 <- fread("../inputs/wave1_v2/travelBehaviorInventory2019HH.csv")
household_2021 <- fread("../inputs/wave2_v2/travelBehaviorInventory2021HH.csv")
household_2023 <- fread("../inputs/wave3_v2/travelBehaviorInventory2023HH.csv")

## Are column names equal?
print(paste0(sum(colnames(household_2019) != colnames(household_2021)), " column names are different."))
print(paste0(sum(colnames(household_2023) != colnames(household_2021)), " column names are different."))

## Stack dataframes
household_all <- rbind(household_2019, household_2021, household_2023)
rm(household_2019, household_2021, household_2023)

## Day Data ----------------------------------------------------------------
day_2019 <- fread("../inputs/wave1_v2/travelBehaviorInventory2019Day.csv")
day_2021 <- fread("../inputs/wave2_v2/travelBehaviorInventory2021Day.csv")
day_2023 <- fread("../inputs/wave3_v2/travelBehaviorInventory2023Day.csv")

## Are column names equal?
print(paste0(sum(colnames(day_2019) != colnames(day_2021)), " column names are different."))
print(paste0(sum(colnames(day_2023) != colnames(day_2021)), " column names are different."))

## Stack dataframes
day_all <- rbind(day_2019, day_2021, day_2023)
rm(day_2019, day_2021, day_2023)


# Combine Data ------------------------------------------------------------
## This part is adapted from VMT Mode Shift, Clean and Decode TBI from Dr. Greg Erhardt
linkedtrip_combined <- left_join(linkedtrip_all, day_all, by=c('survey_year', 'hh_id', 'person_id', 'day_num', 'day_id')) %>%
  left_join(., person_all, by=c('survey_year', 'hh_id', 'person_id')) %>%
  left_join(., household_all, by =c('survey_year','hh_id'))

## Drop duplicates
linkedtrip_combined <- linkedtrip_combined %>% select(-contains(".y"))

## Rename columns that contain .x
names(linkedtrip_combined) <- gsub("\\.x", "", names(linkedtrip_combined))

## Check for more duplicates
which(duplicated(names(linkedtrip_combined)))
grep("person_num", colnames(linkedtrip_combined))
grep("participation_group", colnames(linkedtrip_combined))
View(linkedtrip_combined[, c(120, 409)]) ## Drop 120 since 409 is more full.
View(linkedtrip_combined[, c(4, 180)])
sum(linkedtrip_combined[, 4] == linkedtrip_combined[, 180]) ## Drop either since both columns are equivalent.

## Drop based on above.
linkedtrip_combined <- linkedtrip_combined %>%
  select(-120, -180)

## Keep relevant columns
linkedtrip_combined <- linkedtrip_combined %>%
  select(-contains(c("commute_subsidy", "ev_charge", "hh_member", "num_complete", "race", "school", "share", "transportation_barriers", "prev_res", "micromobility", "num_days_complete")))

sort(colnames(linkedtrip_combined))


# Clean Data --------------------------------------------------------------
## Calculate VMT ----------------------------------------------------------
## This part is adapted from VMT Mode Shift, Clean and Decode TBI from Dr. Greg Erhardt
linkedtrip_combined$vehicle_trip_flag <- ifelse(linkedtrip_combined$mode_type %in% c("Household Vehicle", "Other Vehicle", "Smartphone ridehailing service"), 1, 0)
linkedtrip_combined$vehicle_trips = linkedtrip_combined$vehicle_trip_flag / linkedtrip_combined$num_travelers_int 

# Calculate VMT
linkedtrip_combined$vmt = linkedtrip_combined$vehicle_trips * linkedtrip_combined$distance_miles

## Exclusions -------------------------------------------------------------
## First, exclude trips with missing block groups and timestamp
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(if_all(c(trip_o_cbg_2010, trip_d_cbg_2010, depart_time, arrive_time), negate(is.na)))

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Then, filter out trips where home location is not within the study counties.
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

study_area_counties = c("Anoka", "Carver", "Chisago", "Dakota", "Goodhue", "Hennepin", 
                       "Isanti", "Le Sueur", "McLeod", "Pierce", "Polk", "Ramsey", 
                       "Rice", "Scott",  "Sherburne", "Sibley", "St Croix", "Washington", 
                       "Wright")

### Removes County, MN and County, WI
linkedtrip_combined$home_county <- gsub('.{11}$', '', linkedtrip_combined$home_county)
linkedtrip_combined$trip_o_county <- gsub('.{3}$', '', linkedtrip_combined$trip_o_county)
linkedtrip_combined$trip_d_county <- gsub('.{3}$', '', linkedtrip_combined$trip_d_county)


linkedtrip_combined <- linkedtrip_combined %>%
  filter(home_county %in% study_area_counties,
         trip_o_county %in% study_area_counties,
         trip_d_county %in% study_area_counties)

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Then, remove incosistent modes.
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(!mode_type %in% c("Long distance passenger mode", "Missing", "Other"))

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Finally, remove trips that are too long or too short
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(vmt >= 0.1 & vmt <= 100)

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))


# Write out data to CSV ---------------------------------------------------
linkedtrip_combined <- linkedtrip_combined %>%
  select(survey_year, person_id, hh_id, day_id, day_num, linked_trip_id, travel_dow, depart_time, arrive_time, vmt)

vmt_df <- linkedtrip_combined
rm(linkedtrip_combined)

write.csv(vmt_df, "../inputs/linkedtrip_vmt.csv", row.names = F)

