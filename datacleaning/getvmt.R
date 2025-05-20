### Phase 1 Analysis for TBI VMT

# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/datacleaning")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", "janitor", "tools", "spatstat")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
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

# Drop kids and people who can't drive
person_all <- person_all %>%
  filter((age != "Under 5") & (age != "5 to 15") & (can_drive != "No, does not drive"))

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
#View(linkedtrip_combined[, c(120, 409)]) ## Drop 120 since 409 is more full.
#View(linkedtrip_combined[, c(4, 180)]) ## Drop 180 since 4 is more full
sum(linkedtrip_combined[, 4] == linkedtrip_combined[, 180], na.rm = T) ## Drop either since both columns are equivalent.

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

## Remove trips that are too long or too short
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(vmt >= 0.1 & vmt <= 100)

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Keep people with 7 days worth of travel data
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>% group_by(person_id) %>% 
  filter(n_distinct(day_num) == 7)

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Filter out people who are really young
print(paste0('Trips before young people/cannot drive exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter((age != "Under 5") & (age != "5 to 15"))

print(paste0('Trips after young people/cannot drive exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Filter weird overnight trips
print(paste0('Trips before overnight exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

overnight_exclusions <- c("Went to temporary lodging (e.g., hotel, vacation rental)", 
                          "Spent the night at non-home location out of region")


linkedtrip_combined <- linkedtrip_combined %>%
  filter((!o_purpose %in% overnight_exclusions) & 
           (!d_purpose %in% overnight_exclusions)) 

print(paste0('Trips after overnight exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Filter out vacation
print(paste0('Trips before vacation exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(d_purpose != "Vacation/traveling")

print(paste0('Trips after vacation exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))


# Write out data to CSV ---------------------------------------------------
linkedtrip_combined <- linkedtrip_combined %>%
  select(survey_year, person_id, hh_id, day_id, age, can_drive, day_num, linked_trip_id, travel_dow, depart_time, arrive_time, 
         vmt, linked_trip_weight, person_weight, hh_weight, income_broad, o_purpose, d_purpose, d_purpose_category, depart_hour, arrive_hour)

vmt_df <- linkedtrip_combined

rm(linkedtrip_combined)

write.csv(vmt_df, "../inputs/linkedtrip_vmt.csv", row.names = F)

