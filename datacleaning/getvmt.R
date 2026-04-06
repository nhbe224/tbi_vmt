### Data Cleaning for TBI VMT
### Inputs: Raw TBI Survey Data
### Outputs: VMT weekly, VMT daily, VMT for displacement (future work)

# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/datacleaning")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable",
                     "stringi", "nnet", "plm", "forcats", "modelsummary", 
                     "foreign", "stargazer", "osrm")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)

# Helper Function ---------------------------------------------------------
fill_nearest = function(x){
  keys=which(!is.na(x))
  if(length(keys)==0) return(NA)
  b = map_dbl(seq.int(x), ~keys[which.min(abs(.x-keys))])
  x[b]
}

# Globals -----------------------------------------------------------------
## This sets the threshold for remote work
twork_limit_daily <- 1

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
print(intersect(colnames(linkedtrip_all), colnames(day_all)))
print(intersect(colnames(person_all), colnames(linkedtrip_all)))
print(intersect(colnames(person_all), colnames(day_all)))
print(intersect(colnames(household_all), colnames(day_all)))
print(intersect(colnames(household_all), colnames(linkedtrip_all)))
print(intersect(colnames(household_all), colnames(person_all)))

linkedtrip_combined <- left_join(linkedtrip_all, day_all, by=c('survey_year', 'hh_id', 'person_id', 'day_num', 'day_id', 'person_num')) %>%
  left_join(., person_all, by=c('survey_year', 'hh_id', 'person_id', 'person_num', 'participation_group')) %>%
  left_join(., household_all, by =c('survey_year','hh_id', 'participation_group', 'home_park_other', 'num_days_complete'))

unique(linkedtrip_combined$survey_year)

## Drop duplicates
ncol(linkedtrip_combined)
linkedtrip_combined <- linkedtrip_combined %>% select(-contains(".y"))
ncol(linkedtrip_combined)
which(duplicated(names(linkedtrip_combined)))

## Keep relevant columns
linkedtrip_combined <- linkedtrip_combined %>%
  select(-contains(c("commute_subsidy", "ev_charge", "hh_member", "num_complete", "race", "school", "share", "transportation_barriers", "prev_res", "micromobility", "num_days_complete")))

sort(colnames(linkedtrip_combined))
unique(linkedtrip_combined$survey_year)

# Clean Data --------------------------------------------------------------
## Calculate VMT ----------------------------------------------------------
## This part is adapted from VMT Mode Shift, Clean and Decode TBI from Dr. Greg Erhardt
linkedtrip_combined$vehicle_trip_flag <- ifelse(linkedtrip_combined$mode_type %in% c("Household Vehicle", "Other Vehicle", "Smartphone ridehailing service"), 1, 0)
linkedtrip_combined$vehicle_trips = linkedtrip_combined$vehicle_trip_flag / linkedtrip_combined$num_travelers_int 

# Calculate VMT
linkedtrip_combined$vmt = linkedtrip_combined$vehicle_trips * linkedtrip_combined$distance_miles
unique(linkedtrip_combined$survey_year)

## Exclusions -------------------------------------------------------------
## First, exclude trips with missing block groups and timestamp
print(paste0('Trips before exclusions: ', nrow(linkedtrip_combined), ", VMT before exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

linkedtrip_combined <- linkedtrip_combined %>%
  filter(if_all(c(trip_o_cbg_2010, trip_d_cbg_2010, depart_time, arrive_time), negate(is.na)))

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))
unique(linkedtrip_combined$survey_year)

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
  filter(trip_o_county %in% study_area_counties,
         trip_d_county %in% study_area_counties)

print(paste0('Trips after exclusions: ', nrow(linkedtrip_combined), ", VMT after exclusion: ", round(sum(linkedtrip_combined$vmt, na.rm = T))))

## Then, remove inconsistent modes.
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


# Keep relevant columns ---------------------------------------------------
linkedtrip_combined <- linkedtrip_combined %>%
  select(survey_year, person_id, hh_id, day_id, age, can_drive, day_num, linked_trip_id, travel_dow, depart_time, arrive_time, 
         vmt, linked_trip_weight, person_weight, hh_weight, income_broad, o_purpose, o_purpose_category, d_purpose, d_purpose_category, depart_hour, arrive_hour)

vmt_df <- linkedtrip_combined

# Get a dataset of work arrangement  -------------------------------------
## Filter for those with 7 days of travel data and in rMove ------------------
trip_complete <- linkedtrip_all %>%
  group_by(person_id) %>% 
  filter(n_distinct(day_num) == 7) %>% ## Filters for people with 7 days
  left_join(person_all) %>%
  arrange(person_id) %>% 
  select(hh_id, person_id, survey_year, linked_trip_id, o_purpose, d_purpose, mode_type, participate, day_num, depart_dow, participation_group) %>%
  filter(participation_group %in% c("rMove recruit, rMove diary", "Online or call center recruit, rMove diary") |
           participate == "Yes") ## People with rMove Diary

## Join with person_id to get their job type ---------------------------------
trip_step1 <- trip_complete %>%
  left_join(person_all) %>%
  filter(job_type %in% c("Go to one work location ONLY (outside of home)", "Work location regularly varies (different offices/jobsites)", "Work ONLY from home or remotely (telework, self-employed)", "Telework some days and travel to a work location some days"),
         employment %in% c("Employed full-time" , "Employed part-time", "Self-employed"),
         student %in% c("Not a student")) # No students

telework_days <- day_all %>%
  select(person_id, day_num, telework_time, survey_year) %>%
  replace(is.na(.), 0)

### Fix telework time to be in hours for 2021 and 2023
telework_days$telework_time <- ifelse(telework_days$survey_year != 2019, telework_days$telework_time / 60, telework_days$telework_time)

trip_step2 <- trip_step1 %>%
  inner_join(telework_days, c("person_id", "day_num", "survey_year"))


## For those with complete travel surveys, flag if they have a work trip on a given day.
trip_step3 <- trip_step2 %>%
  select(person_id, survey_year, day_num, o_purpose, d_purpose, telework_time, employment, job_type) %>%
  group_by(person_id, day_num) %>%
  mutate(work_trip_made = +(any(d_purpose %in% c("Primary workplace", "Went to work-related activity (e.g., meeting, delivery, worksite)", "Other work-related")))) %>%
  ungroup %>%
  select(person_id, survey_year, day_num, telework_time, work_trip_made, employment, job_type) %>%
  distinct()


## Convert day number to numeric
trip_step3$day_num <- as.numeric(str_remove(trip_step3$day_num, "Day "))

trip_step3 <- trip_step3 %>%
  arrange(survey_year, person_id, day_num)

work_arr_daily <- trip_step3 %>%
  arrange(survey_year, person_id, day_num) %>%
  mutate(work_arr_day = case_when(telework_time <= twork_limit_daily & work_trip_made == 1 ~ "in_person_day",
                                  telework_time > twork_limit_daily & work_trip_made == 1  ~ "hybrid_day",
                                  telework_time > twork_limit_daily & work_trip_made == 0  ~ "remote_day",
                                  telework_time <= twork_limit_daily & work_trip_made == 0 ~ "no_work_day")) %>% ## Part 1
  select(person_id, survey_year, work_arr_day, employment, job_type, day_num, telework_time)

work_arr <- work_arr_daily %>%
  group_by(person_id, survey_year, work_arr_day, employment, job_type) %>%
  dplyr::summarize(count = n()) %>%
  pivot_wider(names_from = work_arr_day, values_from = count) %>% 
  replace(is.na(.), 0) %>% ## Part 2
  filter(no_work_day != 7) %>%
  mutate(total_work_days = 7 - no_work_day,
         in_person_day_pct = in_person_day / total_work_days,
         remote_day_pct = remote_day / total_work_days,
         hybrid_day_pct = hybrid_day / total_work_days,
         work_arr = case_when((in_person_day + hybrid_day >= 5) | (in_person_day_pct + hybrid_day_pct == 1) ~ "aip",
                              remote_day_pct  == 1 ~ "ar",
                              TRUE ~ "hybrid")) %>%
  select(person_id, survey_year, total_work_days, remote_day, remote_day_pct, work_arr, employment, job_type) %>%
  mutate(remote_day_factor = case_when(remote_day == 0 ~ "0",
                                       remote_day == 1 ~ "1",
                                       remote_day == 2 ~ "2",
                                       remote_day == 3 ~ "3",
                                       remote_day == 4 ~ "4",
                                       remote_day >= 5 ~ "5+"),
         remote_day_pct_factor = case_when(remote_day_pct < 0.2 ~ "0% to <20%",
                                           remote_day_pct >= 0.2 & remote_day_pct < 0.4 ~ "20% to <40%",
                                           remote_day_pct >= 0.4 & remote_day_pct < 0.6 ~ "40% to <60%",
                                           remote_day_pct >= 0.6 & remote_day_pct < 0.8 ~ "60% to <80%",
                                           remote_day_pct >= 0.8 ~ "80%+")) %>%
  ungroup()

## Join with telework hours per week to get a sense of who's reporting a lot of telework.
telework_hours_per_week <- day_all %>%
  select(person_id, telework_time, survey_year) %>%
  replace(is.na(.), 0)

## Convert 2021 and 2023 to hours.
telework_hours_per_week$telework_time <- ifelse(telework_hours_per_week$survey_year != 2019, 
                                                telework_hours_per_week$telework_time / 60, telework_hours_per_week$telework_time)

telework_hours_per_week <- telework_hours_per_week %>%
  group_by(person_id, survey_year) %>%
  dplyr::summarize(telework_hours = sum(telework_time, na.rm = T))

work_arr <- work_arr %>%
  left_join(telework_hours_per_week)

## Work arrangement cleaning
print(paste0(nrow(work_arr), ' people before exclusions.'))

## Filter out people who are not always in-person AND have less than 40 hours of telework.
work_arr <- work_arr %>%
  filter(work_arr != "aip" | telework_hours <= 40)

print(paste0(nrow(work_arr), ' people after exclusions.'))

## Clean Data Frame
work_arr <- work_arr %>% mutate(work_arr = case_when(work_arr == "aip" ~ "Always In-Person",
                                                     work_arr == "hybrid" ~ "Hybrid",
                                                     work_arr == "ar" ~ "Always Remote"))

work_arr$work_arr <- factor(work_arr$work_arr, c("Always In-Person", "Hybrid", "Always Remote"))

## Person Weight Reference Table -------------------------------------------
person_weights <- person_all %>%
  select(person_id, person_weight)

vmt_by_work_arr <- vmt_df %>%
  inner_join(work_arr, c("person_id", "survey_year"))

## Get commute VMT --------------------------------------------------------
commute_vmt <- vmt_by_work_arr %>%
  arrange(person_id, day_num, depart_time) %>%
  relocate(person_id, day_num, o_purpose_category, d_purpose_category)

commute_vmt$day_num <- as.numeric(str_remove(commute_vmt$day_num, "Day "))

## Tour ID. Start with 1 if it's a new person or new day.
commute_vmt <- commute_vmt %>%
  mutate(tour_id_count = if_else((person_id != lag(person_id)) | (day_num != lag(day_num)), 1, if_else((o_purpose_category %in% c('Home', 'Overnight')), 1, 0), missing = 1)) %>%
  relocate(person_id, day_num, tour_id_count)

commute_vmt <- commute_vmt %>%
  group_by(person_id, day_num) %>%
  mutate(tour_id = cumsum(tour_id_count)) %>%
  relocate(person_id, day_num, tour_id) %>%
  select(-tour_id_count)

## A work trip if the origin or destination is work/work-related.
commute_vmt$work_trip = ifelse(commute_vmt$o_purpose_category %in% c("Work", "Work related") | commute_vmt$d_purpose_category %in% c("Work", "Work related"), 1, 0)
commute_vmt <- commute_vmt %>%
  relocate(person_id, day_num, tour_id, work_trip)

## See if this is a work tour
commute_vmt <- commute_vmt %>%
  group_by(person_id, day_num, tour_id) %>%
  mutate(work_tour = max(work_trip)) %>%
  relocate(person_id, day_num, tour_id, work_trip, work_tour)

commute_vmt$work_tour_vmt <- commute_vmt$work_tour * commute_vmt$vmt
commute_vmt$nonwork_tour_vmt <- ifelse(commute_vmt$work_tour == 0, commute_vmt$vmt, 0)

commute_vmt_daily <- commute_vmt %>%
  select(person_id, day_num, work_tour, work_tour_vmt, nonwork_tour_vmt, person_weight, survey_year, travel_dow) %>%
  group_by(person_id, day_num, travel_dow, person_weight, survey_year) %>%
  dplyr::summarize(work_tour_vmt_daily = sum(work_tour_vmt),
            nonwork_tour_vmt_daily = sum(nonwork_tour_vmt)) 

commute_vmt_daily$person_id <- as.numeric(commute_vmt_daily$person_id)
commute_vmt_daily$survey_year <- factor(commute_vmt_daily$survey_year)

commute_vmt_weekly <- commute_vmt %>%
  select(person_id, survey_year, work_tour, work_tour_vmt, person_weight) %>%
  group_by(person_id, survey_year, person_weight) %>%
  dplyr::summarize(work_tour_vmt_weekly = sum(work_tour_vmt))

## Join Commute VMT weekly -------------------------------------------------
vmt_weekly <- vmt_by_work_arr %>%
  group_by(person_id, work_arr, person_weight, survey_year, total_work_days, remote_day, remote_day_pct, remote_day_factor, remote_day_pct_factor) %>%
  dplyr::summarize(vmt_weekly = sum(vmt, na.rm = T)) %>%
  rename(num_remote_days = remote_day)

vmt_weekly <- vmt_weekly %>%
  left_join(commute_vmt_weekly)

## Examine overnight people
overnight <- linkedtrip_all %>%
  filter((o_purpose %in% overnight_exclusions) |
           (d_purpose %in% overnight_exclusions)) %>%
  select(person_id, arrive_dow, o_purpose, d_purpose, o_purpose_category, d_purpose_category) %>%
  arrange(person_id) 

overnighters <- unique(overnight$person_id)

## Which overnighters are in VMT Weekly? Filter them out?
overnight_ids <- overnighters[overnighters %in% vmt_weekly$person_id]
overnight_ids

overnight <- overnight %>%
  filter(person_id %in% overnight_ids) %>%
  group_by(person_id) %>%
  mutate(freq = n()) %>%
  ungroup()

vmt_weekly <- vmt_weekly %>%
  filter(!person_id %in% overnight_ids)

# VMT weekly dataframe (for OLS) -------------------------------------
## Bring in smart location database data
sld2018 <- fread("D:/neeco/rdc_census/sld_change_file/outputs/sld_change_file2018.csv")

## Read in jobs within 30 minute transit for 2021
mn_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Minnesota_27_transit_block_group_2021.csv")
wi_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Wisconsin_55_transit_block_group_2021.csv")
transit2021_load <- rbind(mn_transit2021_load, wi_transit2021_load)
transit2021 <- transit2021_load %>%
  filter(threshold == 1800) %>%
  select(geoid, weighted_average) %>%
  rename(home_bg_2010 = geoid, transit_jobs30 = weighted_average)
transitAll <- read.dta("D:/neeco/rdc_census/to_rdc/transit_ALL.dta")
transitAll <- transitAll %>%
  select(-threshold) %>%
  filter(year %in% c(2019, 2021, 2022))
## Proxy 2023 using 2022
transitAll$year <- ifelse(transitAll$year == 2022, 2023, transitAll$year)

## Read gas prices
mn_gas_prices_load <- read.csv("../inputs/mn_gas_prices.csv")

## Remove person weight
vmt_weekly <- vmt_weekly %>%
  select(-person_weight)

## Join on person variables that are relevant to VMT -----------------------
### First find the variables in person that are relevant to VMT
person_keep <- person_all %>%
  select(survey_year, person_id, person_weight, hh_id, age, gender, employment, education,  race, ethnicity, work_cbg_2010, job_type_pre_covid)
person_keep$person_id <- as.numeric(person_keep$person_id)

### Then join on VMT
vmt_weekly$person_id <- as.numeric(vmt_weekly$person_id)
vmt_weekly_w_person <- vmt_weekly %>%
  left_join(person_keep) 

## Look at non-response/missing columns
unique(vmt_weekly_w_person$age)
unique(vmt_weekly_w_person$race) 
unique(vmt_weekly_w_person$gender)
unique(vmt_weekly_w_person$employment) 
unique(vmt_weekly_w_person$education)

## Plot distributions before grouping
ggplot(vmt_weekly_w_person, aes(x=age)) + geom_bar() + ggtitle("Distribution of Age Groups") +
  xlab("Age") + ylab("Count") + geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_person, aes(x=gender)) + geom_bar() + ggtitle("Distribution of Genders") +
  xlab("Gender") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_person, aes(x=employment)) + geom_bar() + ggtitle("Distribution of Employment") +
  xlab("Employment") + ylab("Count") + geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_person, aes(x=race)) + geom_bar() + ggtitle("Distribution of Racial Groups") +
  xlab("Race") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_person, aes(x=ethnicity)) + geom_bar() + ggtitle("Distribution of Ethnicities") +
  xlab("Ethnicity") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_person, aes(x=education)) + geom_bar() + ggtitle("Distribution of Education") +
  xlab("Education") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

## Age group
vmt_weekly_w_person <- vmt_weekly_w_person %>% 
  rename(age_group = age) 

vmt_weekly_w_person <- vmt_weekly_w_person %>%
  mutate(age_group = case_when(age_group %in% c("18 to 24", "25 to 34") ~ "18 to 34",
                               age_group %in% c("35 to 44", "45 to 54")  ~ "35 to 54",
                               age_group %in% c("55 to 64", "65 to 74", "75 or older") ~ "55 or older"))

vmt_weekly_w_person$age_group <- factor(vmt_weekly_w_person$age_group, 
                                        levels = c("18 to 34", "35 to 54", "55 or older"))

## Race
vmt_weekly_w_person$race <- ifelse(vmt_weekly_w_person$race == "White" & vmt_weekly_w_person$ethnicity == "Not Hispanic or Latino", "White", "nonWhite")
vmt_weekly_w_person$race <- factor(vmt_weekly_w_person$race, 
                                   levels = unique(vmt_weekly_w_person$race))
vmt_weekly_w_person <- vmt_weekly_w_person %>%
  select(-ethnicity)

ggplot(vmt_weekly_w_person, aes(x=race)) + geom_bar() + ggtitle("Distribution of Racial Groups (Accounting for Ethnicity)") +
  xlab("Race") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

## Gender
unique(vmt_weekly_w_person$gender)
vmt_weekly_w_person$gender <- ifelse(vmt_weekly_w_person$gender == "Male", "Male", "nonMale")
vmt_weekly_w_person$gender <- factor(vmt_weekly_w_person$gender,
                                     levels = unique(unique(vmt_weekly_w_person$gender)))

## Employment type
vmt_weekly_w_person$employment <- factor(vmt_weekly_w_person$employment,
                                         levels = unique(unique(vmt_weekly_w_person$employment)))

## Education
vmt_weekly_w_person$education <- ifelse(vmt_weekly_w_person$education %in% c("Bachelor's degree", "Graduate/post-graduate degree"), "bach_plus", "no_bach_plus")
vmt_weekly_w_person$education <- factor(vmt_weekly_w_person$education)
vmt_weekly_w_person$education <- relevel(vmt_weekly_w_person$education, ref = "bach_plus")


## Join on household variables that are relevant to VMT -----------------------
hh_keep <- household_all %>%
  select(survey_year, hh_id, home_bg_2010, income_detailed, num_kids, num_adults,
         num_workers, num_vehicles, num_workers) 

hh_keep$num_workers <- as.numeric(gsub("[^0-9.]", "", hh_keep$num_workers))
hh_keep$vehicles_numeric <- as.numeric(gsub("[^0-9.]", "", hh_keep$num_vehicles))


vmt_weekly_w_hh <- vmt_weekly_w_person %>%
  left_join(hh_keep)

### Clean household data
unique(vmt_weekly_w_hh$num_kids)
unique(vmt_weekly_w_hh$num_workers)
unique(vmt_weekly_w_hh$num_vehicles)
unique(vmt_weekly_w_hh$income_detailed) 

ggplot(vmt_weekly_w_hh, aes(x=num_kids)) + geom_bar() + ggtitle("Distribution of Kids") +
  xlab("Number of Kids") + ylab("Count") + geom_text(stat='count', aes(label=..count..), vjust=-0.2)

ggplot(vmt_weekly_w_hh, aes(x=num_vehicles)) + geom_bar() + ggtitle("Distribution of Vehicles") +
  xlab("Number of Vehicles") + ylab("Count") + geom_text(stat='count', aes(label=..count..), vjust=-0.2)

#### Income
vmt_weekly_w_hh$income_detailed <- factor(vmt_weekly_w_hh$income_detailed, 
                                          levels = c( "<$15K", "$15-25K",  "$25-35K", "$35-50K", "$50-75K",   
                                                      "$75-100K", "$100-150K","$150-200K", "$200-$250K", "$250K+", "Undisclosed"))

ggplot(vmt_weekly_w_hh, aes(x=income_detailed)) + geom_bar() + ggtitle("Distribution of Household Income") +
  xlab("Income Level") + ylab("Count") + theme(axis.text.x=element_text(size=6, angle = 20)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.2)

## Make numeric variables numeric
vmt_weekly_w_hh$num_kids <- as.numeric(substr(vmt_weekly_w_hh$num_kids, 1, 2))
vmt_weekly_w_hh$num_adults <- as.numeric(substr(vmt_weekly_w_hh$num_adults, 1, 2))
vmt_weekly_w_hh$num_vehicles <- as.numeric(substr(vmt_weekly_w_hh$num_vehicles, 1, 2))

## Create variable for number of additional teleworkers in household
vmt_weekly_w_hh$hybrid_or_remote <- ifelse(vmt_weekly_w_hh$work_arr %in% c("Always Remote", "Hybrid"), 1, 0)
vmt_weekly_w_hh <- vmt_weekly_w_hh %>%
  group_by(hh_id) %>% mutate(num_hybrid_or_remote = case_when(work_arr %in% c("Always In-Person") ~ sum(hybrid_or_remote),
                                                              work_arr %in% c("Always Remote", "Hybrid") ~ sum(hybrid_or_remote) - 1)) %>%
  select(-hybrid_or_remote)

## Multiple people in one household
more_than_one_hh <- vmt_weekly_w_hh %>% group_by(hh_id) %>% filter(n()>1) %>%
  select(person_id, hh_id, survey_year, work_arr, num_hybrid_or_remote)

## Group variables
vmt_weekly_w_hh <- vmt_weekly_w_hh %>%
  mutate(num_kids = case_when(num_kids == 0 ~ "0 kids",
                              num_kids > 0 ~ "1+ kid"),
         num_vehicles = case_when(num_vehicles == 0 ~ "0 vehicles",
                                  num_vehicles == 1 ~ "1 vehicle",
                                  num_vehicles >= 2 ~ "2+ vehicles"),
         income_detailed = case_when(income_detailed %in% c("<$15K", "$15-25K", "$25-35K", "$35-50K") ~ "<$50K",
                                     income_detailed %in% c("$50-75K", "$75-100K") ~ "$50-$100K",
                                     income_detailed %in% c("$100-150K") ~ "$100-$150K",
                                     income_detailed %in% c("$150-200K") ~ "$150-$200K",
                                     income_detailed %in% c("$200-$250K", "$250K+") ~ "$200K+",
                                     income_detailed %in% c("Undisclosed") ~ "Undisclosed"))

## Create auto sufficency variable
vmt_weekly_w_hh <- vmt_weekly_w_hh %>%
  mutate(auto_sufficiency = case_when(
    vehicles_numeric == 0 ~ 0,
    vehicles_numeric < num_workers ~ 1,
    .default = 2
  ))

vmt_weekly_w_hh$auto_sufficiency <- as.factor(vmt_weekly_w_hh$auto_sufficiency)
vmt_weekly_w_hh$auto_sufficiency <- relevel(vmt_weekly_w_hh$auto_sufficiency, ref = 2)


## Join on Built Environment Variables -------------------------------------
sld2018 <- sld2018 %>%
  select(bg2010, jobs_per_hh, emp8_ent, res_den, intersection_den)

vmt_weekly_w_be <- vmt_weekly_w_hh %>%
  left_join(sld2018, by = c("home_bg_2010" = "bg2010"))

vmt_weekly_w_be$home_bg_2010 <- as.character(vmt_weekly_w_be$home_bg_2010)

# vmt_weekly_w_be <- vmt_weekly_w_be %>%
#   left_join(transit2021)

# # Get the home BG where transit jobs is NA, fill with previous years' values
# na_home_bg <- vmt_weekly_w_be[is.na(vmt_weekly_w_be$transit_jobs30), ]$home_bg_2010
# # # See if it's in the dataset
# vmt_weekly_w_be %>%
#   filter(home_bg_2010 %in% na_home_bg) %>%
#   select(transit_jobs30, survey_year, home_bg_2010)

# wi_transit <- wi_transit2021_load %>%
#   filter(geoid %in% c(550939604002, 550939607002),
#          threshold == 1800) %>%
#   select(geoid, weighted_average) %>%
#   mutate(geoid = as.character(geoid))
# 
# vmt_weekly_w_be <- vmt_weekly_w_be %>%
#   left_join(wi_transit, by = c("home_bg_2010" = "geoid"))
# 
# vmt_weekly_w_be$transit_jobs30 <- ifelse(is.na(vmt_weekly_w_be$transit_jobs30),
#                                          vmt_weekly_w_be$weighted_average, vmt_weekly_w_be$transit_jobs30)

# vmt_weekly_w_be <- vmt_weekly_w_be %>%
#   select(-weighted_average)

## Join on Economic Variables ----------------------------------------------
## Gas prices
mn_gas_prices_load$year <- rownames(mn_gas_prices_load)
mn_gas_prices <- mn_gas_prices_load[-c(1:4), ]
colnames(mn_gas_prices) <- c("gas_price", "year")
mn_gas_prices$year <- as.numeric(mn_gas_prices$year)
mn_gas_prices$gas_price <- as.numeric(mn_gas_prices$gas_price)

vmt_weekly_w_econ <- vmt_weekly_w_be %>%
  left_join(mn_gas_prices, by = c("survey_year" = "year"))

### Rename
vmt_weekly_for_model <- vmt_weekly_w_econ

### Convert to survey year to factor
vmt_weekly_for_model$year2021 <- ifelse(vmt_weekly_for_model$survey_year == 2021, 1, 0)
vmt_weekly_for_model$year2023 <- ifelse(vmt_weekly_for_model$survey_year == 2023, 1, 0)


## Get Commute VMT  -------------------------------------------
vmt_weekly_for_model$nonwork_tour_vmt_weekly <- vmt_weekly_for_model$vmt_weekly - vmt_weekly_for_model$work_tour_vmt_weekly
vmt_weekly_for_model <- vmt_weekly_for_model %>%
  relocate(person_id, work_arr, survey_year, vmt_weekly, work_tour_vmt_weekly, nonwork_tour_vmt_weekly)

## Relevel -----------------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 54")
vmt_weekly_for_model$income_detailed <- factor(vmt_weekly_for_model$income_detailed, 
                                               c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-$100K")

vmt_weekly_for_model$work_cbg_2010 <- as.character(vmt_weekly_for_model$work_cbg_2010)

## Get distance between home and work --------------------------------------
## Get OD pairs for always in-person and hybrid
# od_pairs <- vmt_weekly_for_model %>%
#   filter(work_arr %in% c("Always In-Person", "Hybrid")) %>%
#   select(person_id, home_bg_2010, work_cbg_2010) %>%
#   drop_na()
# 
# od_pairs <- od_pairs[, -1]
# 
# ## Get centroids of CBGs
# mn_bgs <- block_groups(state = "MN", year = 2010)
# mn_bgs_centroids <- st_centroid(mn_bgs) %>%
#   select(GEOID10, geometry) %>%
#   rename(bg2010 = GEOID10, geom = geometry)
# 
# 
# wi_bgs <- block_groups(state = "WI", year = 2010)
# wi_bgs_centroids <- st_centroid(wi_bgs) %>%
#   select(GEOID10, geometry) %>%
#   rename(bg2010 = GEOID10, geom = geometry)
# 
# bgs_centroids <- rbind(mn_bgs_centroids, wi_bgs_centroids)
# 
# od_pairs <- od_pairs %>%
#   left_join(bgs_centroids, c("home_bg_2010" = "bg2010")) 
# 
# colnames(od_pairs)[4] <- "home_geom"
# 
# od_pairs <- od_pairs %>%
#   left_join(bgs_centroids, c("work_cbg_2010" = "bg2010")) 
# 
# colnames(od_pairs)[5] <- "work_geom"
# 
# origins <- as.data.frame(st_coordinates(od_pairs$home_geom))
# destinations <- as.data.frame(st_coordinates(od_pairs$work_geom))
# 
# # Get travel time (and distance)
# travel_times_list <- list()
# for(i in 1:nrow(origins)) {
#   src_point <- c(origins$X[i], origins$Y[i])
#   dst_point <- c(destinations$X[i], destinations$Y[i])
#   
#   # Call osrmRoute for each pair
#   route <- osrmRoute(
#     src = src_point, 
#     dst = dst_point, 
#     overview = "simplified" # Get simplified geometry
#   )
#   
#   # Store the result in the list, potentially adding an identifier
#   route$person_id <- od_pairs$person_id[i]
#   travel_times_list[[i]] <- route
# }
# 
# # Combine all routes into a single sf data frame
# travel_times_df <- do.call(rbind, travel_times_list)
# travel_times_df <- travel_times_df %>%
#   select(person_id, duration, distance) %>%
#   rename(miles_to_work = distance,
#          minutes_to_work = duration)
#   
# row.names(travel_times_df) <- NULL
# travel_times_df <- travel_times_df %>%
#   st_drop_geometry()

## Log VMT ----------------------------------------------------------------
vmt_weekly_for_model$ln_vmt_weekly <- log(vmt_weekly_for_model$vmt_weekly + 1)
vmt_weekly_for_model$ln_work_tour_vmt_weekly <- log(vmt_weekly_for_model$work_tour_vmt_weekly + 1)
vmt_weekly_for_model$ln_nonwork_tour_vmt_weekly <- log(vmt_weekly_for_model$nonwork_tour_vmt_weekly + 1)


# Log BE variables --------------------------------------------------------
vmt_weekly_for_model$ln_jobs_per_hh <- log(vmt_weekly_for_model$jobs_per_hh + 1)
vmt_weekly_for_model$ln_intersection_den <- log(vmt_weekly_for_model$intersection_den)
vmt_weekly_for_model$ln_res_den <- log(vmt_weekly_for_model$res_den + 1)


# Add post-covid variable -------------------------------------------------
vmt_weekly_for_model$post_covid = ifelse(vmt_weekly_for_model$survey_year %in% c(2021, 2023), 1, 0)



## Relevel variables -------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 54")
vmt_weekly_for_model$income_detailed <- factor(vmt_weekly_for_model$income_detailed, 
                                               c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-$100K")

## Join on travel times ----------------------------------------------------
# vmt_weekly_df <- vmt_weekly_for_model %>%
#   left_join(travel_times_df) %>%
#   relocate(person_id, work_arr, person_weight, survey_year, vmt_weekly, 
#            work_tour_vmt_weekly, nonwork_tour_vmt_weekly,
#            ln_vmt_weekly, ln_work_tour_vmt_weekly, ln_nonwork_tour_vmt_weekly)

vmt_weekly_df <- vmt_weekly_for_model

## Write out VMT weekly ---------------------------------------------------
write.csv(vmt_weekly_df, "./outputs/vmt_weekly_df.csv", row.names = F)


# VMT weekly model matrix (for OPSR) ----------------------------------------
vmt_weekly_convert <- vmt_weekly_df %>%
  select(person_id, person_weight, remote_day_factor, remote_day_pct_factor, age_group, gender, employment, 
         education, race, income_detailed, num_kids, num_vehicles, auto_sufficiency) 

vmt_weekly_convert <- vmt_weekly_convert[, - 1]
vmt_weekly_model_mat_step1 <- model.matrix(~ person_id + person_weight + remote_day_factor + remote_day_pct_factor + age_group + gender + employment + education + 
                                             race + income_detailed + num_kids + num_vehicles + auto_sufficiency, data = vmt_weekly_convert)
vmt_weekly_model_mat_step1 <- vmt_weekly_model_mat_step1[, -1]
colnames(vmt_weekly_model_mat_step1) <- c("person_id", "person_weight", "remote_day1", "remote_day2",
                                          "remote_day3", "remote_day4", "remote_day5plus","remote_pct20to40",
                                          "remote_pct40to60", "remote_pct60to80", "remote_pct80plus",
                                          "age18to34", "age55plus", "male", 
                                          "employed_part_time", "self_employed", "no_bach_plus", "nonWhite", 
                                          "hh_income_under50", "hh_income100to150", "hh_income150to200", 
                                          "hh_income_200plus", "hh_income_undisclosed", "kids_1plus", "vehicle1",
                                          "vehicle2plus", "auto_suf0", "auto_suf2")

vmt_weekly_model_mat_step2 <-  vmt_weekly_df %>%
  select(-age_group, -gender, -employment, -education, -race, -income_detailed, 
         -num_kids, -num_vehicles)

vmt_weekly_model_mat <- vmt_weekly_model_mat_step2 %>%
  left_join(vmt_weekly_model_mat_step1, copy = T)

vmt_weekly_model_mat$work_arr <- ifelse(vmt_weekly_model_mat$work_arr == "Always In-Person", 1,
                                        ifelse(vmt_weekly_model_mat$work_arr == "Hybrid", 2, 3))

vmt_weekly_model_mat <- vmt_weekly_model_mat[, !(names(vmt_weekly_model_mat) %in% c("hh_id", "num_workers"))]

# vmt_weekly_model_mat <- vmt_weekly_model_mat %>%
#   mutate(miles_to_work = if_else(is.na(miles_to_work), 0, miles_to_work),
#          minutes_to_work = if_else(is.na(minutes_to_work), 0, minutes_to_work))

vmt_weekly_model_mat <- vmt_weekly_model_mat %>%
  mutate(nonmale_kids_1plus = ifelse(male == 0 & kids_1plus == 1, 1, 0))

## Write out weekly model matrix for OPSR --------------------------------
write.csv(vmt_weekly_model_mat, "./outputs/vmt_weekly_model_mat.csv", row.names = F)


# VMT daily dataframe (for OLS) -------------------------------------------
vmt_weekly_df$survey_year <- factor(vmt_weekly_df$survey_year)
work_arr_daily$person_id <- as.numeric(work_arr_daily$person_id)
work_arr_daily$survey_year <- factor(work_arr_daily$survey_year)

vmt_daily_df <- commute_vmt_daily %>%
  left_join(vmt_weekly_df) %>%
  select(-total_work_days, -num_remote_days, -remote_day_factor, -remote_day_pct_factor) %>%
  mutate(vmt_daily = work_tour_vmt_daily + nonwork_tour_vmt_daily,
         ln_vmt_daily = log(vmt_daily + 1),
         ln_work_tour_vmt_daily = log(work_tour_vmt_daily + 1),
         ln_nonwork_tour_vmt_daily = log(nonwork_tour_vmt_daily + 1)) %>%
  left_join(work_arr_daily) %>%
  rename(work_arr_dayH = work_arr_day) %>%
  mutate(work_arr_day = case_when(work_arr_dayH %in% c("in_person_day", "hybrid_day") ~ "in_person_day",
                                    work_arr_dayH %in% c("remote_day") ~ "remote_day",
                                    work_arr_dayH %in% c("no_work_day") ~ "no_work_day")) %>%
  filter(!is.na(work_arr))

vmt_daily_df$travel_dow <- factor(vmt_daily_df$travel_dow, 
                               levels = c("Sunday", "Monday", "Tuesday", 
                                          "Wednesday", "Thursday", "Friday", 
                                          "Saturday"))


## Write out VMT daily --------------------------------
write.csv(vmt_daily_df, "./outputs/vmt_daily_df.csv", row.names = F)


# VMT daily model matrix (for OPSR) --------------------------------------
vmt_daily_model_mat_step1 <- model.matrix(~ person_id + person_weight + 
                                            travel_dow + age_group + gender + 
                                            employment + education + race + 
                                            income_detailed + num_kids + 
                                            num_vehicles + day_num + auto_sufficiency, data = vmt_daily_df)
vmt_daily_model_mat_step1 <- vmt_daily_model_mat_step1[, -1]

colnames(vmt_daily_model_mat_step1) <- c("person_id", "person_weight", "monday", 
                                         "tuesday",  "wednesday", "thursday", 
                                         "friday", "saturday", "age18to34", 
                                         "age55plus", "male", 
                                         "employed_part_time", "self_employed", 
                                         "no_bach_plus", "nonWhite", 
                                         "hh_income_under50", "hh_income100to150", 
                                         "hh_income150to200", "hh_income_200plus", 
                                         "hh_income_undisclosed", "kids_1plus", 
                                         "vehicle1","vehicle2plus", "day_num", 
                                         "auto_sufo", "auto_suf2")

vmt_daily_model_mat_step2 <-  vmt_daily_df %>%
  select(-age_group, -travel_dow, -gender, -employment, -education, -race, 
         -income_detailed, -num_kids, -num_vehicles, -auto_sufficiency)

vmt_daily_model_mat <- vmt_daily_model_mat_step2 %>%
  inner_join(vmt_daily_model_mat_step1, by = c("person_id", "person_weight", "day_num"), copy = T)

vmt_daily_model_mat$work_arr <- ifelse(vmt_daily_model_mat$work_arr == "Always In-Person", 1,
                                       ifelse(vmt_daily_model_mat$work_arr == "Hybrid", 2, 3))

vmt_daily_model_mat <- vmt_daily_model_mat[, !(names(vmt_daily_model_mat) %in% c("hh_id"))]

vmt_daily_model_mat$work_arr_day <- ifelse(vmt_daily_model_mat$work_arr_day == "in_person_day", 1,
                                       ifelse(vmt_daily_model_mat$work_arr_day == "remote_day", 2, 3))

## Write out daily model matrix for OPSR --------------------------------
write.csv(vmt_daily_model_mat, "./outputs/vmt_daily_model_mat.csv", row.names = F)


# Displacement Dataframe --------------------------------------------------
vmt_displacement_step1 <- vmt_daily_df %>%
  arrange(person_id, travel_dow) %>%
  relocate(person_id, travel_dow, nonwork_tour_vmt_daily, ln_nonwork_tour_vmt_daily, work_arr_day) %>%
  mutate(work_arr_day = ifelse(work_arr_day == "hybrid_day", "in_person_day", work_arr_day)) 

## First, get the number of in person, remote, and no work days for an individual person
## Determine if it's a weekday
vmt_displacement_all <- vmt_displacement_step1 %>%
  relocate(person_id, work_arr_day) %>%
  group_by(person_id) %>%
  mutate(n_in_person_day = sum(work_arr_day == "in_person_day"),
         n_remote_day = sum(work_arr_day == "remote_day"),
         n_no_work_day = sum(work_arr_day == "no_work_day"),
         weekday = if_else(travel_dow %in% c("Sunday", "Saturday"), 0, 1),
         weekend = if_else(travel_dow %in% c("Sunday", "Saturday"), 1, 0)) %>%
  relocate(person_id, work_arr_day, n_in_person_day, n_remote_day, n_no_work_day, weekday)

## Then, get the number of in person, remote, and no work days for weekdays only.
vmt_displacement_weekdays <- vmt_displacement_all %>%
  filter(weekday == 1) %>%
  group_by(person_id) %>%
  mutate(n_in_person_day_weekday = sum(work_arr_day == "in_person_day"),
         n_remote_day_weekday = sum(work_arr_day == "remote_day"),
         n_no_work_day_weekday = sum(work_arr_day == "no_work_day")) %>%
  select(person_id, travel_dow, n_in_person_day_weekday, n_remote_day_weekday, n_no_work_day_weekday)

## Get nonwork tour VMT for other days
vmt_displacement_step2 <- vmt_displacement_all %>%
  group_by(person_id) %>%
  mutate(nonwork_tour_vmt_other_days = nonwork_tour_vmt_weekly - nonwork_tour_vmt_daily,
         nonwork_tour_vmt_weekly_weekdays = ifelse(weekday == 1, sum(nonwork_tour_vmt_daily), 0),
         nonwork_tour_vmt_other_days_weekdays = nonwork_tour_vmt_weekly_weekdays - nonwork_tour_vmt_daily*weekday,
         nonwork_tour_vmt_daily_weekdays = nonwork_tour_vmt_daily * weekday,
         nonwork_tour_vmt_daily_weekends = nonwork_tour_vmt_daily * weekend) %>%
  ungroup() %>%
  group_by(person_id) %>%
  dplyr::summarize(nonwork_tour_vmt_weekly_weekdays = sum(nonwork_tour_vmt_daily_weekdays),
            nonwork_tour_vmt_weekly_weekends = sum(nonwork_tour_vmt_daily_weekends))

vmt_displacement <- vmt_displacement_all %>%
  left_join(vmt_displacement_weekdays) %>%
  group_by(person_id) %>%
  mutate(n_in_person_day_weekday = ifelse(is.na(n_in_person_day_weekday), max(n_in_person_day_weekday , na.rm = TRUE), n_in_person_day_weekday),
         n_remote_day_weekday = ifelse(is.na(n_remote_day_weekday), max(n_remote_day_weekday , na.rm = TRUE), n_remote_day_weekday),
         n_no_work_day_weekday = ifelse(is.na(n_no_work_day_weekday), max(n_no_work_day_weekday , na.rm = TRUE), n_no_work_day_weekday),
         in_person_day = if_else(work_arr_day == "in_person_day", 1, 0),
         remote_day = if_else(work_arr_day == "remote_day", 1, 0),
         no_work_day = if_else(work_arr_day == "no_work_day", 1, 0),
         n_other_in_person_days = n_in_person_day - in_person_day,
         n_other_remote_days = n_remote_day - remote_day,
         n_other_no_work_days = n_no_work_day - no_work_day,
         n_other_in_person_days_weekday = n_in_person_day_weekday - in_person_day*weekday,
         n_other_remote_days_weekday = n_remote_day_weekday - remote_day*weekday,
         n_other_no_work_days_weekday = n_no_work_day_weekday - no_work_day*weekday) %>%
  ungroup() %>%
  left_join(vmt_displacement_step2) %>%
  relocate(person_id, travel_dow, nonwork_tour_vmt_daily, nonwork_tour_vmt_weekly,
           nonwork_tour_vmt_weekly_weekdays, nonwork_tour_vmt_weekly_weekends) %>%
  mutate(nonwork_tour_vmt_other_weekdays = nonwork_tour_vmt_weekly_weekdays - nonwork_tour_vmt_daily*weekday,
         nonwork_tour_vmt_other_weekends = nonwork_tour_vmt_weekly_weekends - nonwork_tour_vmt_daily*weekend) %>%
  relocate(person_id, travel_dow, nonwork_tour_vmt_weekly_weekends, nonwork_tour_vmt_daily, nonwork_tour_vmt_other_weekends) 

vmt_displacement$travel_dow <- factor(vmt_displacement$travel_dow, 
                                      levels = c("Sunday", "Monday", "Tuesday",
                                                 "Wednesday", "Thursday", "Friday",
                                                 "Saturday"))

vmt_displacement$travel_dow <- relevel(vmt_displacement$travel_dow, ref = "Wednesday")

vmt_displacement$ln_nonwork_tour_vmt_other_all_days <- log(vmt_displacement$nonwork_tour_vmt_other_weekdays + 
                                                          vmt_displacement$nonwork_tour_vmt_other_weekends + 1)

vmt_displacement$ln_nonwork_tour_vmt_other_weekdays <- log(vmt_displacement$nonwork_tour_vmt_other_weekdays + 1)

vmt_displacement <- vmt_displacement %>%
  rename(vmt_today = vmt_daily,
         work_tour_vmt_today = work_tour_vmt_daily,
         nonwork_tour_vmt_today = nonwork_tour_vmt_daily,
         ln_vmt_today = ln_vmt_daily,
         ln_vmt_work_tour_today = ln_work_tour_vmt_daily,
         ln_nonwork_tour_vmt_today = ln_nonwork_tour_vmt_daily)

vmt_displacement <- vmt_displacement %>%
  relocate(person_id, travel_dow, vmt_today, work_tour_vmt_today, nonwork_tour_vmt_today)

## Write out VMT displacement -------------------------------------------
write.csv(vmt_displacement , "./outputs/vmt_daily_disp.csv", row.names = F)

# Displacement Model Matrix (for OPSR) -------------------------------------
vmt_displacement_mat_step1 <- model.matrix(~ person_id + person_weight + 
                                            travel_dow + age_group + gender + 
                                            employment + education + race + 
                                            income_detailed + num_kids + 
                                            num_vehicles + day_num + auto_sufficiency, data = vmt_displacement)
vmt_displacement_mat_step1 <- vmt_displacement_mat_step1[, -1]

colnames(vmt_displacement_mat_step1) <- c("person_id", "person_weight", "sunday", "monday", 
                                         "tuesday",  "thursday", 
                                         "friday", "saturday", "age18to34", 
                                         "age55plus", "male", 
                                         "employed_part_time", "self_employed", 
                                         "no_bach_plus", "nonWhite", 
                                         "hh_income_under50", "hh_income100to150", 
                                         "hh_income150to200", "hh_income_200plus", 
                                         "hh_income_undisclosed", "kids_1plus", 
                                         "vehicle1","vehicle2plus", "day_num", "auto_suf0", "auto_suf2")

vmt_displacement_mat_step2 <-  vmt_displacement %>%
  select(-age_group, -travel_dow, -gender, -employment, -education, -race, 
         -income_detailed, -num_kids, -num_vehicles)

vmt_displacement_mat <- vmt_displacement_mat_step2 %>%
  inner_join(vmt_displacement_mat_step1, by = c("person_id", "person_weight", "day_num"), copy = T)

vmt_displacement_mat$work_arr <- ifelse(vmt_displacement_mat$work_arr == "Always In-Person", 1,
                                       ifelse(vmt_displacement_mat$work_arr == "Hybrid", 2, 3))

vmt_displacement_mat <- vmt_displacement_mat[, !(names(vmt_displacement_mat) %in% c("hh_id"))]

vmt_displacement_mat$work_arr_day <- ifelse(vmt_displacement_mat$work_arr_day == "in_person_day", 1,
                                           ifelse(vmt_displacement_mat$work_arr_day == "remote_day", 2, 3))

## Write out daily model matrix for OPSR --------------------------------
write.csv(vmt_displacement_mat, "./outputs/vmt_daily_disp_mat.csv", row.names = F)


# Write out .RData file ---------------------------------------------------
vmt_files <- list(vmt_weekly_df, vmt_daily_df, vmt_weekly_model_mat, 
                  vmt_daily_model_mat, vmt_displacement, vmt_displacement_mat)

names(vmt_files) <- c("vmt_weekly_df", "vmt_daily_df", "vmt_weekly_model_mat",
                      "vmt_daily_model_mat", "vmt_displacement", "vmt_displacement_mat")
saveRDS(vmt_files, "./outputs/vmt_files.RData")

