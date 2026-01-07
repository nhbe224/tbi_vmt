### Phase 2 MNL for TBI VMT Project
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase2_mnl/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable",
                     "stringi", "nnet", "plm", "forcats", "modelsummary", 
                     "foreign", "stargazer", "osrm")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)

# Read Data ---------------------------------------------------------------
## Bring in cleaned weekly VMT data
vmt_weekly <- read.csv("../phase1/outputs/vmt_weekly.csv")

## Bring in person, household, day and trip data
source("../datacleaning/getvmt.R")
setwd("D:/neeco/thesis/tbi_vmt/phase2_mnl/")

## Bring in smart location database data
sld2018 <- fread("D:/neeco/rdc_census/sld_change_file/outputs/sld_change_file2018.csv")

## Read in jobs within 30 minute transit for 2021
mn_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Minnesota_27_transit_block_group_2021.csv")
wi_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Wisconsin_55_transit_block_group_2021.csv")
transit2021_load <- rbind(mn_transit2021_load, wi_transit2021_load)
transitAll <- read.dta("D:/neeco/rdc_census/to_rdc/transit_ALL.dta")
transitAll <- transitAll %>%
  select(-threshold) %>%
  filter(year %in% c(2019, 2021, 2022))
## Proxy 2023 using 2022
transitAll$year <- ifelse(transitAll$year == 2022, 2023, transitAll$year)

## Read gas prices
mn_gas_prices_load <- read.csv("../inputs/mn_gas_prices.csv")

# Clean Data --------------------------------------------------------------
## Remove person weight
vmt_weekly <- vmt_weekly %>%
  select(-person_weight)

person_all$person_id <- as.numeric(person_all$person_id)

#vmt_weekly$work_arr <- factor(vmt_weekly$work_arr, levels = c("Always Remote", "Hybrid", "Always In-Person"))
## Join on person variables that are relevant to VMT -----------------------
### First find the variables in person that are relevant to VMT
person_keep <- person_all %>%
  select(survey_year, person_id, person_weight, hh_id, age, gender, employment, education,  race, work_cbg_2010)

### Then join on VMT
vmt_weekly_w_person <- vmt_weekly %>%
  left_join(person_keep) 

## Summary stats ---------------

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
vmt_weekly_w_person$race <- ifelse(vmt_weekly_w_person$race == "White", "White", "nonWhite")
vmt_weekly_w_person$race <- factor(vmt_weekly_w_person$race, 
                                   levels = unique(vmt_weekly_w_person$race))

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
         num_workers, num_vehicles) 

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

## Quick summary
stargazer(vmt_weekly_w_hh)

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

## Join on Built Environment Variables -------------------------------------
sld2018 <- sld2018 %>%
  select(bg2010, jobs_per_hh, emp8_ent, intersection_den)

# transit2021 <- transit2021_load %>%
#   filter(threshold == 1800) %>%
#   rename(transit_jobs30 = weighted_average) %>%
#   select(geoid, transit_jobs30) 

vmt_weekly_w_be <- vmt_weekly_w_hh %>%
  left_join(sld2018, by = c("home_bg_2010" = "bg2010"))

# vmt_weekly_w_be <- vmt_weekly_w_be %>%
#   left_join(transit2021, by = c("home_bg_2010" = "geoid"))
vmt_weekly_w_be$home_bg_2010 <- as.character(vmt_weekly_w_be$home_bg_2010)

vmt_weekly_w_be <- vmt_weekly_w_be %>%
  left_join(transitAll, by = c("home_bg_2010" = "bg2010", "survey_year" = "year"))
#rm(transitAll)

vmt_weekly_w_be <- vmt_weekly_w_be %>%
  rename(transit_jobs30 = jobs)

# Get the home BG where transit jobs is NA, fill with previous years' values
na_home_bg <- vmt_weekly_w_be[is.na(vmt_weekly_w_be$transit_jobs30), ]$home_bg_2010
# # See if it's in the dataset
vmt_weekly_w_be %>%
  filter(home_bg_2010 %in% na_home_bg) %>%
  select(transit_jobs30, survey_year, home_bg_2010)

wi_transit <- wi_transit2021_load %>%
  filter(geoid %in% c(550939604002, 550939607002),
         threshold == 1800) %>%
  select(geoid, weighted_average) %>%
  mutate(geoid = as.character(geoid))

vmt_weekly_w_be <- vmt_weekly_w_be %>%
  left_join(wi_transit, by = c("home_bg_2010" = "geoid"))

vmt_weekly_w_be$transit_jobs30 <- ifelse(is.na(vmt_weekly_w_be$transit_jobs30),
                                         vmt_weekly_w_be$weighted_average, vmt_weekly_w_be$transit_jobs30)

vmt_weekly_w_be <- vmt_weekly_w_be %>%
  select(-weighted_average)

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


# Get Commute VMT and Write Out -------------------------------------------
vmt_weekly_for_model$nonwork_tour_vmt_weekly <- vmt_weekly_for_model$weekly_vmt - vmt_weekly_for_model$work_tour_vmt_weekly
vmt_weekly_for_model <- vmt_weekly_for_model %>%
  relocate(person_id, work_arr, survey_year, weekly_vmt, work_tour_vmt_weekly, nonwork_tour_vmt_weekly)

#write.csv(vmt_weekly_for_model, "./outputs/vmt_weekly_for_model.csv", row.names = F)


# Relevel -----------------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 54")
vmt_weekly_for_model$income_detailed <- factor(vmt_weekly_for_model$income_detailed, 
                                              c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-$100K")

vmt_weekly_for_model$work_cbg_2010 <- as.character(vmt_weekly_for_model$work_cbg_2010)

# Get distance between home and work --------------------------------------
## Get OD pairs for always in-person and hybrid
od_pairs <- vmt_weekly_for_model %>%
  filter(work_arr %in% c("Always In-Person", "Hybrid")) %>%
  select(person_id, home_bg_2010, work_cbg_2010) %>%
  drop_na()

od_pairs <- od_pairs[, -1]

## Get centroids of CBGs
mn_bgs <- block_groups(state = "MN", year = 2010)
mn_bgs_centroids <- st_centroid(mn_bgs) %>%
  select(GEOID10, geometry) %>%
  rename(bg2010 = GEOID10, geom = geometry)


wi_bgs <- block_groups(state = "WI", year = 2010)
wi_bgs_centroids <- st_centroid(wi_bgs) %>%
  select(GEOID10, geometry) %>%
  rename(bg2010 = GEOID10, geom = geometry)

bgs_centroids <- rbind(mn_bgs_centroids, wi_bgs_centroids)

od_pairs <- od_pairs %>%
  left_join(bgs_centroids, c("home_bg_2010" = "bg2010")) 

colnames(od_pairs)[4] <- "home_geom"

od_pairs <- od_pairs %>%
  left_join(bgs_centroids, c("work_cbg_2010" = "bg2010")) 

colnames(od_pairs)[5] <- "work_geom"

origins <- as.data.frame(st_coordinates(od_pairs$home_geom))
destinations <- as.data.frame(st_coordinates(od_pairs$work_geom))

# Get travel time (and distance)
travel_times_list <- list()
for(i in 1:nrow(origins)) {
  src_point <- c(origins$X[i], origins$Y[i])
  dst_point <- c(destinations$X[i], destinations$Y[i])
  
  # Call osrmRoute for each pair
  route <- osrmRoute(
    src = src_point, 
    dst = dst_point, 
    overview = "simplified" # Get simplified geometry
  )
  
  # Store the result in the list, potentially adding an identifier
  route$person_id <- od_pairs$person_id[i]
  travel_times_list[[i]] <- route
}

# Combine all routes into a single sf data frame
travel_times_df <- do.call(rbind, travel_times_list)
travel_times_df <- travel_times_df %>%
  select(person_id, duration, distance)
row.names(travel_times_df) <- NULL
travel_times_df <- travel_times_df %>%
  st_drop_geometry()

vmt_weekly_for_model <- vmt_weekly_for_model %>%
  left_join(travel_times_df)

write.csv(vmt_weekly_for_model, "./outputs/vmt_weekly_for_model.csv", row.names = F)
