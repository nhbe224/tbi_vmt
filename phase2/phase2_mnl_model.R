### Phase 2 MNL for TBI VMT Project
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase2/")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(data.table)
library(spatstat)
library(gridExtra)
library(grid)
library(gtable)
library(stringi)
options(scipen = 999)
require(nnet)
library(sf)
library(tigris)
library(gtools)
library(reshape2)
library(stargazer)

# Read Data ---------------------------------------------------------------
## Bring in cleaned weekly VMT data
vmt_weekly <- read.csv("../phase1/outputs/vmt_weekly.csv")

## Bring in person, household, day and trip data
source("../datacleaning/getvmt.R")
setwd("D:/neeco/thesis/tbi_vmt/phase2/")

## Bring in smart location database data
sld2018 <- fread("D:/neeco/rdc_census/sld_change_file/outputs/sld_change_file2018.csv")

## Read in jobs within 30 minute transit for 2021
mn_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Minnesota_transit_2021/Minnesota_27_transit_block_group_2021.csv")
wi_transit2021_load <- fread("D:/neeco/rdc_census/aaa_change_file/inputs/transit2021/Wisconsin_55_transit_block_group_2021.csv")
transit2021_load <- rbind(mn_transit2021_load, wi_transit2021_load)

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
  select(survey_year, person_id, hh_id, age, gender, employment, race, work_cbg_2010)

### Then join on VMT
vmt_weekly_w_person <- vmt_weekly %>%
  left_join(person_keep) 

## Look at non-response/missing columns
unique(vmt_weekly_w_person$age)
unique(vmt_weekly_w_person$race) 
unique(vmt_weekly_w_person$gender) # Filter out Prefer not to answer
unique(vmt_weekly_w_person$employment) 

## Age group
vmt_weekly_w_person <- vmt_weekly_w_person %>% 
  rename(age_group = age) 

vmt_weekly_w_person$age_group <- factor(vmt_weekly_w_person$age_group, 
                                        levels = c("18 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 or older" ))

## Race
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


## Join on household variables that are relevant to VMT -----------------------
hh_keep <- household_all %>%
  select(survey_year, hh_id, home_bg_2010, income_detailed, num_kids, num_adults,
         num_workers, num_vehicles) 

vmt_weekly_w_hh <- vmt_weekly_w_person %>%
  left_join(hh_keep)

### Clean household data
## See which variables need to be dropped
unique(vmt_weekly_w_hh$num_kids)
unique(vmt_weekly_w_hh$num_workers)
unique(vmt_weekly_w_hh$num_vehicles)
unique(vmt_weekly_w_hh$income_detailed) # Drop undisclosed

## Make numeric variables numeric
vmt_weekly_w_hh$num_kids <- as.numeric(substr(vmt_weekly_w_hh$num_kids, 1, 2))
vmt_weekly_w_hh$num_adults <- as.numeric(substr(vmt_weekly_w_hh$num_adults, 1, 2))
vmt_weekly_w_hh$num_workers <- as.numeric(substr(vmt_weekly_w_hh$num_workers, 1, 2))
vmt_weekly_w_hh$num_vehicles <- as.numeric(substr(vmt_weekly_w_hh$num_vehicles, 1, 2))

## Income
vmt_weekly_w_hh$income_detailed <- factor(vmt_weekly_w_hh$income_detailed, 
                                          levels = c( "<$15K", "$15-25K",  "$25-35K", "$35-50K", "$50-75K",   
                                                      "$75-100K", "$100-150K","$150-200K", "$200-$250K", "$250K+", "Undisclosed"))


## Create variable for number of teleworkers in household
vmt_weekly_w_hh$hybrid_or_remote <- ifelse(vmt_weekly_w_hh$work_arr %in% c("Always Remote", "Hybrid"), 1, 0)
vmt_weekly_w_hh <- vmt_weekly_w_hh %>%
  group_by(hh_id) %>% mutate(num_hybrid_or_remote = sum(hybrid_or_remote)) %>%
  select(-hybrid_or_remote)

# same_hh <- vmt_weekly_w_hh %>%
#   group_by(hh_id) %>%
#   filter(n() > 1)

### For regression on categorical variables like race, income, be explicit to computer
### that it's categorical OR have them in their own column as binaries (i.e. race_White, race_Black etc.)

## Join on Built Environment Variables -------------------------------------
sld2018 <- sld2018 %>%
  select(geoid, jobs_per_hh, emp8_ent, intersection_den)

transit2021 <- transit2021_load %>%
  filter(threshold == 1800) %>%
  rename(transit_jobs30 = weighted_average) %>%
  select(geoid, transit_jobs30) 

vmt_weekly_w_be <- vmt_weekly_w_hh %>%
  left_join(sld2018, by = c("home_bg_2010" = "geoid"))

vmt_weekly_w_be <- vmt_weekly_w_be %>%
  left_join(transit2021, by = c("home_bg_2010" = "geoid"))

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
vmt_weekly_for_model$survey_year <- as.character(vmt_weekly_for_model$survey_year)
write.csv(vmt_weekly_for_model, "./outputs/vmt_weekly_for_model.csv", row.names = F)


# Relevel -----------------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 44")
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-75K")



# Multinomial Logit Model -------------------------------------------------
mnl_model1 <- multinom(work_arr ~ age_group + gender + employment + race + 
                          income_detailed + num_kids + num_workers + num_vehicles + num_hybrid_or_remote +
                         jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + gas_price, 
                        data = vmt_weekly_for_model)

stargazer::stargazer(mnl_model1, type = "text")


# The relative probability of being always remote rather than being always in person is 22%
# higher for people ages 18 to 24 than for people ages 35 to 44, all else equal.
exp(0.199) 
exp(-0.245)

# The relative probability of being hybrid rather than being always in person is 72.9%
# lower for people ages 25 to 34 than for people ages 16 to 24, all else equal.


# Explore separate model for self employed and part time.
# Group income categories
# Group race into NH-white and everyone else.
# Number of hybrid workers/remote workers excluding self
# Presence of kids? 
# Number of vehicles: factor (no vehicles, 1+ vehicle)
# Include year as well.

