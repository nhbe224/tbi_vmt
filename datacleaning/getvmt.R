### Phase 1 Analysis for TBI VMT

# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/datacleaning")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(data.table)
library(spatstat)
library(plyr)
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
duplicates = []
for col in df.columns: 
  if '_y' in col: 
  duplicates.append(col)

df = df.drop(columns=duplicates)
