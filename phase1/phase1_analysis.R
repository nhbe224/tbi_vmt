### Phase 1 Analysis for TBI VMT
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase1/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", "patchwork", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable", "ggpattern")
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
twork_limit_daily <- 1

# Read in Data ------------------------------------------------------------
source("../datacleaning/getvmt.R")
setwd("D:/neeco/thesis/tbi_vmt/phase1/")

# Get a dataset of AIP, Hybrid, and AR -------------------------------------
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


## Convert day Number to numeric
trip_step3$day_num <- as.numeric(str_remove(trip_step3$day_num, "Day "))

trip_step3 <- trip_step3 %>%
  arrange(survey_year, person_id, day_num)

work_arr_daily <- trip_step3 %>%
  arrange(survey_year, person_id, day_num) %>%
  mutate(work_arr_day = case_when(telework_time <= twork_limit_daily & work_trip_made == 1 ~ "in_person_day",
                              telework_time > twork_limit_daily & work_trip_made == 1  ~ "hybrid_day",
                              telework_time > twork_limit_daily & work_trip_made == 0  ~ "remote_day",
                              telework_time <= twork_limit_daily & work_trip_made == 0 ~ "no_work_day")) %>% ## Part 1
  select(person_id, survey_year, work_arr_day, employment, job_type, day_num) 

write.csv(work_arr_daily, "./outputs/work_arr_daily.csv", row.names = F)

work_arr <- work_arr_daily %>%
  group_by(person_id, survey_year, work_arr_day, employment, job_type) %>%
  summarize(count = n()) %>%
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
  select(person_id, survey_year, work_arr, employment, job_type) %>%
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
  summarize(telework_hours = sum(telework_time, na.rm = T))

work_arr <- work_arr %>%
  left_join(telework_hours_per_week)

## Work arrangement cleaning
print(paste0(nrow(work_arr), ' people before exclusions.'))

## Filter out people who are always in-person AND have more than 40 hours of telework.
work_arr <- work_arr %>%
  filter(work_arr != "aip" | telework_hours <= 40)

print(paste0(nrow(work_arr), ' people after exclusions.'))

## Clean Data Frame
work_arr <- work_arr %>% mutate(work_arr = case_when(work_arr == "aip" ~ "Always In-Person",
                            work_arr == "hybrid" ~ "Hybrid",
                            work_arr == "ar" ~ "Always Remote"))

work_arr$work_arr <- factor(work_arr$work_arr, c("Always In-Person", "Hybrid", "Always Remote"))

work_arr_all <- work_arr
work_arr <- work_arr_all %>%
  select(person_id, survey_year, work_arr)

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

commute_vmt <- commute_vmt %>%
  mutate(tour_id_count = if_else((person_id != lag(person_id)) | (day_num != lag(day_num)), 1, if_else((o_purpose_category %in% c('Home', 'Overnight')), 1, 0), missing = 1)) %>%
  relocate(person_id, day_num, tour_id_count)

commute_vmt <- commute_vmt %>%
  group_by(person_id, day_num) %>%
  mutate(tour_id = cumsum(tour_id_count)) %>%
  relocate(person_id, day_num, tour_id) %>%
  select(-tour_id_count)

commute_vmt$work_trip = ifelse(commute_vmt$o_purpose_category %in% c("Work", "Work related") | commute_vmt$d_purpose_category %in% c("Work", "Work related"), 1, 0)
commute_vmt <- commute_vmt %>%
  relocate(person_id, day_num, tour_id, work_trip)

commute_vmt <- commute_vmt %>%
  group_by(person_id, day_num, tour_id) %>%
  mutate(work_tour = max(work_trip)) %>%
  relocate(person_id, day_num, tour_id, work_trip, work_tour)

commute_vmt$work_tour_vmt <- commute_vmt$work_tour * commute_vmt$vmt
commute_vmt$nonwork_tour_vmt <- ifelse(commute_vmt$work_tour == 0, commute_vmt$vmt, 0)

commute_vmt_daily <- commute_vmt %>%
  select(person_id, day_num, work_tour, work_tour_vmt, nonwork_tour_vmt, person_weight, survey_year, travel_dow) %>%
  group_by(person_id, day_num, travel_dow, person_weight, survey_year) %>%
  summarize(work_tour_vmt_daily = sum(work_tour_vmt),
            nonwork_tour_vmt_daily = sum(nonwork_tour_vmt)) 

commute_vmt_daily$person_id <- as.numeric(commute_vmt_daily$person_id)
commute_vmt_daily$survey_year <- factor(commute_vmt_daily$survey_year)

write.csv(commute_vmt_daily, "./outputs/commute_vmt_daily.csv", row.names = F)

commute_vmt_weekly <- commute_vmt %>%
  select(person_id, survey_year, work_tour, work_tour_vmt, person_weight) %>%
  group_by(person_id, survey_year, person_weight) %>%
  summarize(work_tour_vmt_weekly = sum(work_tour_vmt))

## Join Commute VMT weekly -------------------------------------------------
vmt_weekly <- vmt_by_work_arr %>%
  group_by(person_id, work_arr, person_weight, survey_year) %>%
  summarize(weekly_vmt = sum(vmt, na.rm = T))

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

## Write out data
write.csv(vmt_weekly, "./outputs/vmt_weekly.csv", row.names = F)

# Summary Stats -----------------------------------------------------------
## How many trips, how many people
trip_complete$person_id <- as.numeric(trip_complete$person_id)
vmt_weekly$person_id <- as.numeric(vmt_weekly$person_id)
vmt_weekly_for_summary <- vmt_weekly %>%
  left_join(trip_complete)

length(unique(vmt_weekly_for_summary$person_id))
rm(vmt_weekly_for_summary)

# Displacement ------------------------------------------------------------
vmt_df$person_id <- as.numeric(vmt_df$person_id)
work_arr_daily$person_id <- as.numeric(work_arr_daily$person_id)
work_arr_daily_join <- work_arr_daily %>% select(person_id, survey_year,work_arr_day,employment, )
vmt_weekly$survey_year <- factor(vmt_weekly$survey_year)
work_arr_daily$survey_year <- factor(work_arr_daily$survey_year)

## Join weekly VMT data with daily data (work VMT and non work VMT)
displacement_step1 <- vmt_weekly %>%
  left_join(commute_vmt_daily) %>%
  mutate(total_vmt_daily = work_tour_vmt_daily + nonwork_tour_vmt_daily)

## Then, get the day arrangement
displacement_step2 <- displacement_step1 %>%
  left_join(work_arr_daily) %>%
  relocate(person_id, work_arr_day, work_arr, travel_dow, total_vmt_daily, 
           work_tour_vmt_daily, nonwork_tour_vmt_daily)

displacement_step2$travel_dow <- factor(displacement_step2$travel_dow, 
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                     "Thursday", "Friday", "Saturday"),
                          ordered = TRUE)

displacement_step2 <- displacement_step2 %>%
  arrange(person_id, travel_dow)

## 2023 Displacement Measures for AIP  ------------------------
displacement_2023 <- displacement_step2 %>%
  filter(survey_year == 2023)

## Get Day Arrangement Percentages
displacement_aip2023 <- displacement_2023 %>%
  filter(work_arr == "Always In-Person") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(weighted_sum = sum(person_weight)) %>%
  group_by(travel_dow) %>%
  mutate(pct = weighted_sum / sum(weighted_sum)) %>%
  select(-weighted_sum)

displacement_aip2023_day_arr <- spread(displacement_aip2023, travel_dow, pct)
displacement_aip2023_day_arr[is.na(displacement_aip2023_day_arr)] <- 0
write.csv(displacement_aip2023_day_arr, "./outputs/displacement_aip2023_day_arr.csv", row.names = F)

## Get Mean Daily VMT
displacement_aip2023 <- displacement_2023 %>%
  filter(work_arr == "Always In-Person") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(mean_vmt_daily = weighted.mean(total_vmt_daily, person_weight))

displacement_aip2023_mean <- spread(displacement_aip2023, travel_dow, mean_vmt_daily)
displacement_aip2023_mean[is.na(displacement_aip2023_mean)] <- 0
write.csv(displacement_aip2023_mean, "./outputs/displacement_aip2023_mean_total_vmt.csv", row.names = F)

## Get Mean Daily Work VMT
displacement_aip2023 <- displacement_2023 %>%
  filter(work_arr == "Always In-Person") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(work_tour_vmt_daily = weighted.mean(work_tour_vmt_daily, person_weight))

displacement_aip2023_work <- spread(displacement_aip2023, travel_dow, work_tour_vmt_daily)
displacement_aip2023_work[is.na(displacement_aip2023_work)] <- 0
write.csv(displacement_aip2023_work, "./outputs/displacement_aip2023_work_vmt.csv", row.names = F)

## Get Total Daily Non-Work VMT
displacement_aip2023 <- displacement_2023 %>%
  filter(work_arr == "Always In-Person") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(nonwork_tour_vmt_daily = weighted.mean(nonwork_tour_vmt_daily, person_weight))

displacement_aip2023_nonwork <- spread(displacement_aip2023, travel_dow, nonwork_tour_vmt_daily)
displacement_aip2023_nonwork[is.na(displacement_aip2023_nonwork)] <- 0
write.csv(displacement_aip2023_nonwork, "./outputs/displacement_aip2023_nonwork_vmt.csv", row.names = F)

## 2023 Displacement Measures for Hybrid  ------------------------
## Get Day Arrangement Percentages
displacement_hybrid2023 <- displacement_2023 %>%
  filter(work_arr == "Hybrid") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(weighted_sum = sum(person_weight)) %>%
  group_by(travel_dow) %>%
  mutate(pct = weighted_sum / sum(weighted_sum)) %>%
  select(-weighted_sum)

displacement_hybrid2023_day_arr <- spread(displacement_hybrid2023, travel_dow, pct)
displacement_hybrid2023_day_arr[is.na(displacement_hybrid2023_day_arr)] <- 0
write.csv(displacement_hybrid2023_day_arr, "./outputs/displacement_hybrid2023_day_arr.csv", row.names = F)

## Get Total Daily VMT
displacement_hybrid2023 <- displacement_2023 %>%
  filter(work_arr == "Hybrid") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(mean_vmt_daily = weighted.mean(total_vmt_daily, person_weight))

displacement_hybrid2023_mean <- spread(displacement_hybrid2023, travel_dow, mean_vmt_daily)
displacement_hybrid2023_mean[is.na(displacement_hybrid2023_mean)] <- 0
write.csv(displacement_hybrid2023_total, "./outputs/displacement_hybrid2023_mean_vmt.csv", row.names = F)

## Get Total Daily Work VMT
displacement_hybrid2023 <- displacement_2023 %>%
  filter(work_arr == "Hybrid") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(work_tour_vmt_daily = weighted.mean(work_tour_vmt_daily, person_weight))

displacement_hybrid2023_work <- spread(displacement_hybrid2023, travel_dow, work_tour_vmt_daily)
displacement_hybrid2023_work[is.na(displacement_hybrid2023_work)] <- 0
write.csv(displacement_hybrid2023_work, "./outputs/displacement_hybrid2023_work_vmt.csv", row.names = F)

## Get Total Daily Non-Work VMT
displacement_hybrid2023 <- displacement_2023 %>%
  filter(work_arr == "Hybrid") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(nonwork_tour_vmt_daily = weighted.mean(nonwork_tour_vmt_daily, person_weight))

displacement_hybrid2023_nonwork <- spread(displacement_hybrid2023, travel_dow, nonwork_tour_vmt_daily)
displacement_hybrid2023_nonwork[is.na(displacement_hybrid2023_nonwork)] <- 0
write.csv(displacement_hybrid2023_nonwork, "./outputs/displacement_hybrid2023_nonwork_vmt.csv", row.names = F)

## 2023 Displacement Measures for Always Remote
## Get Day Arrangement Percentages
displacement_ar2023 <- displacement_2023 %>%
  filter(work_arr == "Always Remote") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(weighted_sum = sum(person_weight)) %>%
  group_by(travel_dow) %>%
  mutate(pct = weighted_sum / sum(weighted_sum)) %>%
  select(-weighted_sum)

displacement_ar2023_day_arr <- spread(displacement_ar2023, travel_dow, pct)
displacement_ar2023_day_arr[is.na(displacement_ar2023_day_arr)] <- 0
write.csv(displacement_ar2023_day_arr, "./outputs/displacement_ar2023_day_arr.csv", row.names = F)

## Get Total Daily VMT
displacement_ar2023 <- displacement_2023 %>%
  filter(work_arr == "Always Remote") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(mean_vmt_daily = weighted.mean(total_vmt_daily, person_weight))

displacement_ar2023_mean <- spread(displacement_ar2023, travel_dow, mean_vmt_daily)
displacement_ar2023_mean[is.na(displacement_ar2023_mean)] <- 0
write.csv(displacement_ar2023_mean, "./outputs/displacement_ar2023_mean_vmt.csv", row.names = F)

## Get Total Daily Work VMT
displacement_ar2023 <- displacement_2023 %>%
  filter(work_arr == "Always Remote") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(work_tour_vmt_daily = weighted.mean(work_tour_vmt_daily, person_weight))

displacement_ar2023_work <- spread(displacement_ar2023, travel_dow, work_tour_vmt_daily)
displacement_ar2023_work[is.na(displacement_ar2023_work)] <- 0
write.csv(displacement_ar2023_work, "./outputs/displacement_ar2023_work_vmt.csv", row.names = F)

## Get Total Daily Non-Work VMT
displacement_ar2023 <- displacement_2023 %>%
  filter(work_arr == "Always Remote") %>%
  group_by(travel_dow, work_arr_day) %>%
  summarize(nonwork_tour_vmt_daily = weighted.mean(nonwork_tour_vmt_daily, person_weight))

displacement_ar2023_nonwork <- spread(displacement_ar2023, travel_dow, nonwork_tour_vmt_daily)
displacement_ar2023_nonwork[is.na(displacement_ar2023_nonwork)] <- 0
write.csv(displacement_ar2023_nonwork, "./outputs/displacement_ar2023_nonwork_vmt.csv", row.names = F)


# Plot Data -------------------------------------------------------------
weighted_vmt_weekly$survey_year <- factor(weighted_vmt_weekly$survey_year, c(2019, 2021, 2023))

ggplot(weighted_vmt_weekly, aes(fill = work_arr, x = survey_year, y = mean_weekly_vmt)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Weighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(mean_weekly_vmt)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 215)
ggsave("./outputs/weighted_weekly_vmt.jpg", width = 8, height = 6, units = "in")

unweighted_vmt_weekly$survey_year <- factor(unweighted_vmt_weekly$survey_year, c(2019, 2021, 2023))

ggplot(unweighted_vmt_weekly, aes(fill = work_arr, x = survey_year, y = mean_weekly_vmt)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Unweighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(mean_weekly_vmt)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 205)
ggsave("./outputs/unweighted_weekly_vmt.jpg", width = 8, height = 6, units = "in")


# Rebound Effects ---------------------------------------------------------
### Always remote: 140.96-4.69 = 133.31 savings in mean work VMT
### Always remote: 138-64.3 = 73.7 increase in mean non work VMT
### rebound effect 73.1/133.1 = 55% rebound effect. Net savings overall.

### Hybrid: 138-87 = 51 savings in mean work VMT
### Hybrid: 89.2-64.3 = 24.9 savings in mean non work VMT
### Rebound effect = 48% rebound effect

mean_vmt_purpose_arr <- vmt_weekly %>%
  group_by(work_arr) %>%
  summarize(mean_weekly_vmt = weighted.mean(weekly_vmt, person_weight, na.rm = T),
            mean_work_tour_vmt = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            mean_nonwork_tour_vmt = weighted.mean(weekly_vmt - work_tour_vmt_weekly, person_weight, na.rm = T))

mean_vmt_aip <- mean_vmt_purpose_arr %>%
  filter(work_arr == "Always In-Person")

mean_vmt_not_aip <- mean_vmt_purpose_arr %>%
  filter(work_arr != "Always In-Person")

## Create always in-person dataframe to get rebound effects.
mean_vmt_aip <- rbind(mean_vmt_aip, mean_vmt_aip)
mean_vmt_aip[c(1), 1] <- "Always Remote"
mean_vmt_aip[c(2), 1] <- "Hybrid"
colnames(mean_vmt_aip)[c(2:4)] <- c("aip_mean_weekly_vmt", "aip_mean_work_tour_vmt", "aip_mean_nonwork_tour_vmt")

## Rebound effect dataframe
rebound_effect <- mean_vmt_not_aip %>% inner_join(mean_vmt_aip) %>%
  mutate(total_vmt_change = round(mean_weekly_vmt - aip_mean_weekly_vmt),
         total_vmt_pct_change = round((mean_weekly_vmt - aip_mean_weekly_vmt) / (aip_mean_weekly_vmt), 2),
         work_vmt_pct_change = round((mean_work_tour_vmt - aip_mean_work_tour_vmt ) / (aip_mean_work_tour_vmt), 2),
         nonwork_vmt_pct_change = round((mean_nonwork_tour_vmt - aip_mean_nonwork_tour_vmt) / (aip_mean_nonwork_tour_vmt), 2),
         work_vmt_savings = aip_mean_work_tour_vmt - mean_work_tour_vmt,
         nonwork_vmt_increase = mean_nonwork_tour_vmt - aip_mean_nonwork_tour_vmt,
         rebound_effect = round((nonwork_vmt_increase / work_vmt_savings), 2)) %>%
  select(work_arr, total_vmt_pct_change, work_vmt_pct_change, nonwork_vmt_pct_change, rebound_effect) %>%
  arrange(desc(work_arr))
write.csv(rebound_effect, "./outputs/rebound_effect.csv", row.names = F)

rebound_effect_underlying_math = mean_vmt_not_aip %>% inner_join(mean_vmt_aip) %>%
  mutate(work_vmt_savings = aip_mean_work_tour_vmt - mean_work_tour_vmt,
         nonwork_vmt_increase = mean_nonwork_tour_vmt - aip_mean_nonwork_tour_vmt,
         rebound_effect = round((nonwork_vmt_increase / work_vmt_savings), 2)) %>%
  select(work_arr, survey_year, aip_mean_work_tour_vmt, mean_work_tour_vmt, work_vmt_savings, 
         mean_nonwork_tour_vmt, aip_mean_nonwork_tour_vmt, nonwork_vmt_increase, rebound_effect) %>%
  arrange(survey_year, desc(work_arr)) %>% 
  mutate_at(vars(aip_mean_work_tour_vmt, mean_work_tour_vmt, work_vmt_savings, 
                 mean_nonwork_tour_vmt, aip_mean_nonwork_tour_vmt, nonwork_vmt_increase), funs(round(., 1)))
write.csv(rebound_effect_underlying_math, "./outputs/rebound_effect_underlying_math.csv", row.names = F)

# Trip-Level VMT Distribution ----------------------------------------------
## Construct Data ----------------------------------------------------------
vmt_dist <- vmt_by_work_arr %>%
  select(work_arr, vmt, survey_year) %>%
  filter(vmt > 0 & vmt <= 25)

## Plot Data -------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(vmt_dist %>% filter(survey_year == i) %>%
          ggplot(aes(x=vmt, group=work_arr, fill=work_arr)) +
          geom_density(adjust=1.5, alpha=.4) +
          ggtitle(paste0("VMT Distribution by Work Arrangement (", i, ")")) + ylim(0, 0.25) +
          xlab("VMT") + ylab("Density") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom"))
}

# Percentage of AIP, Hybrid, and AR --------------------------------------
## Construct Data ----------------------------------------------------------
revealed_work_arr_pct_w <- work_arr %>%
  inner_join(vmt_df, c("person_id", "survey_year")) %>%
  select(person_id, work_arr, survey_year, person_weight) %>%
  distinct() %>%
  group_by(survey_year, work_arr) %>%
  summarize(group_sum = sum(person_weight, na.rm = T)) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total) %>%
  select(work_arr, survey_year, percent)

stated_work_arr_pct_w <- work_arr_all %>%
  inner_join(vmt_df, c("person_id", "survey_year")) %>%
  select(person_id, job_type, survey_year, person_weight) %>%
  distinct() %>%
  group_by(survey_year, job_type) %>%
  summarize(group_sum = sum(person_weight, na.rm = T)) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total) %>%
  select(job_type, survey_year, percent)

revealed_work_arr_pct_u <- work_arr %>%
  inner_join(vmt_df, c("person_id", "survey_year")) %>%
  select(person_id, work_arr, survey_year) %>%
  distinct() %>%
  group_by(survey_year, work_arr) %>%
  summarize(group_sum = n()) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total) %>%
  select(work_arr, survey_year, percent)

stated_work_arr_pct_u <- work_arr_all %>%
  left_join(vmt_df, c("person_id", "survey_year")) %>%
  select(person_id, job_type, survey_year) %>%
  distinct() %>%
  group_by(survey_year, job_type) %>%
  summarize(group_sum = n()) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total) %>%
  select(job_type, survey_year, percent)

## Make Crosstab ------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  t1 <- work_arr_all %>%
    filter(survey_year == i) %>%
    group_by(work_arr) %>%
    count(work_arr, job_type) %>%
    mutate(freq = round(n/sum(n)*100, 2))
  t1 <- tableGrob(t1)
  title <- textGrob(paste0("Crosstab for ", i),gp=gpar(fontsize=20))
  padding <- unit(5,"mm")
  table <- gtable_add_rows(
    t1, 
    heights = grobHeight(title) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    title, 
    1, 1, 1, ncol(table))
  grid.newpage()
  print(grid.draw(table))
}

## Plot Data ------------------------------------------------------------------------
## Weighted
stated_work_arr_pct_w$survey_year <- factor(stated_work_arr_pct_w$survey_year, c(2019, 2021, 2023))

ggplot(stated_work_arr_pct_w, aes(fill = job_type, x = survey_year, y = percent)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Stated Distirbution of Work Arrangements by Year (Weighted)") +
  xlab("Year") + ylab("Percent") + guides(fill=guide_legend(title="")) + theme(legend.position="right") +
  geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9))

revealed_work_arr_pct_w$survey_year <- factor(revealed_work_arr_pct_w$survey_year, c(2019, 2021, 2023))

p1 <-ggplot(revealed_work_arr_pct_w, aes(fill = work_arr, x = survey_year, y = percent)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Observed Distirbution of Work Arrangements by Year (Weighted)") +
  xlab("Year") + ylab("Percent") + guides(fill=guide_legend(title="Work Arrangement")) + theme(legend.position="right") +
  geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) + ylim(0, 1)
p1
ggsave("./outputs/weighted_work_arr_dist.jpg", width = 10, height = 6, units = "in")

## Unweighted
stated_work_arr_pct_u$survey_year <- factor(stated_work_arr_pct_u$survey_year, c(2019, 2021, 2023))

ggplot(stated_work_arr_pct_u, aes(fill = job_type, x = survey_year, y = percent)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Stated Distirbution of Work Arrangements by Year (Unweighted)") +
  xlab("Year") + ylab("Percent") + guides(fill=guide_legend(title="")) + theme(legend.position="right") +
  geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9))

revealed_work_arr_pct_u$survey_year <- factor(revealed_work_arr_pct_u$survey_year, c(2019, 2021, 2023))

p2 <- ggplot(revealed_work_arr_pct_u, aes(fill = work_arr, x = survey_year, y = percent)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Observed Distirbution of Work Arrangements by Year (Unweighted)") +
  xlab("Year") + ylab("Percent") + guides(fill=guide_legend(title="")) + theme(legend.position="right") +
  geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) + ylim(0, 1)
#ggsave("./outputs/unweighted_work_arr_dist.jpg", width = 8, height = 6, units = "in")

combined_plot <- p1 / p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
combined_plot
ggsave("./outputs/work_arr_dist.jpg", width = 8, height = 8, units = "in")

# Average Work Tour VMT by Work Arrangement -------------------------------------
## Construct Data ----------------------------------------------------------
weighted_commute_vmt <- commute_vmt_weekly %>%
  left_join(work_arr) %>%
  group_by(work_arr, survey_year) %>%
  summarize(w_mean_work_tour_vmt = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T))

weighted_commute_vmt$survey_year <- factor(weighted_commute_vmt$survey_year, c(2019, 2021, 2023))

unweighted_commute_vmt <-  commute_vmt_weekly %>%
  left_join(work_arr) %>%
  group_by(work_arr, survey_year) %>%
  summarize(u_mean_work_tour_vmt = mean(work_tour_vmt_weekly, na.rm = T))

unweighted_commute_vmt$survey_year <- factor(unweighted_commute_vmt$survey_year, c(2019, 2021, 2023))

## Plot Data ----------------------------------------------------------
ggplot(weighted_commute_vmt, aes(fill = work_arr, x = survey_year, y = w_mean_work_tour_vmt)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly Total Work Tour VMT by Work Arrangement (Weighted)") +
  xlab("Year") + ylab("VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(w_mean_work_tour_vmt, 2)), size = 3, vjust = -0.25, position = position_dodge(.9))
#ggsave("./outputs/weighted_weekly_work_tour_vmt.jpg", width = 8, height = 6, units = "in")

ggplot(unweighted_commute_vmt, aes(fill = work_arr, x = survey_year, y = u_mean_work_tour_vmt)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly Total Work Tour VMT by Work Arrangement (Unweighted)") +
  xlab("Year") + ylab("VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(u_mean_work_tour_vmt, 2)), size = 3, vjust = -0.25, position = position_dodge(.9))
#ggsave("./outputs/weighted_weekly_work_tour_vmt.jpg", width = 8, height = 6, units = "in")


# Average Weekly Total VMT (Work/Non-Work) by Work Arrangement ------------
vmt_weekly$nonwork_tour_weekly_vmt <- vmt_weekly$weekly_vmt - vmt_weekly$work_tour_vmt_weekly

## Weighted ------------
vmt_weekly_work_nonwork_w <- vmt_weekly %>%
  group_by(work_arr, survey_year) %>%
  summarize(work_vmt = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            nonwork_vmt = weighted.mean(nonwork_tour_weekly_vmt, person_weight, na.rm = T)) %>%
  gather(key = vmt_type, value = weekly_vmt, work_vmt:nonwork_vmt)

vmt_weekly_work_nonwork_w$vmt_type <- ifelse(vmt_weekly_work_nonwork_w$vmt_type == "work_vmt", "Work VMT", "Non-Work VMT")
vmt_weekly_work_nonwork_w$survey_year <- factor(vmt_weekly_work_nonwork_w$survey_year, c(2019, 2021, 2023))

p1 <- ggplot(vmt_weekly_work_nonwork_w, aes(fill = work_arr, x = survey_year, y = weekly_vmt, pattern = vmt_type)) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black", # color of the pattern itself
                   pattern_colour = "black", # border color of the pattern
                   pattern_density = 0.01, # density of the pattern
                   pattern_spacing = 0.01, # spacing between pattern elements
                   pattern_angle = 45) +
  scale_pattern_manual(values = c(NA, "stripe")) + xlab("Year") + ylab("Average Weekly Total VMT") +
  ggtitle("Average Weekly Total VMT by Work Arrangement and VMT Type (Weighted)") +
  guides(fill = guide_legend(title = "Work Arrangement", 
    override.aes = list(
      pattern = c("none", "none", "none") # "none" for Category A, others retain patterns
    )
  ), pattern = guide_legend("VMT Type",
                            override.aes = list(fill = c("lightgrey", "lightgrey")))) +
  geom_text(aes(label = round(weekly_vmt, 1)), size = 2.5, vjust = -0.25, position = position_dodge(.9))

p1
ggsave("./outputs/mean_weekly_work_nonwork_vmt_w.jpg", width = 10, height = 6, units = "in")


## Unweighted --------------------------------------------------------------
vmt_weekly_work_nonwork_u <- vmt_weekly %>%
  group_by(work_arr, survey_year) %>%
  summarize(work_vmt = mean(work_tour_vmt_weekly),
            nonwork_vmt = mean(nonwork_tour_weekly_vmt)) %>%
  gather(key = vmt_type, value = weekly_vmt, work_vmt:nonwork_vmt)

vmt_weekly_work_nonwork_u$vmt_type <- ifelse(vmt_weekly_work_nonwork_u$vmt_type == "work_vmt", "Work VMT", "Non-Work VMT")
vmt_weekly_work_nonwork_u$survey_year <- factor(vmt_weekly_work_nonwork_u$survey_year, c(2019, 2021, 2023))

p2 <- ggplot(vmt_weekly_work_nonwork_u, aes(fill = work_arr, x = survey_year, y = weekly_vmt, pattern = vmt_type)) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black", # color of the pattern itself
                   pattern_colour = "black", # border color of the pattern
                   pattern_density = 0.01, # density of the pattern
                   pattern_spacing = 0.01, # spacing between pattern elements
                   pattern_angle = 45) +
  scale_pattern_manual(values = c(NA, "stripe")) + xlab("Year") + ylab("Average Weekly Total VMT") +
  ggtitle("Average Weekly Total VMT by Work Arrangement (Unweighted)") +
  guides(fill = guide_legend(title = "Work Arrangement", 
                             override.aes = list(
                               pattern = c("none", "none", "none") # "none" for Category A, others retain patterns
                             )
  ), pattern = guide_legend("VMT Type",
                            override.aes = list(fill = c("lightgrey", "lightgrey")))) +
  geom_text(aes(label = round(weekly_vmt, 2)), size = 2.5, vjust = -0.25, position = position_dodge(.9))


## Stack ------------------------------------------------------------
combined_plot <- p1 / p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
combined_plot
ggsave("./outputs/weekly_total_vmt.jpg", width = 10, height = 8, units = "in")


# Average Weekly Total VMT by Work Arrangement and Purpose -------------------
## Construct Data ---------------------------------------------------------
work_arr_purpose <- work_arr %>%
  inner_join(vmt_df, c("person_id", "survey_year")) %>%
  select(person_id, work_arr, person_weight, vmt, d_purpose_category, survey_year) %>%
  filter(!d_purpose_category %in% c("Home", "Not imputable", "Missing: Non-response", "Change mode", "Other")) %>%
  group_by(person_id, work_arr, person_weight, survey_year, d_purpose_category) %>%
  summarize(vmt_weekly = sum(vmt)) %>%
  group_by(survey_year, work_arr, d_purpose_category) %>%
  summarize(vmt_weekly_w = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_u = mean(vmt_weekly, na.rm = T))

work_arr_purpose$survey_year <- factor(work_arr_purpose$survey_year, c(2019, 2021, 2023))

## Plot Data --------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(work_arr_purpose %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr, y = vmt_weekly_w)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Weekly Total VMT by Purpose and Work Arrangement (", i, ", Weighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(vmt_weekly_w, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 60))
  ggsave(paste0("./outputs/w_weekly_total_vmt_purpose_work_arr_", i, ".jpg"), width = 8, height = 6, units = "in")
}

for(i in c(2019, 2021, 2023)){
  print(work_arr_purpose %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr, y = vmt_weekly_u)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Weekly Total VMT by Purpose and Work Arrangement (", i, ", Unweighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(vmt_weekly_u, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 60))
  ggsave(paste0("./outputs/u_weekly_total_vmt_purpose_work_arr_", i, ".jpg"), width = 8, height = 6, units = "in")
}

# Work Arrangements by Employment Status --------------------------------------
## Construct Data -------------------------------------------------------------
work_arr_emp_status_w <- work_arr_all %>%
  inner_join(person_weights) %>%
  group_by(survey_year, work_arr, employment) %>%
  summarize(group_sum = sum(person_weight, na.rm = T)) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total)

work_arr_emp_status_w$survey_year <- factor(work_arr_emp_status_w$survey_year, c(2019, 2021, 2023))

work_arr_emp_status_u <- work_arr_all %>%
  group_by(survey_year, work_arr, employment) %>%
  summarize(count = n()) %>%
  mutate(percent = (count / sum(count))) 

work_arr_emp_status_u$survey_year <- factor(work_arr_emp_status_u$survey_year, c(2019, 2021, 2023))

## Plot Data --------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(work_arr_emp_status_w %>% filter(survey_year == i) %>% ggplot(aes(fill = employment, x = work_arr, y = percent)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Work Arrangement by Employment Status (", i, ", Weighted)")) +
          xlab("Work Arrangement") + ylab("Percent") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
          geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
          ylim(0, 1))
}

for(i in c(2019, 2021, 2023)){
  print(work_arr_emp_status_u %>% filter(survey_year == i) %>% ggplot(aes(fill = employment, x = work_arr, y = percent)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Work Arrangement by Employment Status (", i, ", Unweighted)")) +
          xlab("Work Arrangement") + ylab("Percent") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
          geom_text(aes(label = round(percent, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
          ylim(0, 1))
}

# Work Arrangement by Income --------------------------------------------------
## Construct Data -------------------------------------------------------------
income_work_arr <- work_arr %>%
  inner_join(vmt_df) %>%
  select(person_id, person_weight, income_broad, survey_year, work_arr) %>%
  filter(income_broad != "Undisclosed") %>%
  distinct() %>%
  drop_na()

income_work_arr$work_arr <- factor(income_work_arr$work_arr, c("Always In-Person", "Hybrid", "Always Remote"))

income_work_arr$income_broad <- factor(income_work_arr$income_broad,
                                            c('<$25K', '$25-50K',
                                              '$50-75K', '$75-100K', 
                                              '$100K+'))

unique(income_work_arr$income_broad)
income_work_arr <- income_work_arr %>%
  group_by(work_arr, survey_year, income_broad) %>% 
  summarize(N = n(), wtd_N = sum(person_weight)) %>%
  mutate(pct_u = N / sum(N), pct_w = wtd_N / sum(wtd_N)) %>%
  ungroup()

## Plot Data -----------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(income_work_arr %>% filter(survey_year == i) %>% 
    ggplot(aes(fill = work_arr, x = income_broad, y = pct_w)) +  
    geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Income by Work Arrangement (", i , " Weighted)")) +
    xlab("Work Arrangement") + ylab("Percent") + guides(fill=guide_legend(title="Income Level")) +
    geom_text(aes(label = round(pct_w, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
    ylim(0, 0.9))
}

for(i in c(2019, 2021, 2023)){
  print(income_work_arr %>% filter(survey_year == i) %>% 
          ggplot(aes(fill = work_arr, x = income_broad, y = pct_u)) +  
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Income by Work Arrangement (", i , " Unweighted)")) +
          xlab("Work Arrangement") + ylab("Percent") + guides(fill=guide_legend(title="Income Level")) +
          geom_text(aes(label = round(pct_u, 2)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
          ylim(0, 0.8))
}

# Average VMT by Purpose and Day Arrangement ------------------------------
## Construct Data ---------------------------------------------------------
vmt_purpose_day_arr_step1 <- trip_step3 %>%
  arrange(survey_year, person_id, day_num) %>%
  mutate(work_arr_day = case_when(telework_time <= twork_limit_daily & work_trip_made == 1 ~ "in_person_day",
                                  telework_time > twork_limit_daily & work_trip_made == 1  ~ "hybrid_day",
                                  telework_time > twork_limit_daily & work_trip_made == 0  ~ "remote_day",
                                  telework_time <= twork_limit_daily & work_trip_made == 0 ~ "no_work_day")) %>%
  select(survey_year, person_id, day_num, work_arr_day)

vmt_purpose_day_arr_step1$day_num <- as.numeric(str_remove(vmt_purpose_day_arr_step1$day_num, "Day "))

vmt_purpose_day_arr <- vmt_df %>%
  inner_join(vmt_purpose_day_arr_step1) %>%
  select(survey_year, person_id, person_weight, day_num, vmt, work_arr_day, d_purpose_category) %>%
  arrange(survey_year, person_id, day_num) %>%
  filter(!d_purpose_category %in% c("Not imputable"))

vmt_purpose_day_arr$work_arr_day <- gsub("_", " ", vmt_purpose_day_arr$work_arr_day)
vmt_purpose_day_arr$work_arr_day <- str_to_title(vmt_purpose_day_arr$work_arr_day)
vmt_purpose_day_arr$work_arr_day <- ifelse(vmt_purpose_day_arr$work_arr_day == "In Person Day", "In-Person Day", vmt_purpose_day_arr$work_arr_day)
vmt_purpose_day_arr$work_arr_day <- factor(vmt_purpose_day_arr$work_arr_day, c("In-Person Day", "Hybrid Day", "Remote Day", "No Work Day"))

vmt_purpose_day_arr <- vmt_purpose_day_arr %>%
  filter(!d_purpose_category %in% c("Home", "Not imputable", "Missing: Non-response", "Change mode", "Other")) %>%
  group_by(work_arr_day, survey_year, d_purpose_category) %>%
  summarize(weighted_vmt = weighted.mean(vmt, person_weight, na.rm = T),
             unweighted_vmt = mean(vmt, na.rm = T)) %>%
  drop_na()

## Plot Data --------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(vmt_purpose_day_arr  %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr_day, y = weighted_vmt)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Trip VMT by Purpose and Day Arrangement (", i, ", Weighted)")) +
          xlab("Day Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(weighted_vmt, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 20))
}

for(i in c(2019, 2021, 2023)){
  print(vmt_purpose_day_arr  %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr_day, y = unweighted_vmt)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Trip VMT by Purpose and Day Arrangement (", i, ", Unweighted)")) +
          xlab("Day Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(unweighted_vmt, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 20))
}

# Average Trip VMT by Purpose and Day of Week -----------------------------
## Construct Data ---------------------------------------------------------
vmt_df$travel_dow <- factor(vmt_df$travel_dow, c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday"))

vmt_purpose_dow <- vmt_df %>%
  select(survey_year, person_id, person_weight, linked_trip_weight, d_purpose_category, travel_dow, vmt) %>%
  filter(!d_purpose_category %in% c("Home", "Not imputable", "Missing: Non-response", "Change mode", "Other")) %>%
  group_by(survey_year, d_purpose_category, travel_dow) %>%
  summarize(weighted_vmt = weighted.mean(vmt, person_weight, na.rm = T),
            unweighted_vmt = mean(vmt, na.rm = T))
  
## Plot Data ---------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(vmt_purpose_dow  %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = travel_dow, y = weighted_vmt)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Trip VMT by Purpose and Day of Week (", i, ", Weighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(weighted_vmt, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 23))
}

for(i in c(2019, 2021, 2023)){
  print(vmt_purpose_dow  %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = travel_dow, y = unweighted_vmt)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Trip VMT by Purpose and Day of Week (", i, ", Unweighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(unweighted_vmt, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 20))
}


# Day Arrangement by DOW --------------------------------------------------
## Construct Data ---------------------------------------------------------
day_arr_dow <- vmt_purpose_day_arr_step1 %>%
  inner_join(vmt_df) %>%
  select(survey_year, person_id, work_arr_day, travel_dow) %>%
  left_join(person_weights) %>%
  group_by(survey_year, travel_dow, work_arr_day) %>%
  summarize(N = n(), wtd_N = sum(person_weight)) %>%
  mutate(pct_u = N / sum(N), pct_w = wtd_N / sum(wtd_N)) %>%
  ungroup()

day_arr_dow$work_arr_day <- gsub("_", " ", day_arr_dow$work_arr_day)
day_arr_dow$work_arr_day <- str_to_title(day_arr_dow$work_arr_day)
day_arr_dow$work_arr_day <- ifelse(day_arr_dow$work_arr_day == "In Person Day", "In-Person Day", day_arr_dow$work_arr_day)
day_arr_dow$work_arr_day <- factor(day_arr_dow$work_arr_day, c("In-Person Day", "Hybrid Day", "Remote Day", "No Work Day"))

## Plot Data ---------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(day_arr_dow  %>% filter(survey_year == i) %>% ggplot(aes(fill = work_arr_day, x = travel_dow, y = pct_w)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Day Arrangement by Day of Week (", i, ", Weighted)")) +
          xlab("Day of Week") + ylab("Percentage") + guides(fill=guide_legend(title="Day Arrangement")) +
          geom_text(aes(label = round(pct_w, 2)), size = 2.5, vjust = -0.25, hjust = 0.5, position = position_dodge(0.9)) +
          ylim(0, 1))
}

for(i in c(2019, 2021, 2023)){
  print(day_arr_dow  %>% filter(survey_year == i) %>% ggplot(aes(fill = work_arr_day, x = travel_dow, y = pct_u)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Day Arrangement by Day of Week (", i, ", Unweighted)")) +
          xlab("Day of Week") + ylab("Percentage") + guides(fill=guide_legend(title="Day Arrangement")) +
          geom_text(aes(label = round(pct_u, 2)), size = 2.5, vjust = -0.25, hjust = 0.5, position = position_dodge(0.9)) +
          ylim(0, 1))
}


#### Notes
## Day of week and Day Arrangement percentages (i.e. trip purpose, break out by day arrangement).
## Work Arrangement by Day Arrangement by DOW.
## Look at Python script to get tour ID. Get tour purpose.
##### Hierarchy: work, work-related, escort, maintenance activities (shopping, errands, etc.), discretionary (meals, social)

