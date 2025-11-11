### Phase 2 Regression Models
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase2_regression/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable",
                     "stringi", "nnet", "plm", "forcats", "OPSR", "texreg")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)

# Read Data ---------------------------------------------------------------



# Crosstab for Count of Workers -------------------------------------------
vmt_weekly_for_model %>%
  group_by(survey_year, work_arr) %>%
  summarize(n = n()) %>%
  mutate(freq = round(n / sum(n), 2))

# Percent of sample used in analysis --------------------------------------
nrow(vmt_weekly_for_model) / nrow(person_all)

# Convert year to factor --------------------------------------------------
vmt_weekly_for_model$survey_year <- as.factor(vmt_weekly_for_model$survey_year)
class(vmt_weekly_for_model$survey_year)

# Relevel -----------------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 54")
vmt_weekly_for_model$income_detailed <- factor(vmt_weekly_for_model$income_detailed, 
                                               c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-$100K")

# Categorical Data --------------------------------------------------------
categorical_vars <- vmt_weekly_for_model %>%
  select(age_group, gender, employment, race, education, income_detailed, 
         num_kids, num_vehicles, survey_year)

categorical_vars <- as.data.frame(categorical_vars[-1])

categorical_vars %>%
  group_by(age_group) %>%
  summarize(count = n()) %>%
  mutate(pct = count / sum(count))
         
for(i in colnames(categorical_vars)){
  print(categorical_vars %>%
          group_by(.data[[i]]) %>%
          summarize(count = n()) %>%
          mutate(pct = count / sum(count)))
}


# Displacement Data -------------------------------------------------------
vmt_daily_disp <- vmt_daily %>%
  arrange(person_id, travel_dow) %>%
  relocate(person_id, travel_dow, nonwork_tour_vmt_daily, ln_nonwork_tour_vmt_daily, work_arr_day) %>%
  mutate(work_arr_day = ifelse(work_arr_day == "hybrid_day", "in_person_day", work_arr_day)) %>%
  group_by(person_id) %>%
  mutate(work_arr_day_p1 = dplyr::lead(work_arr_day, n = 1),
         work_arr_day_p2 = dplyr::lead(work_arr_day, n = 2),
         work_arr_day_p3 = dplyr::lead(work_arr_day, n = 3),
         work_arr_day_p4 = dplyr::lead(work_arr_day, n = 4),
         work_arr_day_p5 = dplyr::lead(work_arr_day, n = 5),
         work_arr_day_p6 = dplyr::lead(work_arr_day, n = 6),
         work_arr_day_m1 = dplyr::lag(work_arr_day, n = 1),
         work_arr_day_m2 = dplyr::lag(work_arr_day, n = 2),
         work_arr_day_m3 = dplyr::lag(work_arr_day, n = 3),
         work_arr_day_m4 = dplyr::lag(work_arr_day, n = 4),
         work_arr_day_m5 = dplyr::lag(work_arr_day, n = 5),
         work_arr_day_m6 = dplyr::lag(work_arr_day, n = 6)) %>%
  relocate(person_id, travel_dow, work_arr_day_m6, work_arr_day_m5, work_arr_day_m4,
           work_arr_day_m3, work_arr_day_m2, work_arr_day_m1, work_arr_day, work_arr_day_p1, work_arr_day_p2, 
           work_arr_day_p3, work_arr_day_p4, work_arr_day_p5, work_arr_day_p6) %>%
  ungroup()

# Summary Table -----------------------------------------------------------
vmt_weekly_for_model_covariates <- vmt_weekly_for_model %>%
  select(num_remote_days, num_hybrid_or_remote, intersection_den)

stargazer(as.data.frame(vmt_weekly_for_model))

# Full Model  ------------------------------------------------------------
ols_model1 <- lm(ln_weekly_vmt ~ work_arr + survey_year:work_arr + age_group + 
                   gender + employment + race + income_detailed + num_kids + 
                   gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   survey_year:num_hybrid_or_remote + work_arr:survey_year +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                   data = vmt_weekly_for_model, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

stargazer::stargazer(ols_model1, type = "text", single.row = T)
stargazer::stargazer(ols_model1, type = "latex", single.row = T)
stargazer::stargazer(ols_model1, type = "html", out = "./outputs/ols_model1.htm")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))


ols_model1_pred <- data.frame(person_id = vmt_weekly_for_model$person_id, weekly_vmt_pred = exp(predict(ols_model1)))
ols_model1_pred_obs <- vmt_weekly_for_model %>%
  select(person_id, person_weight, work_arr, weekly_vmt) %>%
  rename(weekly_vmt_obs = weekly_vmt) %>%
  inner_join(ols_model1_pred) %>%
  filter(work_arr == "Always In-Person") %>%
  mutate(diff = weekly_vmt_pred - weekly_vmt_obs, 
         abs_diff = abs(weekly_vmt_pred - weekly_vmt_obs),
         pct_diff = (weekly_vmt_pred - weekly_vmt_obs) / weekly_vmt_obs,
         abs_pct_diff = abs(pct_diff))


sqrt(weighted.mean((ols_model1_pred_obs$diff)^2, w = ols_model1_pred_obs$person_weight))


# Non-Work VMT ------------------------------------------------------------
ols_model2 <- lm(ln_nonwork_tour_vmt_weekly ~ work_arr + survey_year:work_arr + age_group + 
                   gender + employment + race + income_detailed + num_kids + 
                   gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   survey_year:num_hybrid_or_remote + work_arr:survey_year +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

stargazer::stargazer(ols_model2, type = "text", single.row = T)
stargazer::stargazer(ols_model2, type = "latex", single.row = T)

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model2)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model2)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))

## Some Graphs ------------------------------------------------------------
vmt_weekly_for_model %>%
  group_by(work_arr) %>%
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_weekly, w = person_weight, na.rm = T)) %>%
  mutate(mean_nonwork_vmt_daily = mean_nonwork_vmt /7)

vmt_daily %>% group_by(travel_dow) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = travel_dow, y = mean_nonwork_vmt)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by DOW",
       x = "Day of Week",
       y = "Non-Work Tour VMT")

vmt_daily %>% group_by(work_arr_day) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr_day, y = mean_nonwork_vmt)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))

vmt_daily %>% group_by(work_arr, work_arr_day) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr, y = mean_nonwork_vmt, fill = work_arr_day)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))

vmt_daily %>% group_by(work_arr, work_arr_day2) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr, y = mean_nonwork_vmt, fill = work_arr_day2)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))

test <- vmt_daily %>%
  filter(work_arr == "Always In-Person" & work_arr_day == "remote_day") %>%
  relocate(person_id, travel_dow, work_arr, work_arr_day, telework_time)

# OPSR (MSP data with Wang/Mokhtraian specification) -------------------------------------
opsr_model1_rep_eq <- work_arr | ln_weekly_vmt ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed |
  # Factors affecting always in-person VMT
  ## Employment status
  employed_part_time + self_employed + 
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Kids
  kids_1plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus |  
  # Factors affecting always remote VMT
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
opsr_model1_rep <- opsr(opsr_model1_rep_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
opsr_model1_rep_te <- opsr_te(opsr_model1_rep, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)
## Inference
summary(opsr_model1_rep)
texreg(opsr_model1_rep, single.row = T)
print(opsr_model1_rep_te)

predict(opsr_model1_rep, group = 1, type = "unlog-response")

## Calculate average treatment effect on treated
ar_vmt_observed <- weighted.mean(predict(opsr_model1_rep, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_vmt_counterfact <- weighted.mean(predict(opsr_model1_rep, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_vmt_observed
ar_vmt_counterfact
ar_vmt_observed - ar_vmt_counterfact
(ar_vmt_observed - ar_vmt_counterfact) / ar_vmt_counterfact

hybrid_vmt_observed <- weighted.mean(predict(opsr_model1_rep, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_vmt_counterfact <- weighted.mean(predict(opsr_model1_rep, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_vmt_observed
hybrid_vmt_counterfact
hybrid_vmt_observed - hybrid_vmt_counterfact
(hybrid_vmt_observed - hybrid_vmt_counterfact) / hybrid_vmt_counterfact

## Calculate average treatment effect
aip_vmt_observed <-  weighted.mean(predict(opsr_model1_rep, group = 1, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_vmt_observed - aip_vmt_observed

ar_treatment_mat <- opsr_model1_rep_te$ce.by.treatments[[3]]
hybrid_treatment_mat <- opsr_model1_rep_te$ce.by.treatments[[2]] ## DF of treatment effects for hybrid
aip_control_mat <- opsr_model1_rep_te$ce.by.treatments[[1]]


## Column 1 is always in-person to hybrid (for observed AIP), column 2 is always in-person to hybrid (ATT)
hybrid_treatment <- weighted.mean(hybrid_treatment_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
aip_control <- weighted.mean(aip_control_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
ar_treatment <- weighted.mean(ar_treatment_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
ar_treatment - aip_control
(ar_treatment - aip_control) / aip_control
hybrid_treatment - aip_control
(hybrid_treatment - aip_control) / aip_control

texreg::screenreg(opsr_model1_rep, digits = 3, single.row = T)
texreg::htmlreg(opsr_model1_rep, file = "./outputs/opsr_model_rep_weekly_vmt.htm", custom.model.names = c(""))

sqrt(weighted.mean((vmt_weekly_pred_obs$diff)^2, w = vmt_weekly_pred_obs$person_weight, na.rm = T))

# Rebound Effect ----------------------------------------------------------
rebound_work_savings_eq <- work_arr | ln_work_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed |
  # Factors affecting always in-person VMT
  ## Employment status
  employed_part_time + self_employed + 
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Kids
  kids_1plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus |  
  # Factors affecting always remote VMT
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
rebound_work_savings <- opsr(rebound_work_savings_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
rebound_work_savings_te <- opsr_te(rebound_work_savings, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_work_savings)
print(rebound_work_savings_te)
rebound_work_savings_te$ce.by.treatments[[1]]


rebound_nonwork_increase_eq <- work_arr | ln_nonwork_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed |
  # Factors affecting always in-person VMT
  ## Employment status
  employed_part_time + self_employed + 
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Kids
  kids_1plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus |  
  # Factors affecting always remote VMT
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
rebound_nonwork_increase <- opsr(rebound_nonwork_increase_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
rebound_nonwork_increase_te <- opsr_te(rebound_nonwork_increase, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_nonwork_increase)

print(rebound_work_savings_te)
print(rebound_nonwork_increase_te)

ar_work_vmt_observed <- weighted.mean(predict(rebound_work_savings, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_work_vmt_counterfact <- weighted.mean(predict(rebound_work_savings, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_work_vmt_observed
ar_work_vmt_counterfact
ar_work_vmt_observed - ar_work_vmt_counterfact
(ar_work_vmt_observed - ar_work_vmt_counterfact) / ar_work_vmt_counterfact

ar_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_observed
ar_nonwork_vmt_counterfact
ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact

hybrid_work_vmt_observed <- weighted.mean(predict(rebound_work_savings, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_counterfact <- weighted.mean(predict(rebound_work_savings, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_observed
hybrid_work_vmt_counterfact
hybrid_work_vmt_observed - hybrid_work_vmt_counterfact
(hybrid_work_vmt_observed - hybrid_work_vmt_counterfact) / hybrid_work_vmt_counterfact

hybrid_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_observed
hybrid_nonwork_vmt_counterfact
hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact
(hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact) / hybrid_nonwork_vmt_counterfact

# Displacement Effect -----------------------------------------------------
displacement_model_eq <- work_arr_day2 | ln_nonwork_tour_vmt_daily ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Day
  monday + tuesday + wednesday + thursday + friday + saturday +
  ## Year
  year2021 + year2023 +
  ## Duration
  #minutes_to_work +
  ## Interactions
  year2021:self_employed |
  # Factors affecting in-person day VMT
  ## Employment status
  employed_part_time + self_employed + 
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Kids
  kids_1plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus | 
  # Factors affecting remote day VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus |  
  # Factors affecting no work day VMT
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus
  

## Estimation
displacement_model <- opsr(displacement_model_eq, vmt_daily_model_mat, printLevel = 0)
## Treatment effects 
displacement_model_te <- opsr_te(displacement_model, type = "unlog-response", weights = vmt_daily_model_mat$person_weight)
## Inference
summary(displacement_model)
texreg(displacement_model, single.row = T)
print(displacement_model_te)

ar_nonwork_vmt_observed <- weighted.mean(predict(displacement_model, group = 3, type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_counterfact <- weighted.mean(predict(displacement_model, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_observed
ar_nonwork_vmt_counterfact

hybrid_nonwork_vmt_observed <- weighted.mean(predict(displacement_model, group = 2, type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_counterfact <- weighted.mean(predict(displacement_model, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_observed
hybrid_nonwork_vmt_counterfact


# Displacement Model V2 ---------------------------------------------------
displacement_model_v2 <- lm(ln_nonwork_tour_vmt_daily ~ work_arr_day + 
                              work_arr_day_p1 + work_arr_day_m1 + 
                              # work_arr_day_p2 + work_arr_day_m2 +
                              # work_arr_day_p3 + work_arr_day_m3 + 
                              # work_arr_day_p4 + work_arr_day_m4 +
                              # work_arr_day_p5 + work_arr_day_m5 +
                              # work_arr_day_p6 + work_arr_day_m6 +
                              survey_year:work_arr + age_group + 
                   gender + employment + race + income_detailed + num_kids + 
                   gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   survey_year:num_hybrid_or_remote + work_arr:survey_year +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_daily_disp, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

stargazer::stargazer(displacement_model_v2, type = "text", single.row = T)

# OPSR Replication --------------------------------------------------------
data(telework_data)

f <-
  twing_status | vmd_ln ~
 edu_2 + edu_3 + hhincome_2 + hhincome_3 + flex_work + work_fulltime |
   female + age_mean + age_mean_sq + race_black + race_other + vehicle +
   suburban + smalltown + rural + work_fulltime + att_prolargehouse +
   att_procarowning + region_waa |
   edu_2 + edu_3 + suburban + smalltown + rural + work_fulltime |
   female + hhincome_2 + hhincome_3 + child + suburban + smalltown +
   rural 
start_default <- opsr(f, telework_data, .get2step = TRUE)
fit <- opsr(f, telework_data, start = start_default, method = "NM", iterlim = 50e3, printLevel = 0)
summary(fit)
print(opsr_te(fit, type = "unlog-response", weights = telework_data$weight))
print(opsr_te(fit, type = "response", weights = telework_data$weight))

## ATT
exp(-0.11) - 1
exp(-1.13) - 1

telework_data$twing_status <- factor(telework_data$twing_status)
ols_replication <- lm(vmd_ln ~ twing_status + edu_2 + edu_3 + hhincome_2 + hhincome_3 + 
                        flex_work + work_fulltime + twing_feasibility + female +
                        age_mean + age_mean_sq + race_black + race_other + vehicle +
                        suburban + smalltown + rural + work_fulltime + region_waa, 
                      data = telework_data, weights = weight) 

summary(ols_replication)
exp(0.13) - 1 ## -4% from always in-person to hybrid
exp(-0.933) - 1 ## -45% from always in-person to always remote

telework_data %>%
  group_by(twing_status) %>%
  summarize(mean_vmd = weighted.mean(vmd, weight))

(143 - 138) / 138
(63.2 - 138) / 138
