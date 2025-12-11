### Statistical Analysis
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/statistical_analysis")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid",
                     "stargazer", "gtable", "broom", "writexl", "openxlsx",
                     "stringi", "nnet", "plm", "forcats", "OPSR", "texreg", "plm")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)

# Read in Data ------------------------------------------------------------
vmt_files <- readRDS("../datacleaning/outputs/vmt_files.RData")
vmt_weekly_df <- vmt_files[["vmt_weekly_df"]]
vmt_daily_df <- vmt_files[["vmt_daily_df"]]
vmt_weekly_model_mat <- vmt_files[["vmt_weekly_model_mat"]]
vmt_daily_model_mat <- vmt_files[["vmt_daily_model_mat"]]
vmt_displacement <- vmt_files[["vmt_displacement"]]
vmt_displacement_mat <- vmt_files[["vmt_displacement_mat"]]


# Relevel -----------------------------------------------------------------
vmt_weekly_df$age_group <- relevel(vmt_weekly_df$age_group, ref = "35 to 54")
vmt_weekly_df$income_detailed <- factor(vmt_weekly_df$income_detailed, 
                                               c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
vmt_weekly_df$income_detailed <- relevel(vmt_weekly_df$income_detailed, ref = "$50-$100K")

# Categorical Data --------------------------------------------------------
categorical_vars <- vmt_weekly_df %>%
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


# Summary Table -----------------------------------------------------------
vmt_weekly_df_covariates <- vmt_weekly_df %>%
  select(num_remote_days, num_hybrid_or_remote, intersection_den)

stargazer(as.data.frame(vmt_weekly_df))

# Full Model  ------------------------------------------------------------
ols_model1 <- lm(ln_vmt_weekly ~ work_arr + survey_year:work_arr + age_group + 
                   gender + employment + race + income_detailed + num_kids + 
                   gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   survey_year:num_hybrid_or_remote + work_arr:survey_year +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                   data = vmt_weekly_df, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

stargazer::stargazer(ols_model1, type = "text", single.row = T)
stargazer::stargazer(ols_model1, type = "latex", single.row = T)
stargazer::stargazer(ols_model1, type = "html", out = "./outputs/ols_model1.htm")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))


ols_model1_pred <- data.frame(person_id = vmt_weekly_df$person_id, vmt_weekly_pred = exp(predict(ols_model1)))
ols_model1_pred_obs <- vmt_weekly_df %>%
  select(person_id, person_weight, work_arr, vmt_weekly) %>%
  rename(vmt_weekly_obs = vmt_weekly) %>%
  inner_join(ols_model1_pred) %>%
  filter(work_arr == "Always In-Person") %>%
  mutate(diff = vmt_weekly_pred - vmt_weekly_obs, 
         abs_diff = abs(vmt_weekly_pred - vmt_weekly_obs),
         pct_diff = (vmt_weekly_pred - vmt_weekly_obs) / vmt_weekly_obs,
         abs_pct_diff = abs(pct_diff))


sqrt(weighted.mean((ols_model1_pred_obs$diff)^2, w = ols_model1_pred_obs$person_weight))


# Non-Work VMT ------------------------------------------------------------
ols_model2 <- lm(ln_nonwork_tour_vmt_weekly ~ work_arr + survey_year:work_arr + age_group + 
                   gender + employment + race + income_detailed + num_kids + 
                   gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   survey_year:num_hybrid_or_remote + work_arr:survey_year +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_df, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

stargazer::stargazer(ols_model2, type = "text", single.row = T)
stargazer::stargazer(ols_model2, type = "latex", single.row = T)

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model2)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model2)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))

## Some Graphs ------------------------------------------------------------
vmt_weekly_df %>%
  group_by(work_arr) %>%
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_weekly, w = person_weight, na.rm = T)) %>%
  mutate(mean_nonwork_vmt_daily = mean_nonwork_vmt /7)

vmt_daily_df %>% group_by(travel_dow) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = travel_dow, y = mean_nonwork_vmt)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by DOW",
       x = "Day of Week",
       y = "Non-Work Tour VMT")

vmt_daily_df %>% group_by(work_arr_day) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr_day, y = mean_nonwork_vmt)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))

vmt_daily_df %>% group_by(work_arr, work_arr_dayH) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr, y = mean_nonwork_vmt, fill = work_arr_dayH)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))

vmt_daily_df %>% group_by(work_arr, work_arr_day) %>% 
  summarize(mean_nonwork_vmt = weighted.mean(nonwork_tour_vmt_daily, w = person_weight, na.rm = T)) %>% 
  ggplot(aes(x = work_arr, y = mean_nonwork_vmt, fill = work_arr_day)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Non-Work Tour VMT by Work Day Type",
       x = "Work Day Type",
       y = "Non-Work Tour VMT") +
  geom_text(aes(label = round(mean_nonwork_vmt, 2)), size = 4, vjust = -0.25, position = position_dodge(.9))


# OPSR (MSP data with Wang/Mokhtraian specification) -------------------------------------
opsr_model1_rep_eq <- work_arr | ln_vmt_weekly ~ 
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
texreg::htmlreg(opsr_model1_rep, file = "./outputs/opsr_model_rep_vmt_weekly.htm", custom.model.names = c(""))


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

ar_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_observed
ar_nonwork_vmt_counterfact
ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact

### Work VMT Savings / Non-Work VMT Increase
(ar_work_vmt_observed - ar_work_vmt_counterfact) / (ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact)
((ar_work_vmt_observed - ar_work_vmt_counterfact) / ar_work_vmt_counterfact) / ((ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact) / ar_nonwork_vmt_counterfact)
## Work savings > increase in non-work VMT

hybrid_work_vmt_observed <- weighted.mean(predict(rebound_work_savings, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_counterfact <- weighted.mean(predict(rebound_work_savings, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_observed
hybrid_work_vmt_counterfact
hybrid_work_vmt_observed - hybrid_work_vmt_counterfact

hybrid_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_observed
hybrid_nonwork_vmt_counterfact
hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact

### Work VMT Savings / Non-work VMT Increase
(hybrid_work_vmt_observed - hybrid_work_vmt_counterfact) / (hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact)
## Work savings > increase in non-work VMT

# Displacement Model ---------------------------------------------------
## Model 1 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement.
disp_model_v1 <- lm(ln_nonwork_tour_vmt_today ~ work_arr_day +
                              + age_group + travel_dow +
                              gender + employment + race + income_detailed + num_kids + 
                              gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                              survey_year:num_hybrid_or_remote + 
                              jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                            data = vmt_displacement, weights = person_weight) 

stargazer::stargazer(disp_model_v1, type = "text", single.row = T)

coeffs <- tidy(disp_model_v1)
stats <- glance(disp_model_v1)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)

# Create a new workbook
wb <- createWorkbook()

# Add the regression results to a sheet
addWorksheet(wb, "model1_ols")
writeData(wb, "model1_ols", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model1_ols", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 

## Model 2 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days.
disp_model_v2 <- lm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                      n_other_no_work_days + ln_nonwork_tour_vmt_other_all_days +
                      + age_group + travel_dow + 
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement, weights = person_weight) 

stargazer::stargazer(disp_model_v2, type = "text", single.row = T)

coeffs <- tidy(disp_model_v2)
stats <- glance(disp_model_v2)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)

print(paste0("On average, no work days are associated with a ", round(100 * (exp(summary(disp_model_v2)$coefficients[2, 1]) - 1), 1), "% difference in daily VMT relative to in-person days."))
print(paste0("On average, remote days are associated with a ", round(100 * (exp(summary(disp_model_v2)$coefficients[3, 1]) - 1), 1), "% difference in daily VMT relative to in-person days."))

# Add the regression results to a sheet
addWorksheet(wb, "model2_ols")
writeData(wb, "model2_ols", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model2_ols", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 


# Model 2A: Model 2 but just the hybrid workers ---------------------------
disp_model_v2a <- lm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                      n_other_no_work_days + 
                      + age_group + travel_dow + 
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v2a, type = "text", single.row = T)

# Model 3 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days. Fixed effects.
disp_model_v3 <- plm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                     n_other_no_work_days + ln_nonwork_tour_vmt_other_all_days +
                     age_group + travel_dow + gender + employment + race + 
                     income_detailed + num_kids + gender:num_kids + education + 
                     num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                     jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                     survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v3, type = "text", single.row = T)

coeffs <- tidy(disp_model_v3)
stats <- glance(disp_model_v3)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)

# Add the regression results to a sheet
addWorksheet(wb, "model3_fe")
writeData(wb, "model3_fe", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model3_fe", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 

hybrid_disp <- vmt_displacement[vmt_displacement$work_arr == "Hybrid",]
hybrid_disp %>% select(person_id, work_arr_day, n_other_no_work_days) %>%
  count(work_arr_day, n_other_no_work_days)

hybrid_disp %>% select(person_id, work_arr_day, n_other_remote_days) %>%
  count(work_arr_day, n_other_remote_days)


# Model 4 -----------------------------------------------------------------
## LHS: Non-work VMT on all other days as a function of work arrangement, number of other 
## remote days, non-work VMT today, and number of other no work days. Fixed effects.
disp_model_v4 <- plm(ln_nonwork_tour_vmt_other_all_days ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v4, type = "text", single.row = T)

coeffs <- tidy(disp_model_v4)
stats <- glance(disp_model_v4)

# Model 5 -----------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days. Fixed effects.
disp_model_v5 <- plm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v5, type = "text", single.row = T)

coeffs <- tidy(disp_model_v5)
stats <- glance(disp_model_v5)

# Model 6 -----------------------------------------------------------------
## LHS: Non-work VMT on all other days as a function of work arrangement, number of other 
## remote days, non-work VMT today, and number of other no work days. Fixed effects.
disp_model_v6 <- plm(exp(ln_nonwork_tour_vmt_other_all_days) ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v6, type = "text", single.row = T)

coeffs <- tidy(disp_model_v6)
stats <- glance(disp_model_v6)

# Model 7 -----------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days. Fixed effects.
disp_model_v7 <- plm(nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v7, type = "text", single.row = T)

coeffs <- tidy(disp_model_v7)
stats <- glance(disp_model_v7)

# Model 8 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement.
disp_model_v8 <- lm(nonwork_tour_vmt_today ~ work_arr_day +
                      + age_group + travel_dow +
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement[vmt_displacement$work_arr == "Hybrid",], weights = person_weight) 

stargazer::stargazer(disp_model_v8, type = "text", single.row = T)

coeffs <- tidy(disp_model_v8)
stats <- glance(disp_model_v8)

# Model 9 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days.
disp_model_v9 <- lm(exp(ln_nonwork_tour_vmt_other_all_days) ~ work_arr_day +
                      + age_group + travel_dow + 
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement, weights = person_weight) 

stargazer::stargazer(disp_model_v9, type = "text", single.row = T)

coeffs <- tidy(disp_model_v9)
stats <- glance(disp_model_v9)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)


# Model 10 -------------------------------------------------------------
## LHS: Non-work VMT today as a function of work arrangement, number of other 
## remote days, and number of other no work days. OPSR. All days.
disp_model_v10_opsr_eq <- work_arr_day | ln_nonwork_tour_vmt_today ~ 
  # Factors that affect work arrangement today
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Commute length
  minutes_to_work + 
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting in-person day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days + 
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
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting remote day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days  +
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting no work day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days  +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday

## Estimation
disp_model_v10_opsr <- opsr(disp_model_v10_opsr_eq, vmt_displacement_mat, weights = vmt_displacement_mat$person_weight, printLevel = 0)
summary(disp_model_v10_opsr)

## Treatment effects 
disp_model_v10_te <- opsr_te(disp_model_v10_opsr, type = "unlog-response")
print(disp_model_v10_te)

## Model 11 -------------------------------------------------------------
## LHS: Non-work VMT all other days as a function of work arrangement, number of other 
## remote days, and number of other no work days. OPSR. All days.
disp_model_v11_opsr_eq <- work_arr_day | ln_nonwork_tour_vmt_other_all_days ~ 
  # Factors that affect work arrangement today
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Commute length
  minutes_to_work + 
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting in-person day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days + 
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
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting remote day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days  +
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday |
  # Factors affecting no work day VMT
  ## Other days' VMT and remote/non-work days 
  n_other_remote_days +
  n_other_no_work_days  +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday

## Estimation
disp_model_v11_opsr <- opsr(disp_model_v11_opsr_eq, vmt_displacement_mat, weights = vmt_displacement_mat$person_weight, printLevel = 0)
summary(disp_model_v11_opsr)

## Treatment effects 
disp_model_v11_te <- opsr_te(disp_model_v11_opsr, type = "unlog-response")
print(disp_model_v11_te)


# Daily Rebound Effects ---------------------------------------------------
vmt_daily_model_mat$work_arr_day <- factor(vmt_daily_model_mat$work_arr_day, levels = c(1, 2, 3))
vmt_daily_model_mat$in_person_day <- ifelse(vmt_daily_model_mat$work_arr_day == 1, 1, 0)
vmt_daily_model_mat$remote_day <- ifelse(vmt_daily_model_mat$work_arr_day == 2, 1, 0)
vmt_daily_model_mat$no_work_day  <- ifelse(vmt_daily_model_mat$work_arr_day == 3, 1, 0)

rebound_work_savings_daily_eq <- work_arr | ln_work_tour_vmt_daily ~ 
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
  ## DOW
  travel_dow + 
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
  ## DOW
  travel_dow + 
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
  ## DOW
  travel_dow + 
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
rebound_work_savings_daily <- opsr(rebound_work_savings_daily_eq, vmt_daily_model_mat, printLevel = 0)
summary(rebound_work_savings_daily)

## Treatment effects 
rebound_work_savings_daily_te <- opsr_te(rebound_work_savings_daily, type = "unlog-response", weights = vmt_daily_model_mat$person_weight)
print(rebound_work_savings_daily_te)


rebound_nonwork_increase_daily_eq <- work_arr | ln_nonwork_tour_vmt_daily ~ 
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
  ## Daily work arrangement
  remote_day + no_work_day + 
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
  ## DOW
  travel_dow + 
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Daily work arrangement
  remote_day + no_work_day + 
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## DOW
  travel_dow + 
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus |  
  # Factors affecting always remote VMT
  ## Daily work arrangement
  no_work_day + 
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Gender
  male +
  ## Built Environment 
  jobs_per_hh + intersection_den + emp8_ent +
  ## Kids
  kids_1plus +
  ## DOW
  travel_dow + 
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
rebound_nonwork_increase_daily <- opsr(rebound_nonwork_increase_daily_eq, vmt_daily_model_mat, printLevel = 0)
summary(rebound_nonwork_increase_daily)

## Treatment effects 
rebound_nonwork_increase_daily_te <- opsr_te(rebound_nonwork_increase_daily, type = "unlog-response", weights = vmt_daily_model_mat$person_weight)
print(rebound_nonwork_increase_daily_te)

print(rebound_work_savings_daily_te)
print(rebound_nonwork_increase_daily_te)

nonwork_day_work_vmt_observed <- weighted.mean(predict(rebound_work_savings_daily, group = 3, type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
nonwork_day_vmt_counterfact <- weighted.mean(predict(rebound_work_savings_daily, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
nonwork_day_observed
nonwork_day_counterfact
nonwork_day_observed - nonwork_day_vmt_counterfact

nonwork_day_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 3, type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
nonwork_day_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_daily_model_mat$person_weight, na.rm = T)
nonwork_day_nonwork_vmt_observed
nonwork_day_nonwork_vmt_counterfact
nonwork_day_nonwork_vmt_observed - nonwork_day_nonwork_vmt_counterfact

### Work VMT Savings / Non-Work VMT Increase
(ar_work_vmt_observed - ar_work_vmt_counterfact) / (ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact)
((ar_work_vmt_observed - ar_work_vmt_counterfact) / ar_work_vmt_counterfact) / ((ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact) / ar_nonwork_vmt_counterfact)
## Work savings > increase in non-work VMT

in_person_day_work_vmt_observed <- weighted.mean(predict(rebound_work_savings, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
in_person_day_work_vmt_counterfact <- weighted.mean(predict(rebound_work_savings, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
in_person_day_work_vmt_observed
in_person_day_work_vmt_counterfact
in_person_day_work_vmt_observed - in_person_day_work_vmt_counterfact

in_person_day_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
in_person_day_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
in_person_day_nonwork_vmt_observed
in_person_day_nonwork_vmt_counterfact
in_person_day_nonwork_vmt_observed - in_person_day_nonwork_vmt_counterfact

### Work VMT Savings / Non-work VMT Increase
(hybrid_work_vmt_observed - hybrid_work_vmt_counterfact) / (hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact)
## Work savings > increase in non-work VMT

#############################
model_summary <- summary(disp_model_v4)$coefficients

model_stats_df <- data.frame(
  R_squared = model_summary$r.squared,
  Adj_R_squared = model_summary$adj.r.squared,
  F_statistic = model_summary$fstatistic[1],
  df1 = model_summary$fstatistic[2],
  df2 = model_summary$fstatistic[3],
  p_value = pf(model_summary$fstatistic[1], model_summary$fstatistic[2],
               model_summary$fstatistic[3], lower.tail = FALSE)
)

coeffs <- extract(disp_model_v4)
stats <- glance(disp_model_v4)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)

# Add the regression results to a sheet
addWorksheet(wb, "model4_opsr")
writeData(wb, "model4_opsr", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model4_opsr", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 

## Save the workbook to an Excel file -----------------------------------------
saveWorkbook(wb, "./regressions_comparison/displacement.xlsx", overwrite = TRUE)


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
