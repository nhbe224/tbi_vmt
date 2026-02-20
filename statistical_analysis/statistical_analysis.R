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
                   gender:num_kids:year2021 + education + num_vehicles + num_hybrid_or_remote +
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

vmt_weekly_model_mat$remote_day_pct_100 = vmt_weekly_model_mat$remote_day_pct * 100

# OPSR Model 1  -------------------------------------------------
opsr_model1_rep_eq <- work_arr | ln_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Education
  no_bach_plus + 
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |
  # Factors affecting always in-person VMT
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Employment status
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed +
  year2021:vehicle1 + year2023:vehicle1 +
  year2021:vehicle2plus + year2023:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment level
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Percentage of days worked remote
  remote_day_pct_100 +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |  
  # Factors affecting always remote VMT
  ## Gender
  male +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Kids
  kids_1plus +
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  male:kids_1plus:year2021 + 
  male:kids_1plus:year2023

## Estimation
opsr_model1_rep <- opsr(opsr_model1_rep_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
opsr_model1_rep_te <- opsr_te(opsr_model1_rep, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)
## Inference
summary(opsr_model1_rep)
texreg(opsr_model1_rep, single.row = T)
print(opsr_model1_rep_te)
screenreg(opsr_model1_rep, single.row = T)


predict(opsr_model1_rep, group = 1, type = "unlog-response")


## Confidence intervals
rho1 <- c(-0.037789 -1.96*(0.145700), -0.037789 + 1.96*(0.145700))
print(rho1)
rho2 <- c(0.297488 - 1.96*(0.219460), 0.297488 + 1.96*(0.219460))
print(rho2)
rho3 <- c(0.131235 - 1.96*(0.343881), 0.131235 + 1.96*(0.343881))
print(rho3)

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

# OPSR Model (2019)  -------------------------------------------------
opsr_model2_rep_eq <- work_arr | ln_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Other hybrid/remote workers
  num_hybrid_or_remote + 
  male:kids_1plus |
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
  ## Interactions
  male:kids_1plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  male:kids_1plus |  
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
  ## Interactions
  male:kids_1plus

## Estimation
opsr_model2_rep <- opsr(opsr_model2_rep_eq, vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ], printLevel = 0)
## Treatment effects 
opsr_model2_rep_te <- opsr_te(opsr_model2_rep, type = "unlog-response", weights = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight)
## Inference
summary(opsr_model2_rep)
texreg(opsr_model2_rep, single.row = T)
print(opsr_model2_rep_te)

predict(opsr_model2_rep, group = 1, type = "unlog-response")

## Calculate average treatment effect on treated
ar_vmt_observed <- weighted.mean(predict(opsr_model2_rep, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, na.rm = T)
ar_vmt_counterfact <- weighted.mean(predict(opsr_model2_rep, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, na.rm = T)
ar_vmt_observed
ar_vmt_counterfact
ar_vmt_observed - ar_vmt_counterfact
(ar_vmt_observed - ar_vmt_counterfact) / ar_vmt_counterfact

hybrid_vmt_observed <- weighted.mean(predict(opsr_model2_rep, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, na.rm = T)
hybrid_vmt_counterfact <- weighted.mean(predict(opsr_model2_rep, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, na.rm = T)
hybrid_vmt_observed
hybrid_vmt_counterfact
hybrid_vmt_observed - hybrid_vmt_counterfact
(hybrid_vmt_observed - hybrid_vmt_counterfact) / hybrid_vmt_counterfact

## Calculate average treatment effect
aip_vmt_observed <-  weighted.mean(predict(opsr_model2_rep, group = 1, type = "unlog-response"), w = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, na.rm = T)
ar_vmt_observed - aip_vmt_observed

ar_treatment_mat <- opsr_model2_rep_te$ce.by.treatments[[3]]
hybrid_treatment_mat <- opsr_model2_rep_te$ce.by.treatments[[2]] ## DF of treatment effects for hybrid
aip_control_mat <- opsr_model2_rep_te$ce.by.treatments[[1]]


## Column 1 is always in-person to hybrid (for observed AIP), column 2 is always in-person to hybrid (ATT)
hybrid_treatment <- weighted.mean(hybrid_treatment_mat, w = rep(vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, 3), na.rm = T)
aip_control <- weighted.mean(aip_control_mat, w = rep(vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, 3), na.rm = T)
ar_treatment <- weighted.mean(ar_treatment_mat, w = rep(vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2019, ]$person_weight, 3), na.rm = T)
ar_treatment - aip_control
(ar_treatment - aip_control) / aip_control
hybrid_treatment - aip_control
(hybrid_treatment - aip_control) / aip_control

texreg::screenreg(opsr_model2_rep, digits = 3, single.row = T)

# OPSR Model (2021)  -------------------------------------------------
opsr_model3_rep_eq <- work_arr | ln_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Other hybrid/remote workers
  num_hybrid_or_remote + 
  male:kids_1plus |
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
  ## Interactions
  male:kids_1plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  male:kids_1plus |  
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
  ## Interactions
  male:kids_1plus

## Estimation
opsr_model3_rep <- opsr(opsr_model3_rep_eq, vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2021, ], printLevel = 0)
## Treatment effects 
opsr_model3_rep_te <- opsr_te(opsr_model3_rep, type = "unlog-response", weights = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2021, ]$person_weight)
## Inference
summary(opsr_model3_rep)
texreg(opsr_model3_rep, single.row = T)
print(opsr_model3_rep_te)

predict(opsr_model3_rep, group = 1, type = "unlog-response")
texreg::screenreg(opsr_model3_rep, digits = 3, single.row = T)

# OPSR Model (2023)  -------------------------------------------------
opsr_model4_rep_eq <- work_arr | ln_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Other hybrid/remote workers
  num_hybrid_or_remote + 
  male:kids_1plus |
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
  ## Interactions
  male:kids_1plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Percentage of days worked remote
  remote_day_pct +
  male:kids_1plus |  
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
  ## Interactions
  male:kids_1plus

## Estimation
opsr_model4_rep <- opsr(opsr_model4_rep_eq, vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2023, ], printLevel = 0)
## Treatment effects 
opsr_model4_rep_te <- opsr_te(opsr_model4_rep, type = "unlog-response", weights = vmt_weekly_model_mat[vmt_weekly_model_mat[, "survey_year"] == 2023, ]$person_weight)
## Inference
summary(opsr_model4_rep)
texreg(opsr_model4_rep, single.row = T)
print(opsr_model4_rep_te)

predict(opsr_model4_rep, group = 1, type = "unlog-response")

texreg::screenreg(opsr_model4_rep, digits = 3, single.row = T)

## Check treatment effects for each model
print(opsr_model1_rep_te)
print(opsr_model2_rep_te) # 2019
print(opsr_model3_rep_te) # 2021
print(opsr_model4_rep_te) # 2023



# Rebound Effect ----------------------------------------------------------
## Decrease in Work VMT ----------------------
rebound_work_decrease_eq <- work_arr | ln_work_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Education
  no_bach_plus + 
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |
  # Factors affecting always in-person VMT
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Employment status
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed +
  year2021:vehicle1 + year2023:vehicle1 +
  year2021:vehicle2plus + year2023:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment level
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Percentage of days worked remote
  remote_day_pct_100 +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |  
  # Factors affecting always remote VMT
  ## Gender
  male +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Kids
  kids_1plus +
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  male:kids_1plus:year2021 + 
  male:kids_1plus:year2023

## Estimation
rebound_work_decrease <- opsr(rebound_work_decrease_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
rebound_work_decrease_te <- opsr_te(rebound_work_decrease, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_work_decrease)
print(rebound_work_decrease_te)
rebound_work_decrease_te$ce.by.treatments[[1]]
texreg(rebound_work_decrease, single.row = T)

## Increase in Non-Work VMT --------------------
rebound_nonwork_increase_eq <- work_arr | ln_nonwork_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Education
  no_bach_plus + 
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |
  # Factors affecting always in-person VMT
  ## Gender
  male +
  ## Age
  age18to34 + age55plus +
  ## Race
  nonWhite +
  ## Number of vehicles
  vehicle1 + vehicle2plus +
  ## Employment status
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed +
  year2021:vehicle1 + year2023:vehicle1 +
  year2021:vehicle2plus + year2023:vehicle2plus | 
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment level
  employed_part_time + self_employed + 
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Percentage of days worked remote
  remote_day_pct_100 +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + year2023:self_employed |  
  # Factors affecting always remote VMT
  ## Gender
  male +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Kids
  kids_1plus +
  ## Other hybrid/remote workers
  num_hybrid_or_remote +
  ## Built Environment 
  ln_jobs_per_hh + ln_intersection_den + emp8_ent +
  ## Year
  year2021 + year2023 +
  ## Interactions
  male:kids_1plus:year2021 + 
  male:kids_1plus:year2023

## Estimation
rebound_nonwork_increase <- opsr(rebound_nonwork_increase_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
rebound_nonwork_increase_te <- opsr_te(rebound_nonwork_increase, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_nonwork_increase)
texreg(rebound_nonwork_increase, single.row = T)

print(rebound_nonwork_increase_te)
print(rebound_work_decrease_te)

ar_work_vmt_observed <- weighted.mean(predict(rebound_work_decrease, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_work_vmt_counterfact <- weighted.mean(predict(rebound_work_decrease, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_work_vmt_observed
ar_work_vmt_counterfact
ar_work_vmt_observed - ar_work_vmt_counterfact

ar_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_observed
ar_nonwork_vmt_counterfact
ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact

### Work VMT decrease / Non-Work VMT Increase
abs((ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact) / (ar_work_vmt_observed - ar_work_vmt_counterfact))
## Work decrease > increase in non-work VMT

hybrid_work_vmt_observed <- weighted.mean(predict(rebound_work_decrease, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_counterfact <- weighted.mean(predict(rebound_work_decrease, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_work_vmt_observed
hybrid_work_vmt_counterfact
hybrid_work_vmt_observed - hybrid_work_vmt_counterfact

hybrid_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_nonwork_vmt_observed
hybrid_nonwork_vmt_counterfact
hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact

### Work VMT decrease / Non-work VMT Increase
abs((hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact) / (hybrid_work_vmt_observed - hybrid_work_vmt_counterfact))


# Displacement Model ---------------------------------------------------
## Model 1 -------------------------------------------------------------
## Model type: OLS. 
## Specification: Non-work VMT today as a function of work arrangement.
## Workers: All.
disp_model_v1 <- lm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days
                              + n_other_no_work_days +
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


## Model 1h ----------------------------------------------------------------
## Model type: OLS. 
## Specification: Non-work VMT today as a function of work arrangement.
## Workers: Hybrid
disp_model_v1h <- lm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days
                     + n_other_no_work_days +
                      + age_group + travel_dow +
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement[vmt_displacement$work_arr == "Hybrid", ], weights = person_weight) 

stargazer::stargazer(disp_model_v1h, type = "text", single.row = T)

coeffs <- tidy(disp_model_v1h)
stats <- glance(disp_model_v1h)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)


# Add the regression results to a sheet
addWorksheet(wb, "model1h_ols")
writeData(wb, "model1h_ols", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model1h_ols", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 


## Model 2 -------------------------------------------------------------
## Model type: OLS. 
## Specification: Non-work VMT on other days as a function of today's work arrangement.
## Workers: All
disp_model_v2 <- lm(ln_nonwork_tour_vmt_other_all_days ~ work_arr_day + n_other_remote_days +
                      n_other_no_work_days +
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


## Model 2h -------------------------------------------------------------------
## Model type: OLS. 
## Specification: Non-work VMT on other days as a function of today's work arrangement.
## Workers: Hybrid
disp_model_v2h <- lm(ln_nonwork_tour_vmt_other_all_days ~ work_arr_day + n_other_remote_days +
                      n_other_no_work_days +
                      + age_group + travel_dow + 
                      gender + employment + race + income_detailed + num_kids + 
                      gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      survey_year:num_hybrid_or_remote + 
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                    data = vmt_displacement[vmt_displacement$work_arr == "Hybrid", ], weights = person_weight) 

stargazer::stargazer(disp_model_v2h, type = "text", single.row = T)

coeffs <- tidy(disp_model_v2h)
stats <- glance(disp_model_v2h)

regression_output <- list(
  Coefficients = coeffs,
  Model_Statistics = stats
)

# Add the regression results to a sheet
addWorksheet(wb, "model2h_ols")
writeData(wb, "model2h_ols", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model2h_ols", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 


## Model 3 -----------------------------------------------------------------
## Model type: FE
## Specification: Non-work VMT today as a function of work arrangement today and on other days.
## Workers: All
disp_model_v3 <- plm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement, weights = person_weight) 

stargazer::stargazer(disp_model_v3, type = "text", single.row = T)

coeffs <- tidy(disp_model_v3)
stats <- glance(disp_model_v3)

# Add the regression results to a sheet
addWorksheet(wb, "model3_fe")
writeData(wb, "model3_fe", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model3_fe", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 


## Model 3h -----------------------------------------------------------------
## Model type: FE
## Specification: Non-work VMT today as a function of work arrangement today and on other days.
## Workers: Hybrid
disp_model_v3h <- plm(ln_nonwork_tour_vmt_today ~ work_arr_day + n_other_remote_days +
                       n_other_no_work_days +
                       age_group + travel_dow + gender + employment + race + 
                       income_detailed + num_kids + gender:num_kids + education + 
                       num_vehicles + num_hybrid_or_remote + survey_year:num_hybrid_or_remote + 
                       jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + 
                       survey_year, 
                     index ="person_id", model = "within",
                     data = vmt_displacement[vmt_displacement$work_arr == "Hybrid", ], weights = person_weight) 

stargazer::stargazer(disp_model_v3h, type = "text", single.row = T)

coeffs <- tidy(disp_model_v3h)
stats <- glance(disp_model_v3h)

# Add the regression results to a sheet
addWorksheet(wb, "model3h_fe")
writeData(wb, "model3h_fe", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model3h_fe", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 

## Model 4 -------------------------------------------------------------
## Model type: OPSR
## Specification: Non-work VMT today as a function of work arrangement today and on other days.
## Workers: All
disp_model_v4_opsr_eq <- work_arr_day | ln_nonwork_tour_vmt_today ~ 
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
  ## Number of hybrid and remote employees apart from self
  num_hybrid_or_remote +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday +
  male:kids_1plus:year2021 |
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday

## Estimation
disp_model_v4_opsr <- opsr(disp_model_v4_opsr_eq, vmt_displacement_mat, weights = vmt_displacement_mat$person_weight, printLevel = 0)
summary(disp_model_v4_opsr)

## Treatment effects 
disp_model_v4_opsr_te <- opsr_te(disp_model_v4_opsr, type = "unlog-response")
print(disp_model_v4_opsr_te)

te_summary <- summary(disp_model_v4_opsr_te)$te

## Get data for worksheet
coeffs <- summary(disp_model_v4_opsr)$coef_table
coeffs$terms <- rownames(coeffs)
rownames(coeffs) <- NULL

colnames(coeffs) <- c("Estimate", "Standard Error", "t-statistic", "p-value", "Terms")
coeffs <- coeffs %>%
  select(Terms, Estimate, `Standard Error`, `t-statistic`, `p-value`)

disp_model_v4_opsr_summary <- summary(disp_model_v4_opsr)
disp_model_v4_opsr$nObs
disp_model_v4_opsr_summary$GOF$AIC[1]
disp_model_v4_opsr_summary$GOF$BIC
disp_model_v4_opsr_summary$GOFcomponents$R2[1]
disp_model_v4_opsr_summary$df

stats <- data.frame("nObs_always_in_person" = disp_model_v4_opsr$nObs[2],
                         "nObs_hybrid" = disp_model_v4_opsr$nObs[3],
                         "nObs_always_remote" = disp_model_v4_opsr$nObs[4],
                         "AIC" = disp_model_v4_opsr_summary$GOF$AIC[1],
                         "BIC" = disp_model_v4_opsr_summary$GOF$BIC,
                         "r-squared" = disp_model_v4_opsr_summary$GOFcomponents$R2[1])
rownames(stats) = NULL


# Add the regression results to a sheet
addWorksheet(wb, "model4_opsr")
writeData(wb, "model4_opsr", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model4_opsr", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 

writeData(wb, "model4_opsr", te_summary, startCol = 1, startRow = nrow(coeffs) + nrow(stats) + 6, rowNames = TRUE) 

## Model 4h -------------------------------------------------------------
## Model type: OPSR
## Specification: Non-work VMT today as a function of work arrangement today and on other days.
## Workers: Hybrid
disp_model_v4h_opsr_eq <- work_arr_day | ln_nonwork_tour_vmt_today ~ 
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
  ## Number of hybrid and remote employees apart from self
  num_hybrid_or_remote +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday +
  male:kids_1plus:year2021 |
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday

## Estimation
disp_model_v4h_opsr <- opsr(disp_model_v4h_opsr_eq, vmt_displacement_mat[vmt_displacement_mat$work_arr == 2, ], weights = vmt_displacement_mat[vmt_displacement_mat$work_arr == 2, ]$person_weight, printLevel = 0)
summary(disp_model_v4h_opsr)

## Treatment effects 
disp_model_v4h_opsr_te <- opsr_te(disp_model_v4h_opsr, type = "unlog-response")
print(disp_model_v4h_opsr_te)

## Get data for worksheet
te_summary <- summary(disp_model_v4h_opsr_te)$te
disp_model_v4h_opsr_summary <- summary(disp_model_v4h_opsr)

## Prepare data for worksheet
coeffs <- summary(disp_model_v4h_opsr)$coef_table
coeffs$terms <- rownames(coeffs)
rownames(coeffs) <- NULL

colnames(coeffs) <- c("Estimate", "Standard Error", "t-statistic", "p-value", "Terms")
coeffs <- coeffs %>%
  select(Terms, Estimate, `Standard Error`, `t-statistic`, `p-value`)

stats <- data.frame("nObs_always_in_person" = disp_model_v4h_opsr$nObs[2],
                    "nObs_hybrid" = disp_model_v4h_opsr$nObs[3],
                    "nObs_always_remote" = disp_model_v4h_opsr$nObs[4],
                    "AIC" = disp_model_v4h_opsr_summary$GOF$AIC[1],
                    "BIC" = disp_model_v4h_opsr_summary$GOF$BIC,
                    "r-squared" = disp_model_v4h_opsr_summary$GOFcomponents$R2[1])
rownames(stats) = NULL


# Add the regression results to a sheet
addWorksheet(wb, "model4h_opsr")
writeData(wb, "model4h_opsr", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model4h_opsr", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 
writeData(wb, "model4h_opsr", te_summary, startCol = 1, startRow = nrow(coeffs) + nrow(stats) + 6, rowNames = TRUE) 


## Model 5 -------------------------------------------------------------
## Model type: OPSR
## Specification: Non-work VMT on other days as a function of work arrangement today and on other days.
## Workers: All
disp_model_v5_opsr_eq <- work_arr_day | ln_nonwork_tour_vmt_other_all_days ~ 
  # Factors that affect work arrangement today
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Number of hybrid and remote employees apart from self
  num_hybrid_or_remote +
  ## Commute length
  minutes_to_work + 
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday +
  male:kids_1plus:year2021 |
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Day of week
  sunday + monday + tuesday + thursday + friday + saturday

## Estimation
disp_model_v5_opsr <- opsr(disp_model_v5_opsr_eq, vmt_displacement_mat, weights = vmt_displacement_mat$person_weight, printLevel = 0)
summary(disp_model_v5_opsr)

## Treatment effects 
disp_model_v5_opsr_te <- opsr_te(disp_model_v5_opsr, type = "unlog-response")
print(disp_model_v5_opsr_te)

## Get data for worksheet
te_summary <- summary(disp_model_v5_opsr_te)$te
disp_model_v5_opsr_summary <- summary(disp_model_v5_opsr)

## Prepare data for worksheet
coeffs <- summary(disp_model_v5_opsr)$coef_table
coeffs$terms <- rownames(coeffs)
rownames(coeffs) <- NULL

colnames(coeffs) <- c("Estimate", "Standard Error", "t-statistic", "p-value", "Terms")
coeffs <- coeffs %>%
  select(Terms, Estimate, `Standard Error`, `t-statistic`, `p-value`)

stats <- data.frame("nObs_always_in_person" = disp_model_v5_opsr$nObs[2],
                    "nObs_hybrid" = disp_model_v5_opsr$nObs[3],
                    "nObs_always_remote" = disp_model_v5_opsr$nObs[4],
                    "AIC" = disp_model_v5_opsr_summary$GOF$AIC[1],
                    "BIC" = disp_model_v5_opsr_summary$GOF$BIC,
                    "r-squared" = disp_model_v5_opsr_summary$GOFcomponents$R2[1])
rownames(stats) = NULL

# Add the regression results to a sheet
addWorksheet(wb, "model5_opsr")
writeData(wb, "model5_opsr", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model5_opsr", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 
writeData(wb, "model5_opsr", te_summary, startCol = 1, startRow = nrow(coeffs) + nrow(stats) + 6, rowNames = TRUE) 


## Model 6: Displacement Via Daily Rebound Effects ----------------------------
## Model type: OPSR
## Specification: Non-work VMT on other days as a function of weekly work arrangement and on other days.
## Workers: All
### Part 1: Daily decrease ---------------------------------------
vmt_daily_model_mat$work_arr_day <- factor(vmt_daily_model_mat$work_arr_day, levels = c(1, 2, 3))
vmt_daily_model_mat$in_person_day <- ifelse(vmt_daily_model_mat$work_arr_day == 1, 1, 0)
vmt_daily_model_mat$remote_day <- ifelse(vmt_daily_model_mat$work_arr_day == 2, 1, 0)
vmt_daily_model_mat$no_work_day  <- ifelse(vmt_daily_model_mat$work_arr_day == 3, 1, 0)
vmt_daily_model_mat$travel_dow <-relevel(vmt_daily_model_mat$travel_dow, ref = "Wednesday")

rebound_work_decrease_daily_eq <- work_arr | ln_work_tour_vmt_daily ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Number of hybrid and remote employees apart from self
  num_hybrid_or_remote +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + male:kids_1plus:year2021 |
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Year
  year2021 + year2023 +
  year2021:vehicle2plus |
  # Factors affecting hybrid VMT
  ## Education
  no_bach_plus +
  ## Employment status 
  employed_part_time + self_employed + 
  ## Built Environment 
  jobs_per_hh + intersection_den +
  ## Interaction 
  male:kids_1plus:year2021 +
  ## DOW
  travel_dow +
  year2021 + year2023 |  
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Year
  year2021 + year2023

## Estimation
rebound_work_decrease_daily <- opsr(rebound_work_decrease_daily_eq, vmt_daily_model_mat, printLevel = 0)
summary(rebound_work_decrease_daily)

## Treatment effects 
rebound_work_decrease_daily_te <- opsr_te(rebound_work_decrease_daily, type = "unlog-response", weights = vmt_daily_model_mat$person_weight)
print(rebound_work_decrease_daily_te)

### Part 2: Increase in Non-Work VMT -----------------------------------------
rebound_nonwork_increase_daily_eq <- work_arr | ln_nonwork_tour_vmt_daily ~ 
  # Factors that affect work arrangement
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + hh_income_undisclosed +
  ## Number of hybrid and remote employees apart from self
  num_hybrid_or_remote +
  ## Education
  no_bach_plus + 
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  year2021:self_employed + male:kids_1plus:year2021 |
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
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
  ## Interaction 
  male:kids_1plus:year2021 +
  ## Interactions
  year2021:self_employed + year2021:vehicle2plus

## Estimation
rebound_nonwork_increase_daily <- opsr(rebound_nonwork_increase_daily_eq, vmt_daily_model_mat, printLevel = 0)
summary(rebound_nonwork_increase_daily)

## Treatment effects 
rebound_nonwork_increase_daily_te <- opsr_te(rebound_nonwork_increase_daily, type = "unlog-response", weights = vmt_daily_model_mat$person_weight)
print(rebound_nonwork_increase_daily_te)

print(rebound_work_decrease_daily_te)
print(rebound_nonwork_increase_daily_te)

## Get data for worksheet
te_summary_decrease <- summary(rebound_work_decrease_daily_te)$te
te_summary_increase <- summary(rebound_nonwork_increase_daily_te)$te
rebound_work_decrease_daily_summary <- summary(rebound_work_decrease_daily)
rebound_nonwork_increase_daily_summary <- summary(rebound_nonwork_increase_daily)

## Prepare data for worksheet
coeffs <- rebound_work_decrease_daily_summary$coef_table
coeffs$terms <- rownames(coeffs)
rownames(coeffs) <- NULL

colnames(coeffs) <- c("Estimate", "Standard Error", "t-statistic", "p-value", "Terms")
coeffs <- coeffs %>%
  select(Terms, Estimate, `Standard Error`, `t-statistic`, `p-value`)


stats <- data.frame("nObs_always_in_person" = rebound_work_decrease_daily$nObs[2],
                    "nObs_hybrid" = rebound_work_decrease_daily$nObs[3],
                    "nObs_always_remote" = rebound_work_decrease_daily$nObs[4],
                    "AIC" = rebound_work_decrease_daily_summary$GOF$AIC[1],
                    "BIC" = rebound_work_decrease_daily_summary$GOF$BIC,
                    "r-squared" = rebound_work_decrease_daily_summary$GOFcomponents$R2[1])
rownames(stats) = NULL

# Add the regression results to a sheet
addWorksheet(wb, "model6_opsr")
writeData(wb, "model6_opsr", coeffs, startCol = 1, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model6_opsr", stats, startCol = 1, startRow = nrow(coeffs) + 3, rowNames = FALSE) 
writeData(wb, "model6_opsr", te_summary_decrease, startCol = 1, startRow = nrow(coeffs) + nrow(stats) + 6, rowNames = TRUE) 


## Prepare data for worksheet
coeffs <- rebound_nonwork_increase_daily_summary$coef_table
coeffs$terms <- rownames(coeffs)
rownames(coeffs) <- NULL

colnames(coeffs) <- c("Estimate", "Standard Error", "t-statistic", "p-value", "Terms")
coeffs <- coeffs %>%
  select(Terms, Estimate, `Standard Error`, `t-statistic`, `p-value`)

stats <- data.frame("nObs_always_in_person" = rebound_nonwork_increase_daily$nObs[2],
                    "nObs_hybrid" =  rebound_nonwork_increase_daily$nObs[3],
                    "nObs_always_remote" =  rebound_nonwork_increase_daily$nObs[4],
                    "AIC" = rebound_nonwork_increase_daily_summary$GOF$AIC[1],
                    "BIC" = rebound_nonwork_increase_daily_summary$GOF$BIC,
                    "r-squared" = rebound_nonwork_increase_daily_summary$GOFcomponents$R2[1])
rownames(stats) = NULL

writeData(wb, "model6_opsr", coeffs, startCol = 8, startRow = 1, rowNames = FALSE)

# Add the model statistics below the regression results on the same sheet
writeData(wb, "model6_opsr", stats, startCol = 8, startRow = nrow(coeffs) + 3, rowNames = FALSE) 
writeData(wb, "model6_opsr", te_summary_increase, startCol = 8, startRow = nrow(coeffs) + nrow(stats) + 6, rowNames = TRUE) 


## Save the workbook to an Excel file -----------------------------------------
saveWorkbook(wb, "./regressions_comparison/displacement_v1.xlsx", overwrite = TRUE)


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
