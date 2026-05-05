### Statistical Analysis
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/statistical_analysis")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid",
                     "stargazer", "gtable", "broom", "writexl", "openxlsx",
                     "stringi", "nnet", "plm", "forcats", "OPSR", "texreg", "plm",
                     "mlogit")
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

vmt_weekly_model_mat$remote_day_pct_100 <- vmt_weekly_model_mat$remote_day_pct * 100

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
  dplyr::summarize(count = n()) %>%
  mutate(pct = count / sum(count))
         
for(i in colnames(categorical_vars)){
  print(categorical_vars %>%
          group_by(.data[[i]]) %>%
          dplyr::summarize(count = n()) %>%
          mutate(pct = count / sum(count)))
}

# Summary Table -----------------------------------------------------------
vmt_weekly_df_covariates <- vmt_weekly_df %>%
  select(num_remote_days, num_hybrid_or_remote, intersection_den)

stargazer(as.data.frame(vmt_weekly_df))


# Preferred Specification for Total Effects  -------------------------------
set.seed(124)
opsr_model_final_eq <- work_arr | ln_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Age
  age18to34 + age55plus +
  ## Education
  no_bach_plus + 
  ## Race
  nonWhite +
  ## Women with kids
  nonmale_kids_1plus + nonmale_kids_0 + male_kids_1plus +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 +
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  hh_income150to200:post_covid + 
  hh_income_200plus:post_covid + hh_income_undisclosed:post_covid +
  no_bach_plus:post_covid |

  # Factors affecting always in-person VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time + 
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults +
  ## Year
  year2021 + year2023  | 
  
  # Factors affecting hybrid VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 |
  
  # Factors affecting always remote VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 

## Estimation
opsr_model_final <- opsr(opsr_model_final_eq, vmt_weekly_model_mat, printLevel = 0)

## Treatment effects 
opsr_model_final_te <- opsr_te(opsr_model_final, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

## Inference
summary(opsr_model_final)
texreg(opsr_model_final, single.row = T, stars = c(0.001, 0.01, 0.05, 0.1), symbol = "\\dagger")
screenreg(opsr_model_final, single.row = T, stars = c(0.001, 0.01, 0.05, 0.1), symbol = "\\dagger")
print(opsr_model_final_te)

reg_obj <- extract(opsr_model_final)

results_df <- data.frame(
  term = reg_obj@coef.names,
  est  = reg_obj@coef,
  se   = reg_obj@se,
  pval = reg_obj@pvalues
)

final_output <- results_df %>%
  mutate(
    # Create significance stars based on your rules
    stars = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      pval < 0.10  ~ "^",
      TRUE         ~ ""
    ),
    # Combine into the "Estimate (SE)Stars" format
    estimate_se = paste0(
      formatC(est, format = "f", digits = 2), 
      " (", formatC(se, format = "f", digits = 2), ")", 
      stars
    )
  ) %>%
  # Keep only the requested columns
  select(term, estimate_se)



write.csv(final_output, "./outputs/opsr_model_final.csv", row.names = FALSE)


## Percent difference in VMT between hybrid observed and hybrid counterfactual
hybrid_vmt_observed <- weighted.mean(predict(opsr_model_final, group = 2, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_vmt_counterfact <- weighted.mean(predict(opsr_model_final, group = 2, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
hybrid_vmt_observed
hybrid_vmt_counterfact
hybrid_vmt_observed - hybrid_vmt_counterfact
print(paste0("If an observed hybrid worker switches from always in-person to hybrid, their weekly VMT changes by ", round((((hybrid_vmt_observed - hybrid_vmt_counterfact) / hybrid_vmt_counterfact)*100), 2), "%."))


## Percent difference in VMT between always remote observed and always remote counterfactual
ar_vmt_observed <- weighted.mean(predict(opsr_model_final, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_vmt_counterfact <- weighted.mean(predict(opsr_model_final, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_vmt_observed
ar_vmt_counterfact
ar_vmt_observed - ar_vmt_counterfact
print(paste0("If an observed always remote worker switches from always in-person to always remote, their weekly VMT changes by ", round((((ar_vmt_observed - ar_vmt_counterfact) / ar_vmt_counterfact)*100), 1), "%."))


## Calculating ATEs -------------------------------------------------------
### Matrices of treatment effects by work arrangement
all_treatment_mat <- opsr_model_final_te$ce.by.treatments
ar_treatment_mat <- opsr_model_final_te$ce.by.treatments[[3]]
hybrid_treatment_mat <- opsr_model_final_te$ce.by.treatments[[2]] 
aip_control_mat <- opsr_model_final_te$ce.by.treatments[[1]]

### Take the weighted mean of each matrix and that is the treatment value.
### Subtract from control to get treatment effect.
### Each matrix represents all workers, regardless if they are treated.
### For hybrid_treatment_mat, this represents VMT if
### always in-person workers were hybrid (column 1), 
### hybrid worers were hybrid (column 2),
### always remote workers were hybrid (column 3)
hybrid_treatment <- weighted.mean(hybrid_treatment_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
aip_control <- weighted.mean(aip_control_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
ar_treatment <- weighted.mean(ar_treatment_mat, w = rep(vmt_weekly_model_mat$person_weight, 3), na.rm = T)
ar_treatment - aip_control
hybrid_treatment - aip_control

# Preferred Specification for Rebound Effects -----------------------------
## Step 1: increase in non-work tour VMT ----------------------------------
rebound_nonwork_increase_eq <- work_arr | ln_nonwork_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Age
  age18to34 + age55plus +
  ## Education
  no_bach_plus + 
  ## Race
  nonWhite +
  ## Women with kids
  nonmale_kids_1plus + nonmale_kids_0 + male_kids_1plus +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 +
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  hh_income150to200:post_covid + 
  hh_income_200plus:post_covid + hh_income_undisclosed:post_covid +
  no_bach_plus:post_covid |
  
  # Factors affecting always in-person VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time + 
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults +
  ## Year
  year2021 + year2023  | 
  
  # Factors affecting hybrid VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 |
  
  # Factors affecting always remote VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 


## Estimation
rebound_nonwork_increase <- opsr(rebound_nonwork_increase_eq, vmt_weekly_model_mat, printLevel = 0)

## Treatment effects 
rebound_nonwork_increase_te <- opsr_te(rebound_nonwork_increase, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_nonwork_increase)
texreg(rebound_nonwork_increase, single.row = T, stars = c(0.001, 0.01, 0.05, 0.1), symbol = "\\dagger")
print(rebound_nonwork_increase_te)

reg_obj <- extract(rebound_nonwork_increase)

results_df <- data.frame(
  term = reg_obj@coef.names,
  est  = reg_obj@coef,
  se   = reg_obj@se,
  pval = reg_obj@pvalues
)

nonwork_output <- results_df %>%
  mutate(
    # Create significance stars based on your rules
    stars = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      pval < 0.10  ~ "^",
      TRUE         ~ ""
    ),
    # Combine into the "Estimate (SE)Stars" format
    estimate_se = paste0(
      formatC(est, format = "f", digits = 2), 
      " (", formatC(se, format = "f", digits = 2), ")", 
      stars
    )
  ) %>%
  # Keep only the requested columns
  select(term, estimate_se)



write.csv(nonwork_output, "./outputs/nonwork_output.csv", row.names = FALSE)

ar_nonwork_vmt_observed <- weighted.mean(predict(rebound_nonwork_increase, group = 3, type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_counterfact <- weighted.mean(predict(rebound_nonwork_increase, group = 3, counterfact = 1,  type = "unlog-response"), w = vmt_weekly_model_mat$person_weight, na.rm = T)
ar_nonwork_vmt_observed
ar_nonwork_vmt_counterfact
ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact



## Step 2: decrease in work tour VMT ----------------------------------------
rebound_work_decrease_eq <- work_arr | ln_work_tour_vmt_weekly ~ 
  # Factors that affect work arrangement
  ## Age
  age18to34 + age55plus +
  ## Education
  no_bach_plus + 
  ## Race
  nonWhite +
  ## Women with kids
  nonmale_kids_1plus + nonmale_kids_0 + male_kids_1plus +
  ## Household Income
  hh_income_under50 + hh_income100to150 + hh_income150to200 +
  hh_income_200plus + hh_income_undisclosed +
  ## Employment Status
  employed_part_time + self_employed +
  ## Year
  year2021 + year2023 +
  ## Interactions
  hh_income150to200:post_covid + 
  hh_income_200plus:post_covid + hh_income_undisclosed:post_covid +
  no_bach_plus:post_covid |
  
  # Factors affecting always in-person VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time + 
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults +
  ## Year
  year2021 + year2023  | 
  
  # Factors affecting hybrid VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_0 + cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 |
  
  # Factors affecting always remote VMT
  ## Gender
  nonmale +
  ## Kids
  kids_1plus +
  ## Number of vehicles per adult
  cars_lt1_per_adult + 
  ## Employment level
  employed_part_time +
  ## Built Environment 
  ln_res_den + ln_intersection_den +
  ## Number of adults
  num_adults + 
  ## Year
  year2021 + year2023 


## Estimation
rebound_work_decrease <- opsr(rebound_work_decrease_eq, vmt_weekly_model_mat, printLevel = 0)
## Treatment effects 
rebound_work_decrease_te <- opsr_te(rebound_work_decrease, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight)

summary(rebound_work_decrease)
print(rebound_work_decrease_te)
texreg(rebound_work_decrease, single.row = T, stars = c(0.001, 0.01, 0.05, 0.1), symbol = "\\dagger")

reg_obj <- extract(rebound_work_decrease)

results_df <- data.frame(
  term = reg_obj@coef.names,
  est  = reg_obj@coef,
  se   = reg_obj@se,
  pval = reg_obj@pvalues
)

work_output <- results_df %>%
  mutate(
    # Create significance stars based on your rules
    stars = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      pval < 0.10  ~ "^",
      TRUE         ~ ""
    ),
    # Combine into the "Estimate (SE)Stars" format
    estimate_se = paste0(
      formatC(est, format = "f", digits = 2), 
      " (", formatC(se, format = "f", digits = 2), ")", 
      stars
    )
  ) %>%
  # Keep only the requested columns
  select(term, estimate_se)



write.csv(work_output, "./outputs/work_output.csv", row.names = FALSE)

## Step 3: Get rebound effects ------------------------------------------------
### Rebound effects for hybrid ------------------------------------------------
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
hybrid_rebound_effect <- abs((hybrid_nonwork_vmt_observed - hybrid_nonwork_vmt_counterfact) / (hybrid_work_vmt_observed - hybrid_work_vmt_counterfact))
hybrid_rebound_effect

print(paste0(round(hybrid_rebound_effect * 100, 1), "% of hybrid workers' decrease in work tour VMT is offset by increases in non-work tour VMT."))


### Rebound effects for always remote -----------------------------------------
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
ar_rebound_effect <- abs((ar_nonwork_vmt_observed - ar_nonwork_vmt_counterfact) / (ar_work_vmt_observed - ar_work_vmt_counterfact))
ar_rebound_effect

print(paste0(round(ar_rebound_effect * 100, 1), "% of always remote workers' decrease in work tour VMT is offset by increases in non-work tour VMT."))



