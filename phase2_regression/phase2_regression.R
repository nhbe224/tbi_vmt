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
## Bring in cleaned weekly VMT data
source("../phase2_mnl/phase2_mnl_model.R")
setwd("D:/neeco/thesis/tbi_vmt/phase2_regression/")

commute_vmt_daily <- read.csv("../phase1/outputs/commute_vmt_daily.csv")
commute_vmt_daily$survey_year <- factor(commute_vmt_daily$survey_year)

work_arr_daily <- read.csv("../phase1/outputs/work_arr_daily.csv")
work_arr_daily$survey_year <- factor(work_arr_daily$survey_year)
work_arr_daily <- work_arr_daily %>%
  select(person_id, survey_year, work_arr_day, day_num)

# Clean Data --------------------------------------------------------------
vmt_weekly_for_model$ln_weekly_vmt <- log(vmt_weekly_for_model$weekly_vmt)

vmt_weekly_for_model$ln_work_tour_vmt_weekly <- log(vmt_weekly_for_model$work_tour_vmt_weekly + 1)

vmt_weekly_for_model$ln_nonwork_tour_vmt_weekly <- log(vmt_weekly_for_model$nonwork_tour_vmt_weekly + 1)

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


# Convert to Model Matrix (for OPSR) --------------------------------------
vmt_weekly_convert <- vmt_weekly_for_model %>%
  select(person_id, person_weight, age_group, gender, employment, education, race, income_detailed, num_kids, num_vehicles) 

vmt_weekly_convert <- vmt_weekly_convert[, - 1]
vmt_weekly_model_mat_step1 <- model.matrix(~ person_id + person_weight + age_group + gender + employment + education + 
                                       race + income_detailed + num_kids + num_vehicles , data = vmt_weekly_convert)
vmt_weekly_model_mat_step1 <- vmt_weekly_model_mat_step1[, -1]
colnames(vmt_weekly_model_mat_step1) <- c("person_id", "person_weight", "age18to34", "age55plus", "male", 
                                          "employed_part_time", "self_employed", "no_bach_plus", "nonWhite", 
                                          "hh_income_under50", "hh_income100to150", "hh_income150to200", 
                                          "hh_income_200plus", "hh_income_undisclosed", "kids_1plus", "vehicle1",
                                          "vehicle2plus")

vmt_weekly_model_mat_step2 <-  vmt_weekly_for_model %>%
  select(-age_group, -gender, -employment, -education, -race, -income_detailed, -num_kids, -num_vehicles)

vmt_weekly_model_mat <- vmt_weekly_model_mat_step2 %>%
  left_join(vmt_weekly_model_mat_step1, copy = T)

vmt_weekly_model_mat$work_arr <- ifelse(vmt_weekly_model_mat$work_arr == "Always In-Person", 1,
                                        ifelse(vmt_weekly_model_mat$work_arr == "Hybrid", 2, 3))

vmt_weekly_model_mat <- vmt_weekly_model_mat[, !(names(vmt_weekly_model_mat) %in% c("hh_id", "num_workers"))]

# Full Model  ------------------------------------------------------------
ols_model1 <- lm(ln_weekly_vmt ~ work_arr + survey_year:work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                   data = vmt_weekly_for_model, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

summary(ols_model1)
stargazer::stargazer(ols_model1, type = "text")
stargazer::stargazer(ols_model1, type = "latex")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))


# Non-Log Transformed Model -----------------------------------------------
ols_model1b <- lm(weekly_vmt ~ work_arr + survey_year:work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model, weights = person_weight) ## Drop gas price since its perfectly collinear with survey year.

summary(ols_model1b)
stargazer::stargazer(ols_model1b, type = "text")
stargazer::stargazer(ols_model1b, type = "latex")

print(paste0("On average, always remote workers are associated with a ", round(summary(ols_model1b)$coefficients[2, 1]), " difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(summary(ols_model1b)$coefficients[3, 1]), " difference in weekly VMT relative to always in-person workers."))


# Always In-Person -----------------------------------------------------
ols_model2 <- lm(ln_weekly_vmt ~ age_group + gender + employment + race + 
                   income_detailed +  gender:num_kids + num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model[vmt_weekly_for_model$work_arr == "Always In-Person", ]) 

summary(ols_model2)
stargazer::stargazer(ols_model2, type = "text")

ols_model2$rank == ncol(model.matrix(ols_model2))

# Hybrid ---------------------------------------------------------------
ols_model3 <- lm(ln_weekly_vmt ~ age_group + gender + employment + race + 
                   income_detailed + num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model[vmt_weekly_for_model$work_arr == "Hybrid", ]) 

summary(ols_model3)
stargazer::stargazer(ols_model3, type = "text")

ols_model3$rank == ncol(model.matrix(ols_model3))

# Always Remote ---------------------------------------------------------------
ols_model4 <- lm(ln_weekly_vmt ~ age_group + gender + employment + race + 
                   income_detailed + num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model[vmt_weekly_for_model$work_arr == "Always Remote", ]) 

summary(ols_model4)
stargazer::stargazer(ols_model4, type = "text")

ols_model4$rank == ncol(model.matrix(ols_model4))


# Non-Work VMT Only --------------------------------------------------------
ols_model5 <- lm(ln_nonwork_tour_vmt_weekly ~ work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model) 

summary(ols_model5)
stargazer::stargazer(ols_model5, type = "text")

exp(summary(ols_model5)$coefficients[2, 1]) ## Non work VMT increases by factor of 2.35.
exp(summary(ols_model5)$coefficients[3, 1]) ## Factor of 1.50 for hybrid. 

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model5)$coefficients[2, 1]) - 1), 1), "% difference in weekly non-work VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model5)$coefficients[3, 1]) - 1), 1), "% difference in weekly non-work VMT relative to always in-person workers."))


# Work VMT Only --------------------------------------------------------
ols_model6 <- lm(ln_work_tour_vmt_weekly ~ work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model) 

summary(ols_model6)
stargazer::stargazer(ols_model6, type = "text")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model6)$coefficients[2, 1]) - 1), 1), "% difference in weekly work VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model6)$coefficients[3, 1]) - 1), 1), "% difference in weekly work VMT relative to always in-person workers."))


# Daily Models ------------------------------------------------------------
vmt_daily <- commute_vmt_daily %>%
  left_join(vmt_weekly_for_model) %>%
  mutate(vmt_daily = work_tour_vmt_daily + nonwork_tour_vmt_daily,
         ln_vmt_daily = log(vmt_daily + 1),
         ln_work_tour_vmt_daily = log(work_tour_vmt_daily + 1),
         ln_nonwork_tour_vmt_daily = log(nonwork_tour_vmt_daily + 1))

dows <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
ols_model_dow <- list()
for(i in 1:length(dows)){
  ols_model_dow[[i]] <- lm(ln_vmt_daily ~ work_arr + age_group + gender + employment + race + 
                             income_detailed + num_kids + gender:num_kids + num_vehicles + num_hybrid_or_remote +
                             jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                           data = vmt_daily[vmt_daily$travel_dow == dows[i], ]) 
  
  print(paste0("On ", dows[i], " on average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model_dow[[i]])$coefficients[2, 1]) - 1), 1), "% difference in daily VMT relative to always in-person workers."))
  print(paste0("On ", dows[i], " on average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model_dow[[i]])$coefficients[3, 1]) - 1), 1), "% difference in daily VMT relative to always in-person workers."))
}


# For Hybrid: Go to work versus remote days -------------------------------
hybrid_vmt_daily <- vmt_daily %>%
  left_join(work_arr_daily) %>%
  filter(work_arr == "Hybrid")

hybrid_vmt_daily$work_arr_day <- ifelse(hybrid_vmt_daily$work_arr_day == "hybrid_day", "in_person_day", hybrid_vmt_daily$work_arr_day)
hybrid_vmt_daily$work_arr_day <- factor(hybrid_vmt_daily$work_arr_day)
hybrid_vmt_daily$work_arr_day <- relevel(hybrid_vmt_daily$work_arr_day, ref = "in_person_day")

ols_model_hybrid <- lm(ln_vmt_daily ~ work_arr_day + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = hybrid_vmt_daily)

summary(ols_model_hybrid)
stargazer::stargazer(ols_model_hybrid, type = "text")
stargazer::stargazer(ols_model_hybrid, type = "latex", single.row = T, omit.stat = c("aic", "ser"))

print(paste0("On average, remote days are associated with a ", round(100 * (exp(summary(ols_model_hybrid)$coefficients[3, 1]) - 1), 1), "% difference in daily VMT relative to in-person days (for hybrid workers)."))


# Individual Year Models --------------------------------------------------
## 2019 -------------------------------------------------------------------
ols_model2019 <- lm(ln_weekly_vmt ~ work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30, 
                 data = vmt_weekly_for_model[vmt_weekly_for_model$survey_year == 2019, ])

summary(ols_model2019)
stargazer::stargazer(ols_model2019, type = "text")
stargazer::stargazer(ols_model2019, type = "latex")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model2019)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model2019)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))

## 2021 -------------------------------------------------------------------
ols_model2021 <- lm(ln_weekly_vmt ~ work_arr + age_group + gender + employment + race + 
                      income_detailed + num_kids + gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30, 
                    data = vmt_weekly_for_model[vmt_weekly_for_model$survey_year == 2021, ])

summary(ols_model2021)
stargazer::stargazer(ols_model2021, type = "text")
stargazer::stargazer(ols_model2021, type = "latex")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model2021)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model2021)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))

## 2023 -------------------------------------------------------------------
ols_model2023 <- lm(ln_weekly_vmt ~ work_arr + age_group + gender + employment + race + 
                      income_detailed + num_kids + gender:num_kids + education + num_vehicles + num_hybrid_or_remote +
                      jobs_per_hh + emp8_ent + intersection_den + transit_jobs30, 
                    data = vmt_weekly_for_model[vmt_weekly_for_model$survey_year == 2023, ])

summary(ols_model2023)
stargazer::stargazer(ols_model2023, type = "text")
stargazer::stargazer(ols_model2023, type = "latex")

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model2023)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model2023)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))


# Ordered Probit Switching Regression -------------------------------------
opsr_model1_eq <- work_arr | ln_weekly_vmt ~ 
  age18to34 + age55plus + hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + kids_1plus + no_bach_plus + year2021 + year2023 +  year2021:no_bach_plus + 
  employed_part_time + self_employed | 
  # For always in-person
  male + nonWhite + employed_part_time + self_employed + no_bach_plus + vehicle2plus + jobs_per_hh + 
  kids_1plus + year2021 + year2023 + intersection_den + emp8_ent + jobs_per_hh | 
  # For hybrid
  male + employed_part_time + year2021:employed_part_time + self_employed + 
  year2021:self_employed + vehicle2plus + jobs_per_hh + kids_1plus + year2021 + 
  year2023 + intersection_den + emp8_ent + jobs_per_hh  |  
  # For always remote
  male + employed_part_time + nonWhite + self_employed + year2021:self_employed +
  year2021:employed_part_time + vehicle2plus + jobs_per_hh + hh_income100to150 + hh_income150to200 +
  kids_1plus + year2021 + year2023 + intersection_den + emp8_ent + jobs_per_hh 

## Estimation
opsr_model1 <- opsr(opsr_model1_eq, vmt_weekly_model_mat, printLevel = 0)

## Inference
summary(opsr_model1)
print(opsr_te(opsr_model1, type = "response", weights = vmt_weekly_model_mat$person_weight))
print(opsr_te(opsr_model1, type = "unlog-response", weights = vmt_weekly_model_mat$person_weight))


# Weekday Model OPSR ------------------------------------------------------
vmt_daily_model_mat_step1 <- model.matrix(~ person_id + person_weight + age_group + gender + employment + education + 
                                               race + income_detailed + num_kids + num_vehicles + day_num, data = vmt_daily)
vmt_daily_model_mat_step1 <- vmt_daily_model_mat_step1[, -1]

colnames(vmt_daily_model_mat_step1) <- c("person_id", "person_weight", "age18to34", "age55plus", "male", 
                                          "employed_part_time", "self_employed", "no_bach_plus", "nonWhite", 
                                          "hh_income_under50", "hh_income100to150", "hh_income150to200", 
                                          "hh_income_200plus", "hh_income_undisclosed", "kids_1plus", "vehicle1",
                                          "vehicle2plus", "day_num")

vmt_daily_model_mat_step2 <-  vmt_daily %>%
  select(-age_group, -gender, -employment, -education, -race, -income_detailed, -num_kids, -num_vehicles)

vmt_daily_model_mat <- vmt_daily_model_mat_step2 %>%
  inner_join(vmt_daily_model_mat_step1, by = c("person_id", "person_weight", "day_num"), copy = T)

vmt_daily_model_mat$work_arr <- ifelse(vmt_daily_model_mat$work_arr == "Always In-Person", 1,
                                        ifelse(vmt_daily_model_mat$work_arr == "Hybrid", 2, 3))

vmt_daily_model_mat <- vmt_daily_model_mat[, !(names(vmt_daily_model_mat) %in% c("hh_id", "num_workers"))]

vmt_daily_weekday <- vmt_daily_model_mat %>%
  filter(!travel_dow %in% c("Saturday", "Sunday")) %>%
  inner_join(work_arr_daily) %>%
  mutate(work_arr_day = case_when(work_arr_day == "in_person_day" ~ 1,
                   work_arr_day == "hybrid_day" ~ 2,
                   work_arr_day == "remote_day" ~ 3,
                   work_arr_day == "no_work_day" ~ 4))

opsr_model2_eq <- work_arr_day | ln_vmt_daily ~ 
  age18to34 + age55plus + hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + kids_1plus + no_bach_plus + year2021 + year2023 +  year2021:no_bach_plus + 
  employed_part_time + self_employed | 
  # For in-person day
  male + nonWhite + employed_part_time + self_employed + no_bach_plus + vehicle2plus + jobs_per_hh + 
  kids_1plus + year2021 + year2023 + intersection_den + emp8_ent + jobs_per_hh | 
  # For hybrid
  male + employed_part_time + year2021:employed_part_time + self_employed + 
  year2021:self_employed + vehicle2plus + jobs_per_hh + kids_1plus + year2021 + 
  year2023 + intersection_den + emp8_ent + jobs_per_hh  |  
  # For always remote
  male + employed_part_time + nonWhite + self_employed + year2021:self_employed +
  year2021:employed_part_time + vehicle2plus + jobs_per_hh + hh_income100to150 + hh_income150to200 +
  kids_1plus + year2021 + year2023 + intersection_den + emp8_ent + jobs_per_hh |
  # For no work day
  male + nonWhite + employed_part_time + self_employed + no_bach_plus + vehicle2plus
  year2021:employed_part_time + jobs_per_hh + kids_1plus + year2021 + year2023 + 
  intersection_den + emp8_ent + jobs_per_hh 

## Estimation
opsr_model2 <- opsr(opsr_model2_eq, vmt_daily_weekday, printLevel = 0)

## Inference
summary(opsr_model2)
print(opsr_te(opsr_model2, type = "response"))
print(opsr_te(opsr_model2, type = "unlog-response"))

# OPSR 2021 only ----------------------------------------------------------
opsr_model2021_eq <- work_arr | ln_weekly_vmt ~ 
  age18to34 + age55plus + hh_income_under50 + hh_income100to150 + hh_income150to200 + 
  hh_income_200plus + kids_1plus + no_bach_plus + 
  employed_part_time + self_employed | 
  # For always in-person
  male + nonWhite + employed_part_time + self_employed + no_bach_plus + vehicle2plus + jobs_per_hh + 
  kids_1plus + intersection_den + emp8_ent + jobs_per_hh | 
  # For hybrid
  male + employed_part_time + self_employed + vehicle2plus + jobs_per_hh + kids_1plus + 
  intersection_den + emp8_ent + jobs_per_hh  |  
  # For always remote
  male + employed_part_time + nonWhite + self_employed + vehicle2plus + jobs_per_hh + 
  hh_income100to150 + hh_income150to200 +
  kids_1plus + intersection_den + emp8_ent + jobs_per_hh 

## Estimation
opsr_model2021 <- opsr(opsr_model2021_eq, vmt_weekly_model_mat[vmt_weekly_model_mat$survey_year == 2021, ], printLevel = 0)

## Inference
summary(opsr_model2021)
print(opsr_te(opsr_model2021, type = "response", weights = vmt_weekly_model_mat[vmt_weekly_model_mat$survey_year == 2021, ]$person_weight))


# OPSR Replication --------------------------------------------------------
data(telework_data)

f <-
twing_status | vmd_ln ~
edu_2 + edu_3 + hhincome_2 + hhincome_3 + flex_work + work_fulltime + twing_feasibility  |
 female + age_mean + age_mean_sq + race_black + race_other + vehicle +
suburban + smalltown + rural + work_fulltime + region_waa |
   edu_2 + edu_3 + suburban + smalltown + rural + work_fulltime +
   att_prolargehouse + att_proactivemode + att_procarowning |
   female + hhincome_2 + hhincome_3 + child + suburban + smalltown +
   rural + region_waa

start_default <- opsr(f, telework_data, .get2step = TRUE)
fit <- opsr(f, telework_data, start = start_default, method = "NM", iterlim = 50e3, printLevel = 0)
summary(fit)
print(opsr_te(fit, type = "unlog-response", weights = telework_data$weight))
print(opsr_te(fit, type = "response", weights = telework_data$weight))
exp(-0.2) - 1 ## 18% decrease from always in-person to hybrid
exp(-1.228) - 1 ## 70% decrease from always in-person to always remote

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
