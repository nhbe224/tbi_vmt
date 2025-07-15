### Phase 3 Regression Models
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase3/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable",
                     "stringi", "nnet", "plm", "forcats")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)

# Read Data ---------------------------------------------------------------
## Bring in cleaned weekly VMT data
source("../phase2/phase2_mnl_model.R")
setwd("D:/neeco/thesis/tbi_vmt/phase3/")


# Clean Data --------------------------------------------------------------
vmt_weekly_for_model$ln_weekly_vmt <- log(vmt_weekly_for_model$weekly_vmt)

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

# Full Model  ------------------------------------------------------------
ols_model1 <- lm(ln_weekly_vmt ~ work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                   data = vmt_weekly_for_model) ## Drop gas price since its perfectly collinear with survey year.

summary(ols_model1)
stargazer::stargazer(ols_model1, type = "text")
stargazer::stargazer(ols_model1, type = "latex", single.row = T, omit.stat = c("aic", "ser"))

print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))

ols_model1$rank == ncol(model.matrix(ols_model1))

# Always In-Person -----------------------------------------------------
ols_model2 <- lm(ln_weekly_vmt ~ age_group + gender + employment + race + 
                   income_detailed + num_kids + num_vehicles + num_hybrid_or_remote +
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

# Hybrid ---------------------------------------------------------------
ols_model4 <- lm(ln_weekly_vmt ~ age_group + gender + employment + race + 
                   income_detailed + num_kids + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + survey_year, 
                 data = vmt_weekly_for_model[vmt_weekly_for_model$work_arr == "Always Remote", ]) 

summary(ols_model4)
stargazer::stargazer(ols_model4, type = "text")

ols_model4$rank == ncol(model.matrix(ols_model4))

## Model with work commute VMT and non-work VMT.