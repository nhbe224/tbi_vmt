### Phase 2 MNL for TBI VMT Project
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/phase3/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable",
                     "stringi", "nnet", "plm")
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

# Relevel -----------------------------------------------------------------
vmt_weekly_for_model$age_group <- relevel(vmt_weekly_for_model$age_group, ref = "35 to 44")
vmt_weekly_for_model$income_detailed <- relevel(vmt_weekly_for_model$income_detailed, ref = "$50-75K")

# Crosstab for Count of Workers -------------------------------------------
vmt_weekly_for_model %>%
  group_by(survey_year, work_arr) %>%
  summarize(n = n()) %>%
  mutate(freq = round(n / sum(n), 2))

# Percent of sample used in analysis --------------------------------------
nrow(vmt_weekly_for_model) / nrow(person_all)

# Regression Model --------------------------------------------------------
## Log Weekly VMT ---------------------------------------------------------
ols_model1 <- lm(ln_weekly_vmt ~ work_arr + age_group + gender + employment + race + 
                   income_detailed + num_kids + num_workers + num_vehicles + num_hybrid_or_remote +
                   jobs_per_hh + emp8_ent + intersection_den + transit_jobs30 + gas_price, 
                   data = vmt_weekly_for_model)

summary(ols_model1)
stargazer::stargazer(ols_model1, type = "text")
print(paste0("On average, always remote workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[2, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))
print(paste0("On average, hybrid workers are associated with a ", round(100 * (exp(summary(ols_model1)$coefficients[3, 1]) - 1), 1), "% difference in weekly VMT relative to always in-person workers."))


## Segment this by work arrangement and run separately.
## Include year.
## Model with work commute VMT and non-work VMT.