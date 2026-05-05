### Exploratory Analysis for TBI VMT
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/exploratory_analysis/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", "patchwork", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable", "ggpattern",
                     "stargazer", "Hmisc", "dplyr")
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
work_arr_by_region_load <- read.csv("./work_arr_by_region.csv")
us_vmt_per_capita <- read.csv("../inputs/us_vmt_per_capita.csv") %>%
  filter(!is.na(year))

# Summary Tables ----------------------------------------------------------
## Continuous Variables ---------------------------------------------------
get_continuous_stats <- function(data) {
  data %>%
    # Pivot variables so we can calculate stats for all 4 at once
    pivot_longer(cols = c(vmt_weekly, work_tour_vmt_weekly, nonwork_tour_vmt_weekly,
                          res_den, intersection_den,
                          num_adults), 
                 names_to = "variable", 
                 values_to = "value") %>%
    # Group by Year, Variable, and State
    group_by(variable, work_arr) %>%
    dplyr::summarize(
      n = n(),
      w_mean = weighted.mean(value, person_weight, na.rm = TRUE),
      # Using sqrt of weighted variance for SD
      w_sd = sqrt(wtd.var(value, person_weight, na.rm = TRUE)),
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Arrange by state to ensure the "State 1 & State 2" order is consistent
    arrange( variable, work_arr) %>%
    # Group by year and variable to collapse the states into one string
    group_by(variable) %>%
    dplyr::summarize(
      N = paste(n, collapse = " & "),
      Weighted_Mean = paste(round(w_mean, 2), collapse = " & "),
      Weighted_SD = paste(round(w_sd, 2), collapse = " & "),
      Min = paste(round(min_val, 1), collapse = " & "),
      Max = paste(round(max_val, 1), collapse = " & "),
      .groups = "drop"
    )
}

continuous_vars_stats <- get_continuous_stats(vmt_weekly_df)


vmt_weekly_df <- vmt_weekly_df %>%
  mutate(vehicles_per_adult = case_when(vehicles_numeric / num_adults == 0 ~ "Zero vehicles",
                                        vehicles_numeric / num_adults > 0 & vehicles_numeric / num_adults < 1 ~ "LT 1 veh per adult",
                                        vehicles_numeric / num_adults >=  1 ~ "GE 1 veh per adult") )

## Categorical Variables ---------------------------------------------
get_categorical_summary <- function(data) {
  result <- data %>%
    # 1. Reshape to long format to handle all variables at once
    pivot_longer(cols = c(employment, age_group, gender, race, education, income_detailed, 
                          num_kids, vehicles_per_adult), 
                 names_to = "var_name", 
                 values_to = "level") %>%
    
    # 2. Calculate counts and weighted sums per category per state/year
    group_by(work_arr, var_name, level) %>%
    summarise(
      n = n(),
      cat_weight_sum = sum(person_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # 3. Calculate total weight per state/year for the percentage denominator
    group_by(work_arr, var_name) %>%
    mutate(
      total_state_weight = sum(cat_weight_sum, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    # 4. Create the "count (pct\%)" string for each state
    mutate(
      pct = (cat_weight_sum / total_state_weight * 100),
      # Formatting the combined string: "10 (25.5\%)"
      combined_val = paste0(n, " (", round(pct), "\\%)"),
      variable = paste0(var_name, ": ", level)
    ) %>%
    
    # 5. Ensure states are sorted alphabetically for a consistent order in the ampersand list
    arrange(variable, work_arr) %>%
    
    # 6. Final aggregation to join state values with ' & '
    group_by( variable) %>%
    summarise(
      count_pct = paste(combined_val, collapse = " & "),
      .groups = "drop"
    )
  return(result)
}

categorical_vars_stats <- get_categorical_summary(vmt_weekly_df) %>%
  arrange(variable)



##########################


for(i in c(2019, 2021, 2023)){
  aip_continuous <- aip_vmt_weekly_df[aip_vmt_weekly_df$survey_year == i, continuous_vars]
  hybrid_continuous <- hybrid_vmt_weekly_df[hybrid_vmt_weekly_df$survey_year == i, continuous_vars]
  ar_continuous <- ar_vmt_weekly_df[ar_vmt_weekly_df$survey_year == i, continuous_vars]
  
  aip_w_means <- sapply(aip_continuous[continuous_vars[-length(continuous_vars)]], 
                        function(x) weighted.mean(x, w = aip_continuous$person_weight))
  hybrid_w_means <- sapply(hybrid_continuous[continuous_vars[-length(continuous_vars)]], 
                           function(x) weighted.mean(x, w = hybrid_continuous$person_weight))
  ar_w_means <- sapply(ar_continuous[continuous_vars[-length(continuous_vars)]], 
                       function(x) weighted.mean(x, w = ar_continuous$person_weight))
  print(paste0("Year:", i))
  print(paste0("Always In Person Means:", i))
  print(aip_w_means)
  print(paste0("Hybrid Means:", i))
  print(hybrid_w_means)
  print(paste0("Always Remote Means:", i))
  print(ar_w_means)
  
  
  aip_w_sd <- sapply(aip_continuous[continuous_vars[-length(continuous_vars)]], 
                     function(x) sqrt(wtd.var(x, w = aip_continuous$person_weight)))
  hybrid_w_sd <- sapply(hybrid_continuous[continuous_vars[-length(continuous_vars)]], 
                        function(x) sqrt(wtd.var(x, w = hybrid_continuous$person_weight)))
  ar_w_sd <- sapply(ar_continuous[continuous_vars[-length(continuous_vars)]], 
                    function(x) sqrt(wtd.var(x, w = ar_continuous$person_weight)))
  
  print(paste0("Year:", i))
  print(paste0("Always In Person SDs:", i))
  print(aip_w_sd)
  print(paste0("Hybrid SDs:", i))
  print(hybrid_w_sd)
  print(paste0("Always Remote SDs:", i))
  print(ar_w_sd)
  
  stargazer(as.data.frame(aip_continuous), type = "text", digits = 1)
  stargazer(as.data.frame(hybrid_continuous), type = "text", digits = 1)
  stargazer(as.data.frame(ar_continuous), type = "text", digits = 1)
  
  
}



for(v in continuous_vars[-length(continuous_vars)]) {
  # Calculate min and max for each arrangement
  stats <- vmt_weekly_df %>%
    group_by(work_arr) %>%
    dplyr::summarize(
      min_val = round(min(.data[[v]], na.rm = TRUE), 1),
      max_val = round(max(.data[[v]], na.rm = TRUE), 1),
      .groups = 'drop'
    ) %>%
    # Ensure all levels are present even if missing in data
    complete(work_arr)
  
  # Format and print Minimums using & as separator
  cat(paste0(v, ": ", paste(format(stats$min_val, nsmall = 1), collapse = " & "), "\n"))
  
  # Format and print Maximums using & as separator
  cat(paste0(v, ": ", paste(format(stats$max_val, nsmall = 1), collapse = " & "), "\n"))
}

## Categorical Variables ---------------------------------------------------
vmt_weekly_df$age_group <- factor(vmt_weekly_df$age_group, c("18 to 34", "35 to 54", "55 or older"))
vmt_weekly_df$income_detailed <- factor(vmt_weekly_df$income_detailed, 
                                        c("<$50K", "$50-$100K", "$100-$150K", "$150-$200K", "$200K+", "Undisclosed"))
# vmt_weekly_df$income_detailed <- relevel(vmt_weekly_df$income_detailed, ref = "$50-$100K")

categorical_vars <- vmt_weekly_df %>%
  select(work_arr, age_group, gender, employment, race, education, income_detailed, 
         num_kids, num_vehicles, survey_year, auto_sufficiency, person_weight)

categorical_vars <- as.data.frame(categorical_vars[-1])


for(i in colnames(categorical_vars[c(-1, -length(colnames(categorical_vars)))])){
  print(categorical_vars %>%
          group_by(work_arr, .data[[i]]) %>%
          dplyr::summarize(count = n()) %>%
          mutate(pct = count / sum(count)))
}

vmt_weekly_df$auto_sufficiency <- factor(vmt_weekly_df$auto_sufficiency)

for(i in colnames(categorical_vars[c(-1, -length(colnames(categorical_vars)))])){
  print(categorical_vars %>%
          group_by(work_arr, .data[[i]]) %>%
          dplyr::summarize(total_weighted_count = sum(person_weight, na.rm = TRUE),
                           unweighted_count = n()) %>%
          mutate(wtd_pct = (total_weighted_count / sum(total_weighted_count))))
}


vmt_weekly_df_mean <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  dplyr::summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

vmt_weekly_df_mean_by_year <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  dplyr::summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

work_vmt_weekly_df_mean_by_year <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  dplyr::summarize(work_vmt_weekly_wtd_mean = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            work_vmt_weekly_mean = mean(work_tour_vmt_weekly, na.rm = T))


# Work Arrangement Pct by Year --------------------------------------------
work_arr_by_year <- vmt_weekly_df %>% 
  group_by(survey_year, work_arr) %>%
  dplyr::summarize(count = n(), .groups = 'drop_last') %>%
  mutate(pct = (count / sum(count))) %>%
  ungroup()

work_arr_by_year_w <- vmt_weekly_df %>%
  group_by(survey_year, work_arr) %>%
  dplyr::summarize(
    total_weighted_count = sum(person_weight, na.rm = TRUE),
    .groups = "drop_last" 
  ) %>%
  mutate(
    wtd_pct = (total_weighted_count / sum(total_weighted_count) * 100),
    ypos = ifelse(work_arr == "Always In-Person", 
                  cumsum(wtd_pct) - wtd_pct / 2,
                  100 - (cumsum(wtd_pct) - wtd_pct / 2)),
    show_label = wtd_pct > 10
  ) %>%
  ungroup() %>%
  mutate(wtd_pct_text_step1 = paste0(round(wtd_pct, 0), "%"),
         wtd_pct_text = ifelse(wtd_pct >= 9, wtd_pct_text_step1, "")) %>%
  ungroup()
  

# Plot 1: Distribution of Work Arrangements by Year ----------------------
ggplot(work_arr_by_year_w, aes(x = factor(survey_year), y = wtd_pct, fill = work_arr)) +
  geom_col(position = "stack", width = 0.5) + 
  geom_text(aes(label = wtd_pct_text),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4,
            fontface = "bold") +
  labs(title = "Observed Distribution of Work Arrangements by Year (Weighted)",
       x = "Year",
       y = "Percent",
       fill = "Work Arrangement") +
  scale_fill_manual(values = c("Always In-Person" = "gray45", 
                               "Hybrid" = "darkmagenta", 
                               "Always Remote" = "darkorange1"))

ggsave("./outputs/work_arr_by_year_w.jpg", width = 10, height = 6, units = "in")


# Plot 2: VMT by Work Arrangement and VMT Type ----------------------------
vmt_by_work_arr_vmt_type_w <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  dplyr::summarize(work_vmt_weekly_wtd_mean = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            nonwork_tour_vmt_weekly_wtd_mean = weighted.mean(nonwork_tour_vmt_weekly, person_weight, na.rm = T)) %>%
  pivot_longer(cols = c(work_vmt_weekly_wtd_mean, nonwork_tour_vmt_weekly_wtd_mean),
               names_to = "vmt_type",
               values_to = "vmt") %>%
  mutate(vmt_type = ifelse(vmt_type == "work_vmt_weekly_wtd_mean", "Work Tour", "Non-Work Tour"),
         vmt_label = ifelse(vmt > 10, round(vmt, 0), NA))

ggplot(vmt_by_work_arr_vmt_type_w, aes(x = factor(survey_year), y = vmt, fill = vmt_type)) +
  geom_bar(stat = "identity", position = "stack") +
  # Group the plot into separate panels for each category
  facet_wrap(~ work_arr) +
  
  # Add the absolute value labels to the segments 
  geom_text(
    aes(label = vmt_label),
    position = position_stack(vjust = 0.5), 
    color = "white", 
    size = 4,
    fontface = "bold"
  ) +
  
  # Customize labels and use theme_minimal()
  labs(
    title = "Average Weekly Total VMT by Work Arrangement and VMT Type (Weighted)",
    x = "Year",
    y = "VMT",
    fill = "VMT Type"
  ) +
  theme(
    # Set the style for the facet titles (category labels)
    strip.text = element_text(
      size = 12,      # Increase the font size
      face = "bold"   # Make the text bold
    )
  ) +
  scale_fill_manual(values = c("Non-Work Tour" = "goldenrod3",
                               "Work Tour" = "navyblue"))
  

ggsave("./outputs/vmt_by_work_arr_vmt_type_w.jpg", width = 6, height = 10, units = "in")


# Plot 3: Work Arrangement by Region ------------------------------------------
work_arr_by_region <- work_arr_by_region_load %>%
  mutate(wtd_pct_text_step1 = paste0(round(pct*100, 0), "%"),
         wtd_pct_text = ifelse(pct > 0.1, wtd_pct_text_step1, ""),
         region_year = str_replace(region_year, " (?=\\d)", "\n")) %>%
  ungroup()


work_arr_by_region$region_year <- factor(work_arr_by_region$region_year, levels = c("Bay Area\n2019", "Seattle\n2019", 
                                                                                    "Minneapolis\n2019", 
                                           "Bay Area\n2023", "Seattle\n2023", "Minneapolis\n2023"))

work_arr_by_region$work_arr <- factor(work_arr_by_region$work_arr, levels = c("Always In-Person", "Hybrid", "Always Remote"))

ggplot(work_arr_by_region, aes(x = region_year, y = pct*100, fill = work_arr)) +
  geom_col(position = "stack", width = 0.5) + 
  geom_text(aes(label = wtd_pct_text),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4,
            fontface = "bold") +
  labs(title = "Observed Distribution of Work Arrangements by Year and Region",
       x = "Region/Year",
       y = "Percent",
       fill = "Work Arrangement") +
  scale_fill_manual(values = c("Always In-Person" = "gray45", 
                               "Hybrid" = "darkmagenta", 
                               "Always Remote" = "darkorange1"))

ggsave("./outputs/work_arr_by_region.jpg", width = 10, height = 6, units = "in")


# Plot VMT Per Capita US --------------------------------------------------
mn_baseline <- 10691
mn_reduction <- 9195
co_baseline <- 9302
co_reduction <- 8651
ca_baseline <- 8979
ca_reduction <- 6734

ggplot(us_vmt_per_capita, aes(x = year, y = vmt_per_capita)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point() +
  labs(title = "United States VMT Per Capita by Year",
       x = "Year",
       y = "VMT Per Capita") +
  # ## MN Baseline
  # # Add the horizontal benchmark line
  # annotate("point", x = 2019, y = mn_baseline, color = "firebrick", size = 4) +
  # # Add the text label for the benchmark
  # annotate("text",
  #          x = 2010, # Places label at the start of the x-axis
  #          y = mn_baseline - 20,
  #          label = "2019 MN Baseline",
  #          vjust = 0,      # Positions text slightly above the line
  #          hjust = 0,         # Left-aligns text
  #          color = "firebrick",
  #          fontface = "bold",
  #          size = 6) +
  # ## MN Reduction
  # annotate("point", x = 2019, y = mn_reduction, color = "blueviolet", size = 4) +
  # annotate("text",
  #          x = 2004, # Places label at the start of the x-axis
  #          y = mn_reduction - 100,
  #          label = "2040 MN Reduction Target (14%\ndecrease from 2019 baseline)",
  #          vjust = -0.5,      # Positions text slightly above the line
  #          hjust = 0,         # Left-aligns text
  #          color = "blueviolet",
  #          fontface = "bold",
  #          size = 6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 17),
                             plot.title = element_text(size = 20)) +
  ylim(8500, 10750)

ggsave("./outputs/us_vmt_per_capita.jpg", width = 7, height = 9, units = "in")

  


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
vmt_weekly_df$nonwork_tour_vmt_weekly <- vmt_weekly_df$vmt_weekly - vmt_weekly_df$work_tour_vmt_weekly_df

## Weighted ------------
vmt_weekly_df_work_nonwork_w <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  dplyr::summarize(work_vmt = weighted.mean(work_tour_vmt_weekly_df, person_weight, na.rm = T),
            nonwork_vmt = weighted.mean(nonwork_tour_vmt_weekly, person_weight, na.rm = T)) %>%
  gather(key = vmt_type, value = vmt_weekly, work_vmt:nonwork_vmt)

vmt_weekly_df_work_nonwork_w$vmt_type <- ifelse(vmt_weekly_df_work_nonwork_w$vmt_type == "work_vmt", "Work VMT", "Non-Work VMT")
#vmt_weekly_df_work_nonwork_w$survey_year <- factor(vmt_weekly_df_work_nonwork_w$survey_year, c(2019, 2021, 2023))

p1 <- ggplot(vmt_weekly_df_work_nonwork_w, aes(fill = work_arr, x = work_arr, y = vmt_weekly, pattern = vmt_type)) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black", # color of the pattern itself
                   pattern_colour = "black", # border color of the pattern
                   pattern_density = 0.01, # density of the pattern
                   pattern_spacing = 0.01, # spacing between pattern elements
                   pattern_angle = 45) +
  scale_pattern_manual(values = c(NA, "stripe")) + xlab("Work Arrangement") + ylab("Average Weekly Total VMT") +
  ggtitle("Average Weekly Total VMT by Work Arrangement and VMT Type (Weighted)") +
  guides(fill = guide_legend(title = "Work Arrangement", 
    override.aes = list(
      pattern = c("none", "none", "none") # "none" for Category A, others retain patterns
    )
  ), pattern = guide_legend("VMT Type",
                            override.aes = list(fill = c("lightgrey", "lightgrey")))) +
  geom_text(aes(label = round(vmt_weekly)), size = 4, vjust = -0.25, position = position_dodge(.9)) 

p1
ggsave("./outputs/mean_weekly_work_nonwork_vmt_w.jpg", width = 8, height = 6, units = "in")


## Unweighted --------------------------------------------------------------
vmt_weekly_df_work_nonwork_u <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  dplyr::summarize(work_vmt = mean(work_tour_vmt_weekly_df),
            nonwork_vmt = mean(nonwork_tour_vmt_weekly)) %>%
  gather(key = vmt_type, value = vmt_weekly, work_vmt:nonwork_vmt)

vmt_weekly_df_work_nonwork_u$vmt_type <- ifelse(vmt_weekly_df_work_nonwork_u$vmt_type == "work_vmt", "Work VMT", "Non-Work VMT")
vmt_weekly_df_work_nonwork_u$survey_year <- factor(vmt_weekly_df_work_nonwork_u$survey_year, c(2019, 2021, 2023))

p2 <- ggplot(vmt_weekly_df_work_nonwork_u, aes(fill = work_arr, x = survey_year, y = vmt_weekly, pattern = vmt_type)) + 
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
  geom_text(aes(label = round(vmt_weekly, 2)), size = 2.5, vjust = -0.25, position = position_dodge(.9))


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
  dplyr::summarize(vmt_weekly_df = sum(vmt)) %>%
  group_by(survey_year, work_arr, d_purpose_category) %>%
  dplyr::summarize(vmt_weekly_df_w = weighted.mean(vmt_weekly_df, person_weight, na.rm = T),
            vmt_weekly_df_u = mean(vmt_weekly_df, na.rm = T))

work_arr_purpose$survey_year <- factor(work_arr_purpose$survey_year, c(2019, 2021, 2023))

## Plot Data --------------------------------------------------------------
for(i in c(2019, 2021, 2023)){
  print(work_arr_purpose %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr, y = vmt_weekly_df_w)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Weekly Total VMT by Purpose and Work Arrangement (", i, ", Weighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(vmt_weekly_df_w, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 60))
  ggsave(paste0("./outputs/w_weekly_total_vmt_purpose_work_arr_", i, ".jpg"), width = 8, height = 6, units = "in")
}

for(i in c(2019, 2021, 2023)){
  print(work_arr_purpose %>% filter(survey_year == i) %>% ggplot(aes(fill = d_purpose_category, x = work_arr, y = vmt_weekly_df_u)) +
          geom_bar(position="dodge", stat="identity") + ggtitle(paste0("Average Weekly Total VMT by Purpose and Work Arrangement (", i, ", Unweighted)")) +
          xlab("Work Arrangement") + ylab("VMT") + guides(fill="none") +
          geom_text(aes(label = paste0(d_purpose_category, " (", round(vmt_weekly_df_u, 1), ")")), size = 2.5, vjust = -0.25, angle = 90, hjust = -0.05, position = position_dodge(0.9)) +
          ylim(0, 60))
  ggsave(paste0("./outputs/u_weekly_total_vmt_purpose_work_arr_", i, ".jpg"), width = 8, height = 6, units = "in")
}

# Work Arrangements by Employment Status --------------------------------------
## Construct Data -------------------------------------------------------------
work_arr_emp_status_w <- work_arr_all %>%
  inner_join(person_weights) %>%
  group_by(survey_year, work_arr, employment) %>%
  dplyr::summarize(group_sum = sum(person_weight, na.rm = T)) %>%
  mutate(group_total = sum(group_sum),
         percent = group_sum / group_total)

work_arr_emp_status_w$survey_year <- factor(work_arr_emp_status_w$survey_year, c(2019, 2021, 2023))

work_arr_emp_status_u <- work_arr_all %>%
  group_by(survey_year, work_arr, employment) %>%
  dplyr::summarize(count = n()) %>%
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
  dplyr::summarize(N = n(), wtd_N = sum(person_weight)) %>%
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
  dplyr::summarize(weighted_vmt = weighted.mean(vmt, person_weight, na.rm = T),
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
  dplyr::summarize(weighted_vmt = weighted.mean(vmt, person_weight, na.rm = T),
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
  dplyr::summarize(N = n(), wtd_N = sum(person_weight)) %>%
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


