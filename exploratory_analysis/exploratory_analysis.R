### Exploratory Analysis for TBI VMT
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/exploratory_analysis/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", "patchwork", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable", "ggpattern")
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

# Clean Data ---------------------------------------------------------------
vmt_weekly_df_mean <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

vmt_weekly_df_mean_by_year <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

work_vmt_weekly_df_mean_by_year <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  summarize(work_vmt_weekly_wtd_mean = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            work_vmt_weekly_mean = mean(work_tour_vmt_weekly, na.rm = T))


# Work Arrangement Pct by Year --------------------------------------------
work_arr_by_year <- vmt_weekly_df %>% 
  group_by(survey_year, work_arr) %>%
  summarize(count = n(), .groups = 'drop_last') %>%
  mutate(pct = (count / sum(count))) %>%
  ungroup()

work_arr_by_year_w <- vmt_weekly_df %>%
  group_by(survey_year, work_arr) %>%
  summarize(
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
         wtd_pct_text = ifelse(wtd_pct > 10, wtd_pct_text_step1, "")) %>%
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
  summarize(work_vmt_weekly_wtd_mean = weighted.mean(work_tour_vmt_weekly, person_weight, na.rm = T),
            nonwork_tour_vmt_weekly_wtd_mean = weighted.mean(nonwork_tour_vmt_weekly, person_weight, na.rm = T)) %>%
  pivot_longer(cols = c(work_vmt_weekly_wtd_mean, nonwork_tour_vmt_weekly_wtd_mean),
               names_to = "vmt_type",
               values_to = "vmt") %>%
  mutate(vmt_type = ifelse(vmt_type == "work_vmt_weekly_wtd_mean", "Work", "Non-Work"),
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
  scale_fill_manual(values = c("Non-Work" = "goldenrod3",
                               "Work" = "navyblue"))
  

ggsave("./outputs/vmt_by_work_arr_vmt_type_w.jpg", width = 10, height = 6, units = "in")


# Plot Mean Weekly VMT by Work Arrangement (weighted) ------------------------
ggplot(vmt_weekly_df_mean, aes(fill = work_arr, x = work_arr, y = vmt_weekly_wtd_mean)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Weighted)") +
  xlab("Work Arrangement") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(vmt_weekly_wtd_mean)), size = 4, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 215)
ggsave("./outputs/weighted_vmt_weekly_no_year.jpg", width = 8, height = 6, units = "in")


# Plot Mean Weekly VMT by Work Arrangement and Year (Weighted) ------------
ggplot(vmt_weekly_df_mean_by_year, aes(fill = work_arr, x = survey_year, y = vmt_weekly_wtd_mean)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Weighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(vmt_weekly_wtd_mean)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 215)
ggsave("./outputs/weighted_vmt_weekly.jpg", width = 8, height = 6, units = "in")

# Plot Mean Weekly VMT by Work Arrangement and Year (Unweighted) ------------
ggplot(vmt_weekly_df_mean_by_year, aes(fill = work_arr, x = survey_year, y = vmt_weekly_mean)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Unweighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(vmt_weekly_mean)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 205)
ggsave("./outputs/unweighted_vmt_weekly.jpg", width = 8, height = 6, units = "in")

# Average Work Tour VMT by Work Arrangement -------------------------------------
## Construct Data ----------------------------------------------------------
weighted_commute_vmt <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  summarize(w_mean_work_tour_vmt = weighted.mean(work_tour_vmt_weekly_df, person_weight, na.rm = T))

weighted_commute_vmt$survey_year <- factor(weighted_commute_vmt$survey_year, c(2019, 2021, 2023))

unweighted_commute_vmt <-  commute_vmt_weekly_df %>%
  left_join(work_arr) %>%
  group_by(work_arr, survey_year) %>%
  summarize(u_mean_work_tour_vmt = mean(work_tour_vmt_weekly_df, na.rm = T))

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
vmt_weekly_df$nonwork_tour_vmt_weekly <- vmt_weekly_df$vmt_weekly - vmt_weekly_df$work_tour_vmt_weekly_df

## Weighted ------------
vmt_weekly_df_work_nonwork_w <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  summarize(work_vmt = weighted.mean(work_tour_vmt_weekly_df, person_weight, na.rm = T),
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
  summarize(work_vmt = mean(work_tour_vmt_weekly_df),
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
  summarize(vmt_weekly_df = sum(vmt)) %>%
  group_by(survey_year, work_arr, d_purpose_category) %>%
  summarize(vmt_weekly_df_w = weighted.mean(vmt_weekly_df, person_weight, na.rm = T),
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

