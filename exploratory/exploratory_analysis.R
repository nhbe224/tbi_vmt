### Exploratory Analysis for TBI VMT
# Set Working Directory ---------------------------------------------------
setwd("D:/neeco/thesis/tbi_vmt/exploratory/")

# Load Packages -----------------------------------------------------------
packages_vector <- c("tidyverse", "sf", "tigris", "tidycensus", "data.table", "patchwork", 
                     "janitor", "tools", "spatstat", "gridExtra", "grid", "gtable", "ggpattern")
need_to_install <- packages_vector[!(packages_vector %in% installed.packages()[,"Package"])]
if (length(need_to_install)) install.packages(need_to_install)
lapply(packages_vector, library, character.only = TRUE)
options(scipen = 999)


# Read in Data ------------------------------------------------------------
vmt_weekly_df <- read.csv("../datacleaning/outputs/vmt_weekly_df.csv")


# Plot Data -------------------------------------------------------------
vmt_weekly_df_mean <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

vmt_weekly_df_mean_by_year <- vmt_weekly_df %>%
  group_by(work_arr, survey_year) %>%
  summarize(vmt_weekly_wtd_mean = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            vmt_weekly_mean = mean(vmt_weekly, na.rm = T))

vmt_weekly_df_mean_by_year$survey_year <- factor(vmt_weekly_df_mean_by_year$survey_year, c(2019, 2021, 2023))

ggplot(vmt_weekly_df_mean, aes(fill = work_arr, x = work_arr, y = vmt_weekly_wtd_mean)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Weighted)") +
  xlab("Work Arrangement") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(vmt_weekly_wtd_mean)), size = 4, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 215)
ggsave("./outputs/weighted_vmt_weekly_no_year.jpg", width = 8, height = 6, units = "in")

ggplot(vmt_weekly_df_mean_by_year, aes(fill = work_arr, x = survey_year, y = vmt_weekly_wtd_mean)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Weighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(vmt_weekly_wtd_mean)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 215)
ggsave("./outputs/weighted_vmt_weekly.jpg", width = 8, height = 6, units = "in")

unweighted_vmt_weekly_df$survey_year <- factor(unweighted_vmt_weekly_df$survey_year, c(2019, 2021, 2023))

ggplot(unweighted_vmt_weekly_df, aes(fill = work_arr, x = survey_year, y = mean_vmt_weekly)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Average Weekly VMT by Work Arrangement (Unweighted)") +
  xlab("Year") + ylab("Average Weekly VMT") + guides(fill=guide_legend(title="")) + theme(legend.position="bottom") +
  geom_text(aes(label = round(mean_vmt_weekly)), size = 3, vjust = -0.25, position = position_dodge(.9)) +
  ylim(0, 205)
ggsave("./outputs/unweighted_vmt_weekly.jpg", width = 8, height = 6, units = "in")


# Rebound Effects ---------------------------------------------------------
### Always remote: 140.96-4.69 = 133.31 savings in mean work VMT
### Always remote: 138-64.3 = 73.7 increase in mean non work VMT
### rebound effect 73.1/133.1 = 55% rebound effect. Net savings overall.

### Hybrid: 138-87 = 51 savings in mean work VMT
### Hybrid: 89.2-64.3 = 24.9 savings in mean non work VMT
### Rebound effect = 48% rebound effect

mean_vmt_purpose_arr <- vmt_weekly_df %>%
  group_by(work_arr) %>%
  summarize(mean_vmt_weekly = weighted.mean(vmt_weekly, person_weight, na.rm = T),
            mean_work_tour_vmt = weighted.mean(work_tour_vmt_weekly_df, person_weight, na.rm = T),
            mean_nonwork_tour_vmt = weighted.mean(vmt_weekly - work_tour_vmt_weekly_df, person_weight, na.rm = T))

mean_vmt_aip <- mean_vmt_purpose_arr %>%
  filter(work_arr == "Always In-Person")

mean_vmt_not_aip <- mean_vmt_purpose_arr %>%
  filter(work_arr != "Always In-Person")

## Create always in-person dataframe to get rebound effects.
mean_vmt_aip <- rbind(mean_vmt_aip, mean_vmt_aip)
mean_vmt_aip[c(1), 1] <- "Always Remote"
mean_vmt_aip[c(2), 1] <- "Hybrid"
colnames(mean_vmt_aip)[c(2:4)] <- c("aip_mean_vmt_weekly", "aip_mean_work_tour_vmt", "aip_mean_nonwork_tour_vmt")

## Rebound effect dataframe
rebound_effect <- mean_vmt_not_aip %>% inner_join(mean_vmt_aip) %>%
  mutate(total_vmt_change = round(mean_vmt_weekly - aip_mean_vmt_weekly),
         total_vmt_pct_change = round((mean_vmt_weekly - aip_mean_vmt_weekly) / (aip_mean_vmt_weekly), 2),
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
weighted_commute_vmt <- commute_vmt_weekly_df %>%
  left_join(work_arr) %>%
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

