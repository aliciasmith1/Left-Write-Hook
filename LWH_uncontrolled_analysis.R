## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(effectsize)
library(scales)
library(ggpubr)


## ----Load & wrangle dataset---------------------------------------------------
qualtrics_dataDesc <- read_csv("LWH_UT_data.csv", show_col_types = F)

# remove column descriptions
qualtrics_data <- qualtrics_dataDesc[-1,]

# rename columns for clarity
rename_vals <- c(email = "Q6", gender = "Q7", age = "Q9", time.point = "Q19", vigorous_PE = "Q13", moderate_PE = "Q15", walk = "Q16")
d1 <- rename(qualtrics_data, all_of(rename_vals))

# re-label time point data
d1$time.point <- factor(d1$time.point, levels = c(1, 2), labels = c("Pre", "Post"))


## ----Participant count--------------------------------------------------------
# create a unique ID for each email address
d1 <- transform(d1,id=as.numeric(factor(email))) |>
        relocate(id, .before = email) |>
        relocate(time.point, .Post = email)



# count total number of responses at each time point
totalResponses <- d1 |> count(time.point) 
print(paste0("The total number of survey responses at baseline: ", totalResponses[1,2]))
print(paste0("The total number of survey responses at follow-up: ", totalResponses[2,2]))



# count number of participants who did not fully complete survey
d1$Progress <- as.numeric(d1$Progress)
incompleteResponses <- d1 |> filter(Progress < 90) |>
                              count(time.point)
print(paste0("The total number of incomplete surveys at baseline: ", incompleteResponses[1,2]))



# count number of participants that completed both time points
fullCompleters <- d1 |> dplyr::filter(n() == 2, .by = id) |>
                        dplyr::summarize(count = n_distinct(id))
print(paste0("The number of participants who completed the survey at both time points: ", fullCompleters))


## ----Identify groups & time between surveys-----------------------------------


# for each participant, calculate number of days between T1 and T2
d1_dates <- d1 |> subset(select = c(id, StartDate, time.point)) |>
                  spread(time.point, StartDate)
d1_dates <- d1_dates[order(as.Date(d1_dates$Pre, format="%d/%m/%Y %H:%M")),]

d1_dates$date_diff <- as.Date(as.character(d1_dates$Post), format="%d/%m/%Y %H:%M")-
                  as.Date(as.character(d1_dates$Pre), format="%d/%m/%Y %H:%M")

fullCompletion <- d1_dates |> count(date_diff >= 49)
print(paste0("The number of participants who took more than 7 weeks between surveys: ", fullCompletion[2,2]))



# create group variable based on start dates
d1_dates$start_month <- format(as.Date(d1_dates$Pre), "%m")
d1_dates$group <- ifelse(d1_dates$start_month == "07", 1, ifelse(d1_dates$start_month == "10", 2, 3))
groups <- d1_dates |> count(group)

print(paste0("The number of groups: ", groups[3,1]))
print(paste0("The number of participants in group 1: ", groups[1,2]))
print(paste0("The number of participants in group 2: ", groups[2,2]))
print(paste0("The number of participants in group 3: ", groups[3,2]))


## ----Store data---------------------------------------------------------------

# add this data to d1 dataset
d1_dates_subset <- d1_dates |>
                      subset(select = c(id, date_diff, group))
d2 <- merge(d1, d1_dates_subset, by="id")
d2 <- d2 |> 
        relocate(date_diff, .after = time.point) |>
        relocate(group, .after = id)

## ----Score questionnaires-----------------------------------------------------
# DASS (3 subscales + total): Should be scored 0-3
# Q17 = ITQ PTSD (3 subscales + total): only 1 - 6 used for dimensional scoring. Should be scored 0-4
# Q18 = ITQ DSO (3 subscales + total): only 1 - 6 used. Should be scored 0-4
# Q19 = TMQQ (total): missing q8. Reverse score q6. Correct scoring.
# Q20 = PTCI (3 subscales + total). Correct scoring.
# Q21 = sMHC (total). Should be scored 0-5


# transform data to numeric
d2[, 27:126] <- lapply(d2[, 27:126], as.numeric)


# reverse score q6 in TMQ
d2$Q19_6 = 5 - d2$Q19_6  

# rescore questionnaires are incorrectly scored 
rescore <- c("DASS-21_1", "DASS-21_2", "DASS-21_3", "DASS-21_4", "DASS-21_5", "DASS-21_6", "DASS-21_7", "DASS-21_8", "DASS-21_9", "DASS-21_10", 
             "DASS-21_11", "DASS-21_12", "DASS-21_13", "DASS-21_14", "DASS-21_15", "DASS-21_16", "DASS-21_17", "DASS-21_18", "DASS-21_19", "DASS-21_20", "DASS-21_21",
              "Q17_1", "Q17_2", "Q17_3", "Q17_4", "Q17_5", "Q17_6", "Q17_7", "Q17_8", "Q17_9", # rescore ITQ PTSD items
              "Q18_1", "Q18_2", "Q18_3", "Q18_4", "Q18_5", "Q18_6", "Q18_7", "Q18_8", "Q18_9", # rescore ITQ DSO items
              "Q21_1", "Q21_2", "Q21_3", "Q21_4", "Q21_5", "Q21_6", "Q21_7", "Q21_8", "Q21_9", "Q21_10", "Q21_11", "Q21_12", "Q21_13", "Q21_14") # rescore sHMC items

d2[rescore] <- d2[rescore] - 1

# calculate total scores on each scale
d2 <- d2 |> mutate(DASS_total = rowSums(across('DASS-21_1': 'DASS-21_21')), 
                    DASS_dep_total = rowSums(across(c('DASS-21_3', 'DASS-21_5', 'DASS-21_10', 'DASS-21_13', 'DASS-21_16', 'DASS-21_17', 'DASS-21_21'))),
                    DASS_anx_total = rowSums(across(c('DASS-21_2', 'DASS-21_4', 'DASS-21_7', 'DASS-21_9', 'DASS-21_15', 'DASS-21_19', 'DASS-21_20'))),
                    DASS_stress_total = rowSums(across(c('DASS-21_1', 'DASS-21_6', 'DASS-21_8', 'DASS-21_11', 'DASS-21_12', 'DASS-21_14', 'DASS-21_18'))),
                    ITQ_PTSD_total = rowSums(across(Q17_1:Q17_6)),  
                    ITQ_PTSD_Re_total = rowSums(across(Q17_1:Q17_2)),
                    ITQ_PTSD_Av_total = rowSums(across(Q17_3:Q17_4)),
                    ITQ_PTSD_Th_total = rowSums(across(Q17_5:Q17_6)),
                    ITQ_DSO_total = rowSums(across(Q18_1:Q18_6)), 
                    ITQ_DSO_AD_total = rowSums(across(Q18_1:Q18_2)),
                    ITQ_DSO_NSC_total = rowSums(across(Q18_3:Q18_4)),
                    ITQ_DSO_DR_total = rowSums(across(Q18_5:Q18_6)),
                    TMQQ_total = rowSums(across(Q19_1:Q19_10)), # missing q8
                    PTCI_total = rowSums(across(Q20_1:Q20_33)),
                    PTCI_cogSelf_total = rowSums(across(c(Q20_2,Q20_3,Q20_4,Q20_5,Q20_6,Q20_9,Q20_12,Q20_13,
                                                          Q20_15,Q20_16,Q20_19,Q20_20,Q20_23,Q20_24,Q20_25,Q20_27,
                                                          Q20_28,Q20_29,Q20_31,Q20_32,Q20_33))),
                    PTCI_cogWorld_total = rowSums(across(c(Q20_7,Q20_8,Q20_10,Q20_11,Q20_17,Q20_22,Q20_26))),
                    PTCI_selfBlame_total = rowSums(across(c(Q20_1,Q20_14,Q20_18,Q20_21,Q20_30))),
                    sMHC_total = rowSums(across(Q21_1:Q21_14))) 


## ----Score ITQ - PTSD diagnosis-----------------------------------------------

d2 <- d2 |> mutate(Re_dx = if_else(Q17_1 >= 2 | Q17_2 >= 2, 1, 0)) |>
            mutate(Av_dx = if_else(Q17_3 >= 2 | Q17_4 >= 2, 1, 0)) |>
            mutate(Th_dx = if_else(Q17_5 >= 2 | Q17_6 >= 2, 1, 0)) |>
            mutate(PTSDFI = if_else(Q17_7 >= 2 | Q17_8 >= 2 | Q17_9 >= 2, 1, 0)) |>
            mutate(PTSD = if_else(Re_dx == 1 & Av_dx == 1 & Th_dx == 1 & PTSDFI == 1, 1, 0))

d2 <- d2 |> mutate(AD_dx = if_else(Q18_1 >= 2 | Q18_2 >= 2, 1, 0)) |>
            mutate(NSC_dx = if_else(Q18_3 >= 2 | Q18_4 >= 2, 1, 0)) |>
            mutate(DR_dx = if_else(Q18_5 >= 2 | Q18_6 >= 2, 1, 0)) |>
            mutate(DSOFI = if_else(Q18_7 >= 2 | Q18_8 >= 2 | Q18_9 >= 2, 1, 0)) |>
            mutate(DSO = if_else(AD_dx == 1 & NSC_dx == 1 & DR_dx == 1 & DSOFI == 1, 1, 0)) |>
            mutate(CPTSD_dx = if_else(PTSD == 1 & DSO == 1, 1, 0)) |>
            mutate(PTSD_dx = if_else(PTSD == 1 & DSO == 0, 1, 0))



## ----descriptive statistics---------------------------------------------------

demo <- d2 |> subset(select = c(id, time.point, gender, age, vigorous_PE, moderate_PE, walk))

demo[,c(4:7)]=sapply(demo[,c(4:7)], as.numeric)
              
demo = demo %>%
  mutate(gender = if_else(gender == "3", "non-binary", "female"))

demo_wide <- demo |> pivot_wider(
                        names_from = time.point,
                        values_from = c(
                          gender, 
                          age, 
                          vigorous_PE, 
                          moderate_PE, 
                          walk
                        )
                      ) |>
                    subset(select = -c(gender_Post, age_Post))


gender <- demo_wide |> 
  count(gender_Pre) |>                    
  mutate(percent = scales::percent(n / sum(n))) 
gender


desc_stat <- rstatix::get_summary_stats(demo_wide)
desc_stat





## ----Questionnaire descriptive stats------------------------------------------

full_d <- d2 |> dplyr::filter(n() == 2, .by = id) |>
                dplyr::filter(date_diff >= 49) # remove participant with < 7weeks between surveys

d_wide <- full_d |> pivot_wider(
            id_cols = id,
            names_from = time.point,
            values_from = c(
              vigorous_PE, 
              moderate_PE, 
              walk,
              DASS_total,
              DASS_dep_total,
              DASS_anx_total,
              DASS_stress_total,
              ITQ_PTSD_total, 
              ITQ_PTSD_Re_total, 
              ITQ_PTSD_Av_total, 
              ITQ_PTSD_Th_total, 
              ITQ_DSO_total, 
              ITQ_DSO_AD_total, 
              ITQ_DSO_NSC_total, 
              ITQ_DSO_DR_total, 
              TMQQ_total, 
              PTCI_total, 
              PTCI_cogSelf_total, 
              PTCI_cogWorld_total, 
              PTCI_selfBlame_total, 
              sMHC_total)
          )

# Get all pre and post variable names
pre_post_vars <- grep("_Pre$|_Post$", names(d_wide), value = TRUE)

d_long <- d_wide |>
  select(id, all_of(pre_post_vars)) |>
  tidyr::pivot_longer(
    cols = -id,
    names_to = c("measure", "time"),
    names_pattern = "^(.*)_(Pre|Post)$",
    values_to = "value"
  )

desc_stats <- d_long |>
  group_by(measure, time) |>
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )


print(desc_stats)
write_csv(desc_stats, "desc_stats.csv")


## ----Data analysis, message = FALSE-------------------------------------------


all_vars <- gsub("_Pre|_Post", "", grep("_Pre|_Post", names(d_wide), value = TRUE))
unique_vars <- unique(all_vars)

ttest_results <- lapply(unique_vars, function(var) {
  t.test(d_wide[[paste0(var, "_Post")]], d_wide[[paste0(var, "_Pre")]], paired = TRUE)
})

names(ttest_results) <- unique_vars
ttest_results

cohensd_results <- lapply(unique_vars, function(var) {
  effectsize::cohens_d(d_wide[[paste0(var, "_Post")]], d_wide[[paste0(var, "_Pre")]], pooled_sd = F, paired = TRUE)
})

names(cohensd_results) <- unique_vars
cohensd_results


## ----PTSD dx descriptive stats and analysis-----------------------------------

ptsd_wide <- full_d |> pivot_wider(
            id_cols = id,
            names_from = time.point,
            values_from = c(
              PTSD_dx,
              CPTSD_dx))
write_csv(ptsd_wide, 'ptsd_dx.csv')              
              
# PTSD diagnosis descriptive statistics
ptsd_desc <- full_d %>%
  filter(!is.na(PTSD_dx)) %>%
  group_by(time.point) %>%
  summarise(
    n = n(),
    n_ptsd = sum(PTSD_dx),
    percent_ptsd = round(100 * n_ptsd / n, 1)
  )

print("PTSD diagnosis counts and percentages:")
print(ptsd_desc)

# CPTSD diagnosis descriptive statistics
cptsd_desc <- full_d %>%
  filter(!is.na(CPTSD_dx)) %>%
  group_by(time.point) %>%
  summarise(
    n = n(),
    n_cptsd = sum(CPTSD_dx),
    percent_cptsd = round(100 * n_cptsd / n, 1)
  )

print("CPTSD diagnosis counts and percentages:")
print(cptsd_desc)


# PTSD dx analysis

# PTSD diagnosis at pre and post

table(full_d$time.point, full_d$PTSD_dx)
ptsd_counts <- aggregate(PTSD_dx ~ time.point, data = full_d, sum)


# Logistic regression

PTSD_glm_model <- glm(PTSD_dx ~ time.point, data = full_d, family = binomial)
summary(PTSD_glm_model)

# Odds ratio
exp(coef(PTSD_glm_model))

# 95% confidence intervals
exp(confint(PTSD_glm_model)) 

# CPTSD dx analysis

# CPTSD diagnosis at pre and post

table(full_d$time.point, full_d$CPTSD_dx)
cptsd_counts <- aggregate(CPTSD_dx ~ time.point, data = full_d, sum)

# Logistic regression
CPTSD_glm_model <- glm(CPTSD_dx ~ time.point, data = full_d, family = binomial)
summary(CPTSD_glm_model)

# odds ratio
exp(coef(CPTSD_glm_model)) 

# 95% CI
exp(confint(CPTSD_glm_model)) 

## ----Plots--------------------------------------------------------------------

# PTSD dx plots

 PTSD_dx_plot <- ggplot(data = ptsd_counts, aes(x= time.point, y= PTSD_dx, fill = time.point))+
   stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
   scale_fill_manual(values = c("gold", "khaki1")) + 
   scale_y_continuous(labels = number_format(accuracy = 1)) + 
   theme_classic() +
   theme(plot.title = element_text(size = 12)) +
   expand_limits(y=0) +
   ggtitle("PTSD diagnosis") +
   labs(x="Time Point", y="N")
 
 # PTSD dx plots
 
CPTSD_dx_plot <- ggplot(data = cptsd_counts, aes(x= time.point, y= CPTSD_dx, fill = time.point))+
   stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
   scale_fill_manual(values = c("gold", "khaki1")) + 
   scale_y_continuous(labels = number_format(accuracy = 1)) + 
   theme_classic() +
   theme(plot.title = element_text(size = 12)) +
   expand_limits(y=0) +
   ggtitle("CPTSD diagnosis") +
   labs(x="Time Point", y="N")

# Main plots

DASS_plot <- ggplot(data = full_d, aes(x= time.point, y= DASS_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Depression, Anxiety, Stress") +
  labs(x="Time Point", y="Total Score")


ITQ_PTSD_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_PTSD_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("PTSD symptoms") +
  labs(x="Time Point", y="Total Score")


ITQ_DSO_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_DSO_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Disturbances in Self Organization") +
  labs(x="Time Point", y="Total Score")


TMQQ_plot <- ggplot(data = full_d, aes(x= time.point, y= TMQQ_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Trauma Memory Quality") +
  labs(x="Time Point", y="Total Score")


PTCI_plot <- ggplot(data = full_d, aes(x= time.point, y= PTCI_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Post Traumatic Cognitions") +
  labs(x="Time Point", y="Total Score")


sMHC_plot <- ggplot(data = full_d, aes(x= time.point, y= sMHC_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Wellbeing") +
  labs(x="Time Point", y="Total Score")


main_plot <-
  ggarrange(DASS_plot, ITQ_PTSD_plot, ITQ_DSO_plot, TMQQ_plot, PTCI_plot, sMHC_plot,
         labels = c("A", "B", "C", "D", "E", "F"),
         ncol = 3, nrow = 2)
main_plot

PTSD_plot <- 
  ggarrange(PTSD_dx_plot, CPTSD_dx_plot, ITQ_PTSD_plot, ITQ_DSO_plot,
         labels = c("A", "B", "C", "D"),
         ncol = 2, nrow = 2)
PTSD_plot

mech_plot <-
    ggarrange(TMQQ_plot, PTCI_plot,
         labels = c("A", "B"),
         ncol = 2, nrow = 1)
mech_plot

otherSymp_plot <-
      ggarrange(DASS_plot, sMHC_plot,
         labels = c("A", "B"),
         ncol = 2, nrow = 1)
otherSymp_plot

# DASS Subscales

DASS_dep_plot <- ggplot(data = full_d, aes(x= time.point, y= DASS_dep_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Depression") +
  labs(x="Time Point", y="Total Score")

DASS_anx_plot <- ggplot(data = full_d, aes(x= time.point, y= DASS_anx_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Anxiety") +
  labs(x="Time Point", y="Total Score")

DASS_stress_plot <- ggplot(data = full_d, aes(x= time.point, y= DASS_stress_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Stress") +
  labs(x="Time Point", y="Total Score")

DASS_sub_plots <-
  ggarrange(DASS_dep_plot, DASS_anx_plot, DASS_stress_plot, 
         labels = c("A", "B", "C"),
         ncol = 3, nrow = 1)
DASS_sub_plots

# ITQ PTSD sub plots

ITQ_PTSD_Re_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_PTSD_Re_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Re-experiencing") +
  labs(x="Time Point", y="Total Score")


ITQ_PTSD_Av_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_PTSD_Av_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Avoidance") +
  labs(x="Time Point", y="Total Score")


ITQ_PTSD_Th_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_PTSD_Th_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 12)) +
  expand_limits(y=0) +
  ggtitle("Sense of Current Threat") +
  labs(x="Time Point", y="Total Score")


ITQ_PTSD_sub_plots <-
  ggarrange(ITQ_PTSD_Re_plot, ITQ_PTSD_Av_plot, ITQ_PTSD_Th_plot, 
         labels = c("A", "B", "C"),
         ncol = 3, nrow = 1)
ITQ_PTSD_sub_plots

# ITQ DSO sub plots

ITQ_DSO_AD_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_DSO_AD_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10)) +
  expand_limits(y=0) +
  ggtitle("Affective Dysregulation") +
  labs(x="Time Point", y="Total Score")


ITQ_DSO_NSC_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_DSO_NSC_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10)) +
  expand_limits(y=0) +
  ggtitle("Negative self-concept") +
  labs(x="Time Point", y="Total Score")


ITQ_DSO_DR_plot <- ggplot(data = full_d, aes(x= time.point, y= ITQ_DSO_DR_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10)) +
  expand_limits(y=0) +
  ggtitle("Disturbances in Relationships") +
  labs(x="Time Point", y="Total Score")


ITQ_DSO_sub_plots <-
  ggarrange(ITQ_DSO_AD_plot, ITQ_DSO_NSC_plot, ITQ_DSO_DR_plot, 
         labels = c("A", "B", "C"),
         ncol = 3, nrow = 1)
ITQ_DSO_sub_plots

# PTCI sub plots

PTCI_cogSelf_plot <- ggplot(data = full_d, aes(x= time.point, y= PTCI_cogSelf_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  expand_limits(y=0) +
  ggtitle("Negative Cognitions About Self") +
  labs(x="Time Point", y="Total Score")


PTCI_cogWorld_plot <- ggplot(data = full_d, aes(x= time.point, y= PTCI_cogWorld_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  expand_limits(y=0) +
  ggtitle("Negative Cognitions About World") +
  labs(x="Time Point", y="Total Score")


PTCI_selfBlame_plot <- ggplot(data = full_d, aes(x= time.point, y= PTCI_selfBlame_total, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10)) +
  expand_limits(y=0) +
  ggtitle("Self-blame") +
  labs(x="Time Point", y="Total Score")


PTCI_sub_plots <-
  ggarrange(PTCI_cogSelf_plot, PTCI_cogWorld_plot, PTCI_selfBlame_plot, 
         labels = c("A", "B", "C"),
         ncol = 3, nrow = 1)
PTCI_sub_plots

# Exercise plots

vig_PE_plot <- ggplot(data = full_d, aes(x= time.point, y= vigorous_PE, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  expand_limits(y=0) +
  ggtitle("Vigorous Physical Exercise") +
  labs(x="Time Point", y="Total Score")


mod_PE_plot <- ggplot(data = full_d, aes(x= time.point, y= moderate_PE, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 9)) +
  expand_limits(y=0) +
  ggtitle("Moderate Physical Exercise") +
  labs(x="Time Point", y="Total Score")


walk_plot <- ggplot(data = full_d, aes(x= time.point, y= walk, fill = time.point))+
  stat_summary(geom = "bar", fun = mean, position = "dodge", show.legend = F) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
  scale_fill_manual(values = c("gold", "khaki1")) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10)) +
  expand_limits(y=0) +
  ggtitle("Walking") +
  labs(x="Time Point", y="Total Score")


exercise_plots <-
  ggarrange(vig_PE_plot, mod_PE_plot, walk_plot, 
         labels = c("A", "B", "C"),
         ncol = 3, nrow = 1)
exercise_plots

# ggsave("PTSD_dx_plot.png", plot = PTSD_plot, device='png', dpi=700)



## ----Re-analyse with extra participant----------------------------------------

full_d_extra <- d2 |> dplyr::filter(n() == 2, .by = id)

d_wide <- full_d_extra |> pivot_wider(
            id_cols = id,
            names_from = time.point,
            values_from = c(
              vigorous_PE, 
              moderate_PE, 
              walk,
              DASS_total,
              DASS_dep_total,
              DASS_anx_total,
              DASS_stress_total,
              ITQ_PTSD_total, 
              ITQ_PTSD_Re_total, 
              ITQ_PTSD_Av_total, 
              ITQ_PTSD_Th_total, 
              ITQ_DSO_total, 
              ITQ_DSO_AD_total, 
              ITQ_DSO_NSC_total, 
              ITQ_DSO_DR_total, 
              TMQQ_total, 
              PTCI_total, 
              PTCI_cogSelf_total, 
              PTCI_cogWorld_total, 
              PTCI_selfBlame_total, 
              sMHC_total)
          )

# Get all pre and post variable names
pre_post_vars <- grep("_Pre$|_Post$", names(d_wide), value = TRUE)

d_long <- d_wide |>
  select(id, all_of(pre_post_vars)) |>
  tidyr::pivot_longer(
    cols = -id,
    names_to = c("measure", "time"),
    names_pattern = "^(.*)_(Pre|Post)$",
    values_to = "value"
  )

desc_stats <- d_long |>
  group_by(measure, time) |>
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )


print(desc_stats)
write_csv(desc_stats, "desc_stats_extraparticipant.csv")

## ----Data analysis with extra participant, message = FALSE--------------------


all_vars <- gsub("_Pre|_Post", "", grep("_Pre|_Post", names(d_wide), value = TRUE))
unique_vars <- unique(all_vars)

ttest_results <- lapply(unique_vars, function(var) {
  t.test(d_wide[[paste0(var, "_Post")]], d_wide[[paste0(var, "_Pre")]], paired = TRUE)
})

names(ttest_results) <- unique_vars
ttest_results

cohensd_results <- lapply(unique_vars, function(var) {
  effectsize::cohens_d(d_wide[[paste0(var, "_Post")]], d_wide[[paste0(var, "_Pre")]], pooled_sd = F, paired = TRUE)
})

names(cohensd_results) <- unique_vars
cohensd_results


## ----PTSD dx descriptive stats and analysis with extra participant------------
# PTSD diagnosis descriptive statistics
ptsd_desc <- full_d %>%
  filter(!is.na(PTSD_dx)) %>%
  group_by(time.point) %>%
  summarise(
    n = n(),
    n_ptsd = sum(PTSD_dx),
    percent_ptsd = round(100 * n_ptsd / n, 1)
  )

print("PTSD diagnosis counts and percentages:")
print(ptsd_desc)

# CPTSD diagnosis descriptive statistics
cptsd_desc <- full_d %>%
  filter(!is.na(CPTSD_dx)) %>%
  group_by(time.point) %>%
  summarise(
    n = n(),
    n_cptsd = sum(CPTSD_dx),
    percent_cptsd = round(100 * n_cptsd / n, 1)
  )

print("CPTSD diagnosis counts and percentages:")
print(cptsd_desc)


# PTSD dx analysis

# PTSD diagnosis at pre and post

table(full_d$time.point, full_d$PTSD_dx)
ptsd_counts <- aggregate(PTSD_dx ~ time.point, data = full_d, sum)


# Logistic regression

PTSD_glm_model <- glm(PTSD_dx ~ time.point, data = full_d, family = binomial)
summary(PTSD_glm_model)

# Odds ratio
exp(coef(PTSD_glm_model))

# 95% confidence intervals
exp(confint(PTSD_glm_model)) 

# CPTSD dx analysis

# CPTSD diagnosis at pre and post

table(full_d$time.point, full_d$CPTSD_dx)
cptsd_counts <- aggregate(CPTSD_dx ~ time.point, data = full_d, sum)

# Logistic regression
CPTSD_glm_model <- glm(CPTSD_dx ~ time.point, data = full_d, family = binomial)
summary(CPTSD_glm_model)

# odds ratio
exp(coef(CPTSD_glm_model)) 

# 95% CI
exp(confint(CPTSD_glm_model)) 

