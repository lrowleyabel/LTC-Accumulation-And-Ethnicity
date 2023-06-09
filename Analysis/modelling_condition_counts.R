################################
#                              #
#  MODELLING CONDITION COUNTS  #
#                              #
################################


# Laurence Rowley-Abel, University of Edinburgh
# Updated 18/03/2023

# Description: This file uses Wave 10 of the Understanding Society dataset to model the number of long-term conditions different ethnic groups
# accumulate over the life course in the UK.

library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(survey)
library(marginaleffects)

# Clear environment
rm(list = ls())

# Set path to repository root directory
root_dir<- "C:/Users/lrowley/OneDrive - University of Edinburgh/AIM-CISC/LTC Accumulation and Ethnicity"

# Set working directory
setwd(paste0(root_dir, "/Analysis"))

# Load in the pre-processed Wave 10 data (code for creating this file from the raw data are in https://github.com/lrowleyabel/Neighbourhood-And-Multimorbidity)
load("../Data/Understanding_Society_Wave_10_With_MM_Variables.Rda")


#### Step 1: Set up the data ####

# Create numeric age variable
df_w10<- mutate(df_w10, age = j_age_dv%>%
                   as.character()%>%
                   as.integer())

# Extend existing 5 year age groups to include older groups
df_w10<- mutate(df_w10, age_group = case_when(age < 70 ~ as.character(j_agegr5_dv),
                                              age >= 70 & age < 75 ~ "70-74",
                                              age >= 75 & age < 80 ~ "75-79",
                                              age >= 80 & age < 85 ~ "80-84",
                                              age >= 85 ~ "85+",
                                              T ~ NA_character_))


# Recode ethnicity variable so that small groups are grouped under "any other ethnic group" and mixed are all grouped together
df_w10<- mutate(df_w10, ethnicity = case_when(j_ethn_dv == "chinese" ~ "any other ethnic group",
                                              j_ethn_dv == "any other black background" ~ "any other ethnic group",
                                              j_ethn_dv == "any other asian background" ~ "any other ethnic group",
                                              j_ethn_dv == "arab" ~ "any other ethnic group",
                                              j_ethn_dv == "gypsy or irish traveller" ~ "any other ethnic group",
                                              j_ethn_dv == "white and black caribbean" ~ "mixed",
                                              j_ethn_dv == "white and black african" ~ "mixed",
                                              j_ethn_dv == "white and asian" ~ "mixed",
                                              j_ethn_dv == "any other mixed background" ~ "mixed",
                                              T ~ as.character(j_ethn_dv)))



# Create broad ethnic group variable
table(df_w10$j_ethn_dv)

df_w10<- mutate(df_w10, broad_ethnic_group = case_when(j_ethn_dv %in% c("british/english/scottish/welsh/northern irish", "irish", "any other white background") ~ "White",
                                                         j_ethn_dv %in% c("indian", "pakistani", "bangladeshi") ~ "South Asian",
                                                         j_ethn_dv %in% c("caribbean", "african", "any other black background") ~ "Black",
                                                         ethnicity == "mixed" ~ "Other",
                                                         ethnicity == "any other ethnic group" ~ "Other",
                                                         j_ethn_dv %in% c("missing", "inapplicable", "refusal", "don't know") ~ "Missing"))

df_w10<- mutate(df_w10, broad_ethnic_group = factor(broad_ethnic_group, levels = c("White", "Black", "South Asian", "Other")))

# Set up survey design object
options(survey.lonely.psu = "remove")

svy_df<- svydesign(ids = ~j_psu, strata = ~j_strata, weights = ~j_indinui_xw, data = df_w10)

# Remove observations with missing values
svy_df<- subset(svy_df, !is.na(age) & !is.na(broad_ethnic_group))

# Get number of valid observations
svy_df$variables%>%
  filter(!is.na(age) & !is.na(broad_ethnic_group))%>%
  nrow()

#### Model counts by age ####


# Calculate observed mean condition counts by age group
age_counts<- svyby(~condition_count, ~age_group, FUN = svymean, design = svy_df, na.rm = T, vartype = "ci")

# Plot observed mean condition counts by age group
age_counts%>%
  mutate(age_group = str_remove(age_group, " years old"))%>%
  ggplot()+
  geom_point(aes(x = age_group, y = condition_count))+
  geom_errorbar(aes(x = age_group, ymin = ci_l, ymax = ci_u), width = 0.1)+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,20,0)),
        plot.margin = margin(20,20,20,20))+
  labs(x = "Age group", y = "Condition count", title = "Long-term condition counts by age group", caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval")

ggsave("Plots/Condition counts by age group.png", units = "in", width = 6, height = 5, dpi = 1000)


# Run quasi-poisson regression with linear age term only
m1<- svyglm(condition_count ~ age, svy_df, family = quasipoisson())

# Run quasi-poisson regression with quadratic age term
m2<- svyglm(condition_count ~ age + I(age^2), svy_df, family = quasipoisson())

# Calculate predicted counts across age for models 1 and 2
preds_m1<- predictions(m1, newdata = datagrid(model = m1, age = 16:90))
preds_m2<- predictions(m2, newdata = datagrid(model = m2, age = 16:90))

# For observed mean counts, create mid-point age for each age group so we can plot on the same axis as the model predictions
age_counts<- age_counts%>%
  mutate(age_group_min = str_extract(age_group, "[0-9][0-9](?=-)|[0-9][0-9](?=\\+)")%>%
                          as.numeric(),
         age_group_max = str_extract(age_group, "(?<=-)[0-9][0-9]|[0-9][0-9](?=\\+)")%>%
                          as.numeric())%>%
  mutate(age = age_group_min + ((age_group_max - age_group_min)/2))

# Plot the modelled condition counts by age for the linear and quadratic model and compare to the observed counts
ggplot()+
  geom_line(data = preds_m1, aes(x = age, y = estimate, color = "Linear model"))+
  geom_line(data = preds_m2, aes(x = age, y = estimate, color = "Quadratic model"))+
  geom_point(data = age_counts, aes(x = age, y = condition_count, shape = "Observed"))+
  geom_errorbar(data = age_counts, aes(x = age, ymin = ci_l, ymax = ci_u), width = 0.5)+
  ggthemes::scale_color_economist(name = "")+
  scale_shape(name = "")+
  scale_x_continuous(breaks = seq(20,90,10), limits = c(15,90))+
  theme_economist()+
  theme(axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(-20,0,-20,0),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,20,0)),
        plot.margin = margin(20,20,20,20))+
  labs(x = "Age", y = "Condition count", title = "Modelled and observed condition counts by age", caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval")

ggsave("Plots/Modelled and observed counts by age.png", units = "in", width = 6, height = 5, dpi = 1000)


#### Model counts by age and ethnicity ####


# Create broad age groups so that number of ethnic minority observations within older age groups are not too low
svy_df<- update(svy_df, broad_age_group = case_when(age < 20 ~ "16-19",
                                                    age >= 20 & age < 30 ~ "20-29",
                                                    age >= 30 & age < 40 ~ "30-39",
                                                    age >= 40 & age < 50 ~ "40-49",
                                                    age >= 50 & age < 60 ~ "50-59",
                                                    age >= 60 ~ "60+"))


# Calculate observed mean condition counts within each broad age group for each ethnicity
asian_age_counts<- svyby(~condition_count, ~broad_age_group, FUN = svymean, design = subset(svy_df, broad_ethnic_group == "South Asian"), vartype = "ci", na.rm = T)
black_age_counts<- svyby(~condition_count, ~broad_age_group, FUN = svymean, design = subset(svy_df, broad_ethnic_group == "Black"), vartype = "ci", na.rm = T)
white_age_counts<- svyby(~condition_count, ~broad_age_group, FUN = svymean, design = subset(svy_df, broad_ethnic_group == "White"), vartype = "ci", na.rm = T)

# Join the calculated counts for each ethnicity and plot
ethnicity_age_counts<- rbind(mutate(asian_age_counts, broad_ethnic_group = "South Asian"),
                             mutate(black_age_counts, broad_ethnic_group = "Black"),
                             mutate(white_age_counts, broad_ethnic_group = "White"))

ethnicity_age_counts%>%
  ggplot()+
  geom_point(aes(x = broad_age_group, y = condition_count, colour = broad_ethnic_group, group = broad_ethnic_group), position = position_dodge(width = 0.4))+
  geom_errorbar(aes(x = broad_age_group, ymin = ci_l, ymax = ci_u, colour = broad_ethnic_group, group = broad_ethnic_group), position = position_dodge(width = 0.4), width = 0.1)+
  scale_color_economist(name = "")+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(-20,0,-20,0),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,20,0)),
        plot.margin = margin(20,20,20,20))+
  labs(x = "Age group", y = "Condition count", title = "Observed condition counts by age group and\nethnicity", caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval")

ggsave("Plots/Condition counts by age group and ethnicity.png", units = "in", width = 6, height = 5, dpi = 1000)

# Run quasi-poisson regression of counts with age, age-squared and ethnicity
m3<- svyglm(condition_count ~ age + I(age^2) + broad_ethnic_group, svy_df, family = quasipoisson())
summary(m3)


# For observed mean counts, create mid-point age for each age group so we can plot on the same axis as the model predictions
ethnicity_age_counts<- ethnicity_age_counts%>%
  mutate(age_group_min = str_extract(broad_age_group, "[0-9][0-9](?=-)|[0-9][0-9](?=\\+)")%>%
           as.numeric(),
         age_group_max = str_extract(broad_age_group, "(?<=-)[0-9][0-9]|[0-9][0-9](?=\\+)")%>%
           as.numeric())%>%
  mutate(age = age_group_min + ((age_group_max - age_group_min)/2))

# Calculate mean age of over 60s within each ethnic group so that we can assign a better mid-point age
svyby(~age, ~broad_ethnic_group, design = subset(svy_df, age >= 60), FUN = svymean, na.rm = T)

ethnicity_age_counts<- ethnicity_age_counts%>%
  mutate(age = case_when(broad_age_group == "60+" ~ 70,
                         T ~ age))

# Plot the modelled and observed counts
preds_m3<- predictions(model = m3, newdata = datagrid(model = m3, age = 16:80, broad_ethnic_group = c("White", "Black", "South Asian")))

ggplot()+
  geom_line(data = preds_m3, aes(x = age, y = estimate, color = broad_ethnic_group))+
  geom_point(data = ethnicity_age_counts, aes(x = age, y = condition_count, shape = "Observed", color = ethnicity), position = position_dodge(width = 0.5))+
  geom_errorbar(data = ethnicity_age_counts, aes(x = age, ymin = ci_l, ymax = ci_u, group = ethnicity, color = ethnicity), width = 0.5, position = position_dodge(width = 0.5))+
  ggthemes::scale_color_economist(name = "")+
  scale_shape(name = "")+
  scale_x_continuous(breaks = seq(20,90,10), limits = c(15,90))+
  theme_economist()+
  theme(axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(-20,0,-20,0),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,20,0)),
        plot.margin = margin(20,20,20,20))+
  labs(x = "Age", y = "Condition count", title = "Modelled and observed condition counts by age", caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval")


# Run quasi-poisson regression of counts with age, age-squared, ethnicity and ethnicity-age interaction
m4<- svyglm(condition_count ~ age + I(age^2) + broad_ethnic_group + (age*broad_ethnic_group), svy_df, family = quasipoisson())
summary(m4)

# Plot the modelled and observed counts
preds_m4<- predictions(model = m4, newdata = datagrid(model = m4, age = 16:80, broad_ethnic_group = c("White", "Black", "South Asian")))

ggplot()+
  geom_line(data = preds_m4, aes(x = age, y = estimate, color = broad_ethnic_group))+
  geom_point(data = ethnicity_age_counts, aes(x = age, y = condition_count, shape = "Observed", color = broad_ethnic_group), position = position_dodge(width = 0.5))+
  geom_errorbar(data = ethnicity_age_counts, aes(x = age, ymin = ci_l, ymax = ci_u, group = broad_ethnic_group, color = broad_ethnic_group), width = 0.5, position = position_dodge(width = 0.5))+
  geom_text(data = ethnicity_age_counts, aes(x = age, y = ci_u + 0.05, label = broad_age_group), size = 3, angle = 90, hjust = 0)+
  scale_color_economist(name = "", labels = c("Modelled White", "Modelled Black", "Modelled South Asian"))+
  scale_shape(name = "")+
  scale_x_continuous(breaks = seq(20,90,10), limits = c(15,90))+
  facet_wrap(~broad_ethnic_group)+
  theme_economist()+
  theme(axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0,0,0,0),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,10,0)),
        plot.margin = margin(20,20,20,20),
        strip.text = element_blank())+
  labs(x = "Age", y = "Condition count", title = "Modelled and observed condition counts by age and ethnicity", subtitle ="Model with ethnicity-age iteraction term", caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval\nLabels on plots are age groups for observed counts")+
  guides(colour = guide_legend(override.aes = list(shape = c(NA,NA,NA))))

ggsave("Plots/Modelled and observed counts by age and ethnicity with age-ethnicity interaction.png", units = "in", width = 9, height = 5, dpi = 1000)



# Run quasi-poisson regression of counts with age, age-squared, ethnicity and ethnicity-age interaction and ethnicity-agesquared interaction
m5<- svyglm(condition_count ~ age + I(age^2) + broad_ethnic_group + (age*broad_ethnic_group) + (I(age^2)*broad_ethnic_group), svy_df, family = quasipoisson())
summary(m5)

# Plot the modelled and observed counts
preds_m5<- predictions(model = m5, newdata = datagrid(model = m5, age = 16:80, broad_ethnic_group = c("White", "Black", "South Asian")))

ggplot()+
  geom_line(data = preds_m5, aes(x = age, y = estimate, color = broad_ethnic_group))+
  geom_point(data = ethnicity_age_counts, aes(x = age, y = condition_count, shape = "Observed", color = broad_ethnic_group), position = position_dodge(width = 0.5))+
  geom_errorbar(data = ethnicity_age_counts, aes(x = age, ymin = ci_l, ymax = ci_u, group = broad_ethnic_group, color = broad_ethnic_group), width = 0.5, position = position_dodge(width = 0.5))+
  geom_text(data = ethnicity_age_counts, aes(x = age, y = ci_u + 0.05, label = broad_age_group), size = 3, angle = 90, hjust = 0)+
  scale_color_economist(name = "", labels = c("Modelled White", "Modelled Black", "Modelled South Asian"))+
  scale_shape(name = "")+
  scale_x_continuous(breaks = seq(20,90,10), limits = c(15,90))+
  scale_y_continuous(limits = c(0,1.5))+
  facet_wrap(~broad_ethnic_group)+
  theme_economist()+
  theme(axis.title.x = element_text(margin = margin(10,0,10,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0,0,0,0),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(10,0,10,0)),
        plot.margin = margin(20,20,20,20),
        strip.text = element_blank())+
  labs(x = "Age", y = "Condition count", title = "Modelled and observed condition counts by age and ethnicity", subtitle = bquote("Model with ethnicity-age and ethnicity-"*age^2*" interaction terms"), caption = "Understanding Society Wave 10\nN = 34220\nError bar = 95% conf. interval\nLabels on plots are age groups for observed counts")+
  guides(colour = guide_legend(override.aes = list(shape = c(NA,NA,NA))))

ggsave("Plots/Modelled and observed counts by age and ethnicity with age-ethnicity and agesqrd-ethnicity interaction.png", units = "in", width = 9, height = 5, dpi = 1000)