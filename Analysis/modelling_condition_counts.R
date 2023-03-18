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
  labs(x = "Age group", y = "Condition count", title = "Long-term condition counts by age group", caption = "Understanding Society Wave 10\nN = 34316\nError bar = 95% conf. interval")

ggsave("Plots/Condition counts by age group.png", units = "in", width = 6, height = 5, dpi = 1000)


# Run quasi-poisson regression with linear age term only
m1<- svyglm(condition_count ~ age, svy_df, family = quasipoisson())

# Run quasi-poisson regression with quadratic age term
m2<- svyglm(condition_count ~ age + I(age^2), svy_df, family = quasipoisson())

# Calculate predicted counts across age for models 1 and 2
preds_m1<- predictions(m1, newdata = datagrid(model = m6, age = 16:90))
preds_m2<- predictions(m2, newdata = datagrid(model = m7, age = 16:90))

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
  labs(x = "Age", y = "Condition count", title = "Modelled and observed condition counts by age", caption = "Understanding Society Wave 10\nN = 34316\nError bar = 95% conf. interval")

ggsave("Plots/Modelled and observed counts by age.png", units = "in", width = 6, height = 5, dpi = 1000)
