# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lme4)  # For fitting linear mixed-effects models
library(ggplot2)  # For creating plots
library(lmerTest)  # For adding p-values to lmer models
library(glmmTMB)  # For fitting generalized linear mixed models using Template Model Builder
library(ggeffects)  # For creating predicted values from models
library(sjPlot)  # For visualizing results from statistical models

# Load data for fecundity and population density
fecundity <- read.csv("data/data/fecundity.csv")  # Read the fecundity data
Density <- read.csv("data/data/pop_est.csv")  # Read the population density data

# Select relevant columns from the density data
PopD <- Density %>% select(DeerYear, Hinds, Adults, Total, LU_Total)  # Select specific columns

# Join fecundity data with population density data based on the DeerYear
fecundity <- fecundity %>%
  left_join(PopD, by = "DeerYear")

# Create a new column for the square of the hind's age
fecundity$AgeSquared <- (fecundity$Age) * (fecundity$Age)

# Filter out data where hind's age is 2 or less
fecundity <- fecundity %>% filter(Age > 2)

# Fit a generalized linear mixed model for fecundity based on adult density
fecundity_mod <- glmmTMB(Fecundity ~ Adults
                         + ReprodStatus
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = fecundity)
# Summarize the model
summary(fecundity_mod)

# Filter data for hinds that were seen in rut
rut <- fecundity %>% filter(SeenInRut == -1)
rut <- na.omit(rut)  # Remove rows with missing data
unique_rut <- rut %>% distinct(Female, .keep_all = TRUE)  # Get unique rows by Female ID

# Further filter data based on reproductive status
milk <- rut %>% filter(ReprodStatus == "Milk")
summer_yeld <- rut %>% filter(ReprodStatus == "Summer yeld")
winter_yeld <- rut %>% filter(ReprodStatus == "Winter yeld")
true_yeld <- rut %>% filter(ReprodStatus == "True yeld")
naive <- rut %>% filter(!ReprodStatus %in% c("Milk", "Winter yeld", "Summer yeld", "True yeld"))

# Fit models for different reproductive statuses
milk_mod <- glmmTMB(Fecundity ~ Hinds
                    + (1 | DeerYear)
                    + (1 | Female),
                    family = binomial(link = "logit"), data = milk)
summary(milk_mod)

yeld <- rut %>% filter(ReprodStatus %in% c("Winter yeld", "Summer yeld", "True yeld"))

yeld_mod <- glmmTMB(Fecundity ~ Hinds
                    + ReprodStatus
                    + (1 | DeerYear)
                    + (1 | Female),
                    family = binomial(link = "logit"), data = yeld)
summary(yeld_mod)

# Models with and without age, and with year as a random effect for different reproductive statuses

# Milk models
milk_Hinds <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + DeerYear
                      + (1 | DeerYear)
                      + (1 | Female),
                      family = binomial(link = "logit"), data = milk)
summary(milk_Hinds)

milk_Adults <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + DeerYear
                       + (1 | DeerYear)
                       + (1 | Female),
                       family = binomial(link = "logit"), data = milk)
summary(milk_Adults)

milk_Total <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + DeerYear
                      + (1 | DeerYear)
                      + (1 | Female),
                      family = binomial(link = "logit"), data = milk)
summary(milk_Total)

milk_LU_Total <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + DeerYear
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = milk)
summary(milk_LU_Total)

# Models without age
milk_Hinds_noage <- glmmTMB(Fecundity ~ Hinds + DeerYear
                            + (1 | DeerYear)
                            + (1 | Female),
                            family = binomial(link = "logit"), data = milk)
summary(milk_Hinds_noage)

milk_Adults_noage <- glmmTMB(Fecundity ~ Adults + DeerYear
                             + (1 | DeerYear)
                             + (1 | Female),
                             family = binomial(link = "logit"), data = milk)
summary(milk_Adults_noage)

milk_Total_noage <- glmmTMB(Fecundity ~ Total + DeerYear
                            + (1 | DeerYear)
                            + (1 | Female),
                            family = binomial(link = "logit"), data = milk)
summary(milk_Total_noage)

milk_LU_Total_noage <- glmmTMB(Fecundity ~ LU_Total + DeerYear
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = milk)
summary(milk_LU_Total_noage)

# Models without age and year
milk_Hinds_noage_yr <- glmmTMB(Fecundity ~ Hinds
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = milk)
summary(milk_Hinds_noage_yr)

milk_Adults_noage_yr <- glmmTMB(Fecundity ~ Adults
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = milk)
summary(milk_Adults_noage_yr)

milk_Total_noage_yr <- glmmTMB(Fecundity ~ Total
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = milk)
summary(milk_Total_noage_yr)

milk_LU_Total_noage_yr <- glmmTMB(Fecundity ~ LU_Total
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = milk)
summary(milk_LU_Total_noage_yr)

# Models for Summer yeld reproductive status
summer_Hinds <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + DeerYear
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Hinds)

summer_Adults <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + DeerYear
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Adults)

summer_Total <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + DeerYear
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Total)

summer_LU_Total <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + DeerYear
                           + (1 | DeerYear)
                           + (1 | Female),
                           family = binomial(link = "logit"), data = summer_yeld)
summary(summer_LU_Total)

# Models without age
summer_Hinds_noage <- glmmTMB(Fecundity ~ Hinds + DeerYear
                              + (1 | DeerYear)
                              + (1 | Female),
                              family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Hinds_noage)

summer_Adults_noage <- glmmTMB(Fecundity ~ Adults + DeerYear
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Adults_noage)

summer_Total_noage <- glmmTMB(Fecundity ~ Total + DeerYear
                              + (1 | DeerYear)
                              + (1 | Female),
                              family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Total_noage)

summer_LU_Total_noage <- glmmTMB(Fecundity ~ LU_Total + DeerYear
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = summer_yeld)
summary(summer_LU_Total_noage)

# Models without age and year
summer_Hinds_noage_yr <- glmmTMB(Fecundity ~ Hinds
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Hinds_noage_yr)

summer_Adults_noage_yr <- glmmTMB(Fecundity ~ Adults
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Adults_noage_yr)

summer_Total_noage_yr <- glmmTMB(Fecundity ~ Total
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = summer_yeld)
summary(summer_Total_noage_yr)

summer_LU_Total_noage_yr <- glmmTMB(Fecundity ~ LU_Total
                                    + (1 | DeerYear)
                                    + (1 | Female),
                                    family = binomial(link = "logit"), data = summer_yeld)
summary(summer_LU_Total_noage_yr)

# Winter yeld models
winter_Hinds <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + DeerYear
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Hinds)

winter_Adults <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + DeerYear
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Adults)

winter_Total <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + DeerYear
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Total)

winter_LU_Total <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + DeerYear
                           + (1 | DeerYear)
                           + (1 | Female),
                           family = binomial(link = "logit"), data = winter_yeld)
summary(winter_LU_Total)

# Models without age
winter_Hinds_noage <- glmmTMB(Fecundity ~ Hinds + DeerYear
                              + (1 | DeerYear)
                              + (1 | Female),
                              family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Hinds_noage)

winter_Adults_noage <- glmmTMB(Fecundity ~ Adults + DeerYear
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Adults_noage)

winter_Total_noage <- glmmTMB(Fecundity ~ Total + DeerYear
                              + (1 | DeerYear)
                              + (1 | Female),
                              family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Total_noage)

winter_LU_Total_noage <- glmmTMB(Fecundity ~ LU_Total + DeerYear
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = winter_yeld)
summary(winter_LU_Total_noage)

# Models without age and year
winter_Hinds_noage_yr <- glmmTMB(Fecundity ~ Hinds
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Hinds_noage_yr)

winter_Adults_noage_yr <- glmmTMB(Fecundity ~ Adults
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Adults_noage_yr)

winter_Total_noage_yr <- glmmTMB(Fecundity ~ Total
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = winter_yeld)
summary(winter_Total_noage_yr)

winter_LU_Total_noage_yr <- glmmTMB(Fecundity ~ LU_Total
                                    + (1 | DeerYear)
                                    + (1 | Female),
                                    family = binomial(link = "logit"), data = winter_yeld)
summary(winter_LU_Total_noage_yr)

# True yeld models
true_Hinds <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + DeerYear
                      + (1 | DeerYear)
                      + (1 | Female),
                      family = binomial(link = "logit"), data = true_yeld)
summary(true_Hinds)

true_Adults <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + DeerYear
                       + (1 | DeerYear)
                       + (1 | Female),
                       family = binomial(link = "logit"), data = true_yeld)
summary(true_Adults)

true_Total <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + DeerYear
                      + (1 | DeerYear)
                      + (1 | Female),
                      family = binomial(link = "logit"), data = true_yeld)
summary(true_Total)

true_LU_Total <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + DeerYear
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = true_yeld)
summary(true_LU_Total)

# Models without age
true_Hinds_noage <- glmmTMB(Fecundity ~ Hinds + DeerYear
                            + (1 | DeerYear)
                            + (1 | Female),
                            family = binomial(link = "logit"), data = true_yeld)
summary(true_Hinds_noage)

true_Adults_noage <- glmmTMB(Fecundity ~ Adults + DeerYear
                             + (1 | DeerYear)
                             + (1 | Female),
                             family = binomial(link = "logit"), data = true_yeld)
summary(true_Adults_noage)

true_Total_noage <- glmmTMB(Fecundity ~ Total + DeerYear
                            + (1 | DeerYear)
                            + (1 | Female),
                            family = binomial(link = "logit"), data = true_yeld)
summary(true_Total_noage)

true_LU_Total_noage <- glmmTMB(Fecundity ~ LU_Total + DeerYear
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = true_yeld)
summary(true_LU_Total_noage)

# Models without age and year
true_Hinds_noage_yr <- glmmTMB(Fecundity ~ Hinds
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = true_yeld)
summary(true_Hinds_noage_yr)

true_Adults_noage_yr <- glmmTMB(Fecundity ~ Adults
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = true_yeld)
summary(true_Adults_noage_yr)

true_Total_noage_yr <- glmmTMB(Fecundity ~ Total
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = true_yeld)
summary(true_Total_noage_yr)

true_LU_Total_noage_yr <- glmmTMB(Fecundity ~ LU_Total
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = true_yeld)
summary(true_LU_Total_noage_yr)

# Naive models
naive_Hinds <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + DeerYear
                       + (1 | DeerYear)
                       + (1 | Female),
                       family = binomial(link = "logit"), data = naive)
summary(naive_Hinds)

naive_Adults <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + DeerYear
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = naive)
summary(naive_Adults)

naive_Total <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + DeerYear
                       + (1 | DeerYear)
                       + (1 | Female),
                       family = binomial(link = "logit"), data = naive)
summary(naive_Total)

naive_LU_Total <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + DeerYear
                          + (1 | DeerYear)
                          + (1 | Female),
                          family = binomial(link = "logit"), data = naive)
summary(naive_LU_Total)

# Models without age
naive_Hinds_noage <- glmmTMB(Fecundity ~ Hinds + DeerYear
                             + (1 | DeerYear)
                             + (1 | Female),
                             family = binomial(link = "logit"), data = naive)
summary(naive_Hinds_noage)

naive_Adults_noage <- glmmTMB(Fecundity ~ Adults + DeerYear
                              + (1 | DeerYear)
                              + (1 | Female),
                              family = binomial(link = "logit"), data = naive)
summary(naive_Adults_noage)

naive_Total_noage <- glmmTMB(Fecundity ~ Total + DeerYear
                             + (1 | DeerYear)
                             + (1 | Female),
                             family = binomial(link = "logit"), data = naive)
summary(naive_Total_noage)

naive_LU_Total_noage <- glmmTMB(Fecundity ~ LU_Total + DeerYear
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = naive)
summary(naive_LU_Total_noage)

# Models without age and year
naive_Hinds_noage_yr <- glmmTMB(Fecundity ~ Hinds
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = naive)
summary(naive_Hinds_noage_yr)

naive_Adults_noage_yr <- glmmTMB(Fecundity ~ Adults
                                 + (1 | DeerYear)
                                 + (1 | Female),
                                 family = binomial(link = "logit"), data = naive)
summary(naive_Adults_noage_yr)

naive_Total_noage_yr <- glmmTMB(Fecundity ~ Total
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = naive)
summary(naive_Total_noage_yr)

naive_LU_Total_noage_yr <- glmmTMB(Fecundity ~ LU_Total
                                   + (1 | DeerYear)
                                   + (1 | Female),
                                   family = binomial(link = "logit"), data = naive)
summary(naive_LU_Total_noage_yr)

# Models with age
milk_mod_age <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = milk)
summary(milk_mod_age)

yeld_mod_age <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared
                        + ReprodStatus
                        + (1 | DeerYear)
                        + (1 | Female),
                        family = binomial(link = "logit"), data = yeld)
summary(yeld_mod_age)

naive_mod_age <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared
                         + (1 | DeerYear)
                         + (1 | Female),
                         family = binomial(link = "logit"), data = naive)
summary(naive_mod_age)


# Models with age but without year
Hinds_fecundity_age <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + ReprodStatus
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = rut)
summary(Hinds_fecundity_age)

Adults_fecundity_age <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + ReprodStatus
                                + (1 | DeerYear)
                                + (1 | Female),
                                family = binomial(link = "logit"), data = rut)
summary(Adults_fecundity_age)

Total_fecundity_age <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + ReprodStatus
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = rut)
summary(Total_fecundity_age)

LU_Total_fecundity_age <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + ReprodStatus
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = rut)
summary(LU_Total_fecundity_age)

# Models with interaction, age but without year
int_Hinds_fecundity_age <- glmmTMB(Fecundity ~ Hinds * ReprodStatus + Age + AgeSquared
                                   + (1 | DeerYear)
                                   + (1 | Female),
                                   family = binomial(link = "logit"), data = rut)
summary(int_Hinds_fecundity_age)

int_Adults_fecundity_age <- glmmTMB(Fecundity ~ Adults * ReprodStatus + Age + AgeSquared
                                    + (1 | DeerYear)
                                    + (1 | Female),
                                    family = binomial(link = "logit"), data = rut)
summary(int_Adults_fecundity_age)

int_Total_fecundity_age <- glmmTMB(Fecundity ~ Total * ReprodStatus + Age + AgeSquared
                                   + (1 | DeerYear)
                                   + (1 | Female),
                                   family = binomial(link = "logit"), data = rut)
summary(int_Total_fecundity_age)

int_LU_Total_fecundity_age <- glmmTMB(Fecundity ~ LU_Total * ReprodStatus + Age + AgeSquared
                                      + (1 | DeerYear)
                                      + (1 | Female),
                                      family = binomial(link = "logit"), data = rut)
summary(int_LU_Total_fecundity_age)

# Models with age and additional year
yr_Hinds_fecundity_age <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + ReprodStatus + DeerYear
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = rut)
summary(yr_Hinds_fecundity_age)

yr_Adults_fecundity_age <- glmmTMB(Fecundity ~ Adults + Age + AgeSquared + ReprodStatus + DeerYear
                                   + (1 | DeerYear)
                                   + (1 | Female),
                                   family = binomial(link = "logit"), data = rut)
summary(yr_Adults_fecundity_age)

yr_Total_fecundity_age <- glmmTMB(Fecundity ~ Total + Age + AgeSquared + ReprodStatus + DeerYear
                                  + (1 | DeerYear)
                                  + (1 | Female),
                                  family = binomial(link = "logit"), data = rut)
summary(yr_Total_fecundity_age)

yr_LU_Total_fecundity_age <- glmmTMB(Fecundity ~ LU_Total + Age + AgeSquared + ReprodStatus + DeerYear
                                     + (1 | DeerYear)
                                     + (1 | Female),
                                     family = binomial(link = "logit"), data = rut)
summary(yr_LU_Total_fecundity_age)

# Null Models without density
noyr_noden_fecundity_age <- glmmTMB(Fecundity ~ Age + AgeSquared + ReprodStatus
                                    + (1 | DeerYear)
                                    + (1 | Female),
                                    family = binomial(link = "logit"), data = rut)

summary(noyr_noden_fecundity_age)

noden_fecundity_age <- glmmTMB(Fecundity ~ Age + AgeSquared + ReprodStatus + DeerYear
                               + (1 | DeerYear)
                               + (1 | Female),
                               family = binomial(link = "logit"), data = rut)
summary(noden_fecundity_age)

# Models with interaction, age, and additional year
int_yr_Hinds_fecundity_age <- glmmTMB(Fecundity ~ Hinds * ReprodStatus + Age + AgeSquared + DeerYear
                                      + (1 | DeerYear)
                                      + (1 | Female),
                                      family = binomial(link = "logit"), data = rut)
summary(int_yr_Hinds_fecundity_age)

int_yr_Adults_fecundity_age <- glmmTMB(Fecundity ~ Adults * ReprodStatus + Age + AgeSquared + DeerYear
                                       + (1 | DeerYear)
                                       + (1 | Female),
                                       family = binomial(link = "logit"), data = rut)
summary(int_yr_Adults_fecundity_age)

int_yr_Total_fecundity_age <- glmmTMB(Fecundity ~ Total * ReprodStatus + Age + AgeSquared + DeerYear
                                      + (1 | DeerYear)
                                      + (1 | Female),
                                      family = binomial(link = "logit"), data = rut)
summary(int_yr_Total_fecundity_age)

int_yr_LU_Total_fecundity_age <- glmmTMB(Fecundity ~ LU_Total * ReprodStatus + Age + AgeSquared + DeerYear
                                         + (1 | DeerYear)
                                         + (1 | Female),
                                         family = binomial(link = "logit"), data = rut)
summary(int_yr_LU_Total_fecundity_age)

# Model summaries in tabular form
tab_model(Hinds_fecundity_age, transform = NULL, digits = 4, show.se = TRUE, show.stat = TRUE)

tab_model(int_yr_Adults_fecundity_age, transform = NULL, digits = 4, show.stat = TRUE, show.se = TRUE)

# Generate data for proportion plots
portion <- rut %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    portion = reproduced / count,
  )

# Filter out specific years
portion <- portion %>% filter(!DeerYear %in% c(1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969))

# Join population density data with portion data
portion <- portion %>%
  left_join(PopD, by = "DeerYear")

# Plot proportion of females seen in rut that reproduced over time
ggplot(portion, aes(x = DeerYear, y = portion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Proportion of females seen in rut reproduced over Year") +
  ylab("Proportion of females reproduced") + xlab("Year") +
  theme_minimal() +
  geom_text(aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot proportion of females seen in rut that reproduced over hind density
ggplot(portion, aes(x = Hinds, y = portion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Proportion of females seen in rut reproduced over Density") +
  ylab("Proportion of females reproduced") + xlab("Hind Density") +
  theme_minimal() +
  geom_text(aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot proportion of females seen in rut that reproduced over adult density
ggplot(portion, aes(x = Adults, y = portion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Proportion of females seen in rut reproduced over Density") +
  ylab("Proportion of females reproduced") + xlab("Adult Density") +
  theme_minimal() +
  geom_text(aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot proportion of females seen in rut that reproduced over total density
ggplot(portion, aes(x = Total, y = portion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Proportion of females seen in rut reproduced over Density") +
  ylab("Proportion of females reproduced") + xlab("Total Density") +
  theme_minimal() +
  geom_text(aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot proportion of females seen in rut that reproduced over livestock units
ggplot(portion, aes(x = LU_Total, y = portion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Proportion of females seen in rut reproduced over Density") +
  ylab("Proportion of females reproduced") + xlab("Livestock units") +
  theme_minimal() +
  geom_text(aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Generate proportion data for different reproductive statuses
portion_milk <- milk %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    milk = reproduced / count,
  )
portion_milk <- portion_milk %>% select(DeerYear, milk)

portion_summer_yeld <- summer_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    summer_yeld = reproduced / count,
  )
portion_summer_yeld <- portion_summer_yeld %>% select(DeerYear, summer_yeld)

portion_winter_yeld <- winter_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    winter_yeld = reproduced / count,
  )
portion_winter_yeld <- portion_winter_yeld %>% select(DeerYear, winter_yeld)

portion_true_yeld <- true_yeld %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    true_yeld = reproduced / count,
  )
portion_true_yeld <- portion_true_yeld %>% select(DeerYear, true_yeld)

portion_naive <- naive %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    reproduced = sum(Fecundity, na.rm = TRUE),
    naive = reproduced / count,
  )
portion_naive <- portion_naive %>% select(DeerYear, naive)

# Join the different proportion data with the main portion data
portion <- portion %>%
  left_join(portion_milk, by = "DeerYear") %>%
  left_join(portion_summer_yeld, by = "DeerYear") %>%
  left_join(portion_winter_yeld, by = "DeerYear") %>%
  left_join(portion_true_yeld, by = "DeerYear") %>%
  left_join(portion_naive, by = "DeerYear")


portion<-portion%>% filter(!DeerYear %in% c(1970, 1971))

cor(portion$portion, PopD$Hinds)

# Plot proportion of females seen in rut that reproduced for different reproductive statuses over time
ggplot(portion, aes(x = DeerYear)) +
  geom_point(aes(y = milk, color = "milk")) +
  geom_point(aes(y = summer_yeld, color = "summer_yeld")) +
  geom_point(aes(y = winter_yeld, color = "winter_yeld")) +
  geom_point(aes(y = true_yeld, color = "true_yeld")) +
  geom_point(aes(y = naive, color = "naive")) +
  geom_smooth(method = "lm", aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm", aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = winter_yeld, color = "winter_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced per Year",
       x = "Year",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black",
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

# Plot proportion of females seen in rut that reproduced for different reproductive statuses over hind density
ggplot(portion, aes(x = Hinds)) +
  geom_point(aes(y = milk, color = "milk")) +
  geom_point(aes(y = summer_yeld, color = "summer_yeld")) +
  geom_point(aes(y = winter_yeld, color = "winter_yeld")) +
  geom_point(aes(y = true_yeld, color = "true_yeld")) +
  geom_point(aes(y = naive, color = "naive")) +
  geom_smooth(method = "lm", aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm", aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = winter_yeld, color = "winter_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Hind Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black",
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

# Plot proportion of females seen in rut that reproduced for different reproductive statuses over adult density
ggplot(portion, aes(x = Adults)) +
  geom_point(aes(y = milk, color = "milk")) +
  geom_point(aes(y = summer_yeld, color = "summer_yeld")) +
  geom_point(aes(y = winter_yeld, color = "winter_yeld")) +
  geom_point(aes(y = true_yeld, color = "true_yeld")) +
  geom_point(aes(y = naive, color = "naive")) +
  geom_smooth(method = "lm", aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm", aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = winter_yeld, color = "winter_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Adult Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black",
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

# Plot proportion of females seen in rut that reproduced for different reproductive statuses over total density
ggplot(portion, aes(x = Total)) +
  geom_point(aes(y = milk, color = "milk")) +
  geom_point(aes(y = summer_yeld, color = "summer_yeld")) +
  geom_point(aes(y = winter_yeld, color = "winter_yeld")) +
  geom_point(aes(y = true_yeld, color = "true_yeld")) +
  geom_point(aes(y = naive, color = "naive")) +
  geom_smooth(method = "lm", aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm", aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = winter_yeld, color = "winter_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Total Density",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black",
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

# Plot proportion of females seen in rut that reproduced for different reproductive statuses over livestock units
ggplot(portion, aes(x = LU_Total)) +
  geom_point(aes(y = milk, color = "milk")) +
  geom_point(aes(y = summer_yeld, color = "summer_yeld")) +
  geom_point(aes(y = winter_yeld, color = "winter_yeld")) +
  geom_point(aes(y = true_yeld, color = "true_yeld")) +
  geom_point(aes(y = naive, color = "naive")) +
  geom_smooth(method = "lm", aes(y = milk, color = "milk"), size = 2) +
  geom_smooth(method = "lm", aes(y = summer_yeld, color = "summer_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = winter_yeld, color = "winter_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = true_yeld, color = "true_yeld"), size = 2) +
  geom_smooth(method = "lm", aes(y = naive, color = "naive"), size = 2) +
  labs(title = "Proportion of females seen in rut reproduced over Density",
       x = "Livestock units",
       y = "Proportion of females reproduced") +
  scale_color_manual(values = c("milk" = "red", "summer_yeld" = "blue", "winter_yeld" = "black",
                                "true_yeld" = "green", "naive" = "orange"),
                     breaks = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"),
                     labels = c("milk", "summer_yeld", "winter_yeld", "true_yeld", "naive"))

# Plot average reproduction probability over time with error bars
Avgrut <- rut %>%
  group_by(DeerYear) %>%
  summarise(
    mean_fec = mean(Fecundity),
    sample_size = n(),
    se_fec = sd(Fecundity) / sqrt(n())
  )

ggplot(data = Avgrut, aes(x = DeerYear, y = mean_fec)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_fec - se_fec, ymax = mean_fec + se_fec), width = 0.2, color = "black") +
  scale_x_continuous(breaks = seq(1973, 2022, by = 1)) +
  labs(x = "Year", y = "Probability of Female Reproduction") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),
        legend.position = "bottom") +
  geom_text(aes(y = mean_fec + se_fec + 0.05, label = paste0("(", sample_size, ")")), size = 3, color = "black")

portion_filtered <- portion %>% select(DeerYear, portion)
fecundity_pred <- rut %>% left_join(portion_filtered, by = "DeerYear")

# Generate predictions for different density types
predictions_Hinds_fecundity_yr <- ggpredict(yr_Hinds_fecundity_age, terms = "Hinds [all]")
predictions_Hinds_fecundity_yr$Hinds <- predictions_Hinds_fecundity_yr$x
predictions_Hinds_fecundity_noyr <- ggpredict(Hinds_fecundity_age, terms = "Hinds [all]")
predictions_Hinds_fecundity_noyr$Hinds <- predictions_Hinds_fecundity_noyr$x

fecundity_pred_Hinds_yr <- fecundity_pred %>% left_join(predictions_Hinds_fecundity_yr, by = "Hinds")
fecundity_pred_Hinds_noyr <- fecundity_pred %>% left_join(predictions_Hinds_fecundity_noyr, by = "Hinds")

# Add a Type column to differentiate the data sets
fecundity_pred_Hinds_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Hinds <- rbind(fecundity_pred_Hinds_yr, fecundity_pred_Hinds_noyr)

# Keep only unique points for plotting
unique_fecundity_Hinds <- combined_fecundity_Hinds %>%
  distinct(Hinds, portion, .keep_all = TRUE)

Hind_Fec_terms <- paste(
  "Adjusted for:",
  "Hind Reproductive Status = Milk",
  "Hind's Age = 7",
  "Hind's Age Squared = 49",
  "Deer Year = 2000",
  sep = "\n"
)

# Plot the combined data
ggplot(combined_fecundity_Hinds, aes(x = Hinds, y = Fecundity)) +
  geom_jitter(width = 1, height = 0.01, alpha = 0.008, size = 2) +
  geom_line(aes(y = predicted, color = Type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +
  labs(x = "Hind Population Size",
       y = "Predicted Probability of Conception") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  annotate("text", x = 80, y = 0.25, label = Hind_Fec_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# Generate predictions for Adults
predictions_Adults_fecundity_yr <- ggpredict(yr_Adults_fecundity_age, terms = "Adults [all]")
predictions_Adults_fecundity_yr$Adults <- predictions_Adults_fecundity_yr$x
predictions_Adults_fecundity_noyr <- ggpredict(Adults_fecundity_age, terms = "Adults [all]")
predictions_Adults_fecundity_noyr$Adults <- predictions_Adults_fecundity_noyr$x

fecundity_pred_Adults_yr <- fecundity_pred %>% left_join(predictions_Adults_fecundity_yr, by = "Adults")
fecundity_pred_Adults_noyr <- fecundity_pred %>% left_join(predictions_Adults_fecundity_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
fecundity_pred_Adults_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Adults <- rbind(fecundity_pred_Adults_yr, fecundity_pred_Adults_noyr)

# Keep only unique points for plotting
unique_fecundity_Adults <- combined_fecundity_Adults %>%
  distinct(Adults, portion, .keep_all = TRUE)

# Plot the combined data
ggplot(combined_fecundity_Adults, aes(x = Adults, y = Fecundity)) +
  geom_point(shape = 1) +
  geom_point(data = unique_fecundity_Adults, aes(y = portion)) +
  geom_text(data = unique_fecundity_Adults, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +
  labs(x = "Adults Density",
       y = "Predicted Probability of female reproduction",
       title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Generate predictions for Total
predictions_Total_fecundity_yr <- ggpredict(yr_Total_fecundity_age, terms = "Total [all]")
predictions_Total_fecundity_yr$Total <- predictions_Total_fecundity_yr$x
predictions_Total_fecundity_noyr <- ggpredict(Total_fecundity_age, terms = "Total [all]")
predictions_Total_fecundity_noyr$Total <- predictions_Total_fecundity_noyr$x

fecundity_pred_Total_yr <- fecundity_pred %>% left_join(predictions_Total_fecundity_yr, by = "Total")
fecundity_pred_Total_noyr <- fecundity_pred %>% left_join(predictions_Total_fecundity_noyr, by = "Total")

# Add a Type column to differentiate the data sets
fecundity_pred_Total_yr$Type <- "With Year as Fixed effect"
fecundity_pred_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_Total <- rbind(fecundity_pred_Total_yr, fecundity_pred_Total_noyr)

# Keep only unique points for plotting
unique_fecundity_Total <- combined_fecundity_Total %>%
  distinct(Total, portion, .keep_all = TRUE)

# Plot the combined data
ggplot(combined_fecundity_Total, aes(x = Total, y = Fecundity)) +
  geom_point(shape = 1) +
  geom_point(data = unique_fecundity_Total, aes(y = portion)) +
  geom_text(data = unique_fecundity_Total, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +
  labs(x = "Total Density",
       y = "Predicted Probability of female reproduction",
       title = "Predicted Probability of female reproduction over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Generate predictions for LU_Total
predictions_LU_Total_fecundity_yr <- ggpredict(yr_LU_Total_fecundity_age, terms = "LU_Total [all]")
predictions_LU_Total_fecundity_yr$LU_Total <- predictions_LU_Total_fecundity_yr$x
predictions_LU_Total_fecundity_noyr <- ggpredict(LU_Total_fecundity_age, terms = "LU_Total [all]")
predictions_LU_Total_fecundity_noyr$LU_Total <- predictions_LU_Total_fecundity_noyr$x

fecundity_pred_LU_Total_yr <- fecundity_pred %>% left_join(predictions_LU_Total_fecundity_yr, by = "LU_Total")
fecundity_pred_LU_Total_noyr <- fecundity_pred %>% left_join(predictions_LU_Total_fecundity_noyr, by = "LU_Total")

# Add a Type column to differentiate the data sets
fecundity_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
fecundity_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_LU_Total <- rbind(fecundity_pred_LU_Total_yr, fecundity_pred_LU_Total_noyr)

# Keep only unique points for plotting
unique_fecundity_LU_Total <- combined_fecundity_LU_Total %>%
  distinct(LU_Total, portion, .keep_all = TRUE)

# Plot the combined data
ggplot(combined_fecundity_LU_Total, aes(x = LU_Total, y = Fecundity)) +
  geom_point(shape = 1) +
  geom_point(data = unique_fecundity_LU_Total, aes(y = portion)) +
  geom_text(data = unique_fecundity_LU_Total, aes(y = portion + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +
  labs(x = "LU_Total Density",
       y = "Predicted Probability of Conception",
       title = "Predicted Probability of Conception over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Print predictions to review
print(predictions_Hinds_fecundity_yr)
print(predictions_Hinds_fecundity_noyr)
print(predictions_Adults_fecundity_yr)
print(predictions_Adults_fecundity_noyr)
print(predictions_Total_fecundity_yr)
print(predictions_Total_fecundity_noyr)
print(predictions_LU_Total_fecundity_yr)
print(predictions_LU_Total_fecundity_noyr)
print(predictions_int_adult_fecundity_yr)

# Plot models with sjPlot
set_theme(base = theme_minimal()
          +theme(plot.title = element_blank(),
                 axis.line = element_line(color = "black"),
                 axis.title.x = element_text(size = 15),
                 axis.title.y = element_text(size = 15),
                 axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 legend.text = element_text(size = 10),
                 legend.title = element_blank()),
          legend.pos = "bottom")
plot_model(Hinds_fecundity_age, type = "pred", terms = "Hinds[all]")
plot_model(int_yr_Adults_fecundity_age, type = "pred", terms = c("DeerYear [all]", "ReprodStatus"))
plot_model(int_yr_Adults_fecundity_age, type = "pred", terms = c("Adults [all]"))

int_adults_sjplot_terms <- paste(
  "Adjusted for:",
  "Hind's Age = 7",
  "Hind's Age Squared = 49",
  "Deer Year = 2000",
  sep = "\n"
)

plot_model(int_yr_Adults_fecundity_age, 
           type = "pred", 
           terms = c("Adults [all]", "ReprodStatus"))+ labs(
             x = "Adult Population Size",
             y = "Predicted Probability of Conception",
               color = "Female Reproductive Status" ) +
  theme(  legend.title = element_text(size = 12, face = "plain"),  # Remove bold
          legend.justification = "center", 
          legend.title.position = "top"  # Position title above the legend
  ) + scale_y_continuous(
    limits = c(0, 1),               # Set y-axis limits from 0 to 1
    breaks = seq(0, 1, by = 0.25),   # Set y-axis breaks (e.g., 0.0, 0.1, ..., 1.0)
    labels = scales::number_format(accuracy = 0.01)  # Format labels as 0.00 to 1.00
  ) + labs(
    y = "Predicted Probability of Conception"  # Update y-axis title to reflect proportions
  )+
  annotate("text", x = 190, y = 0.25, label = int_adults_sjplot_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)


# Generate predictions for interaction terms
predictions_int_adult_fecundity_yr <- ggpredict(int_yr_Adults_fecundity_age, terms = "Adults [all]")
predictions_int_adult_fecundity_yr$Adults <- predictions_int_adult_fecundity_yr$x
predictions_int_adult_fecundity_noyr <- ggpredict(int_Adults_fecundity_age, terms = "Adults [all]")
predictions_int_adult_fecundity_noyr$Adults <- predictions_int_adult_fecundity_noyr$x

fecundity_pred_int_adults_yr <- fecundity_pred %>% left_join(predictions_int_adult_fecundity_yr, by = "Adults")
fecundity_pred_int_adults_noyr <- fecundity_pred %>% left_join(predictions_int_adult_fecundity_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
fecundity_pred_int_adults_yr$Type <- "With Year as Fixed effect"
fecundity_pred_int_adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_fecundity_int_adults <- rbind(fecundity_pred_int_adults_yr, fecundity_pred_int_adults_noyr)

# Keep only unique points for plotting
unique_fecundity_int_adults <- combined_fecundity_int_adults %>%
  distinct(Adults, portion, .keep_all = TRUE)

int_adults_Fec_terms <- paste(
  "Adjusted for:",
  "Hind Reproductive Status = Milk",
  "Hind's Age = 7",
  "Hind's Age Squared = 49",
  "Deer Year = 2000",
  sep = "\n"
)


# Plot the combined data
ggplot(combined_fecundity_int_adults, aes(x = Adults, y = Fecundity)) +
  geom_jitter(width = 1, height = 0.01, alpha = 0.008, size = 2) +
  geom_line(aes(y = predicted, color = Type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +
  labs(x = "Adult Population Size",
       y = "Predicted Probability of female reproduction") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  annotate("text", x = 190, y = 0.25, label = int_adults_Fec_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# Compare best model using ANOVA to determine significance of fixed effects
anova(noyr_noden_fecundity_age, Hinds_fecundity_age)
anova(yr_Hinds_fecundity_age, Hinds_fecundity_age)


# Compare nested models using ANOVA to determine significance of random effects
Hinds_fecundity_age_no_yrrandom <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + ReprodStatus
                               + (1 | Female),
                               family = binomial(link = "logit"), data = rut)
Hinds_fecundity_age_no_mumrandom <- glmmTMB(Fecundity ~ Hinds + Age + AgeSquared + ReprodStatus
                               + (1 | DeerYear),
                               family = binomial(link = "logit"), data = rut)
anova(Hinds_fecundity_age,Hinds_fecundity_age_no_yrrandom)
anova(Hinds_fecundity_age,Hinds_fecundity_age_no_mumrandom)


# Compare best model using ANOVA to determine significance of fixed effects
anova(noyr_noden_fecundity_age, int_yr_Adults_fecundity_age)
anova(yr_Hinds_fecundity_age, int_yr_Adults_fecundity_age)


int_yr_Adults_fecundity_age_noyrrandom <- glmmTMB(Fecundity ~ Adults * ReprodStatus + Age + AgeSquared + DeerYear
                                       + (1 | Female),
                                       family = binomial(link = "logit"), data = rut)
int_yr_Adults_fecundity_age_nofemrandom <- glmmTMB(Fecundity ~ Adults * ReprodStatus + Age + AgeSquared + DeerYear
                                       + (1 | DeerYear),
                                       family = binomial(link = "logit"), data = rut)

anova(int_yr_Adults_fecundity_age,int_yr_Adults_fecundity_age_noyrrandom)
anova(int_yr_Adults_fecundity_age,int_yr_Adults_fecundity_age_nofemrandom)
