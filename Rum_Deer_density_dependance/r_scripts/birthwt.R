# Load necessary libraries for data manipulation, statistical modeling, and visualization
library(tidyverse)  # For data manipulation and visualization
library(lme4)       # For fitting linear mixed-effects models
library(ggplot2)    # For creating plots
library(lmerTest)   # For adding p-values to mixed models
library(glmmTMB)    # For fitting generalized linear mixed models
library(ggeffects)  # For generating predicted values from models
library(sjPlot)     # For creating summary tables and visualizing model results

# Create data frames for individual codes from different datasets
ind_bw <- birthwt[,"Code", drop = FALSE]
ind_fws <- FWS[,"Code", drop = FALSE]
ind_spike <- Spike_pred[,"Code", drop = FALSE]
ind_fecundity <- unique_rut[,"Female", drop = FALSE]
names(ind_fecundity) <- "Code"  # Rename column for consistency

# Combine individual codes into one data frame and remove duplicates
all_individuals <- rbind(ind_bw, ind_fws, ind_spike, ind_fecundity)
all_individuals <- unique(all_individuals)

# Read in external datasets
birthwt <- read.csv("data/birthwt.csv")  # Birth weight and mother's age data
Density <- read.csv("data/pop_est.csv")  # Population estimates data

# Select relevant columns from Density data
PopD <- Density %>% select(DeerYear, Hinds, Stags, Calves, Calves_M, Calves_F, Adults, Total, LU_Total)

# Create a new column for the square of MumAge
birthwt$MumAgeSquared <- (birthwt$MumAge) * (birthwt$MumAge)

# Create a new column for DeerYear, identical to BirthYear
birthwt$DeerYear <- birthwt$BirthYear

# Join birthwt data with population density data on DeerYear
birthwt <- birthwt %>%
  left_join(PopD, by = "DeerYear")

# Fit a simple GLMM with hind density as a fixed effect
Hinds_bw_simple <- glmmTMB(BirthWt ~ Hinds
                           + Sex
                           + DeerYear
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

# Summarize the model results
summary(Hinds_bw_simple)

# Fit a GLMM without DeerYear as a fixed effect
Hinds_bw_noyr <- glmmTMB(BirthWt ~ Hinds
                         + Sex
                         + (1|DeerYear)
                         + (1|MumCode),
                         family = gaussian(), data = birthwt)
summary(Hinds_bw_noyr)

# Fit a GLMM with additional covariates (DaysFrom1May) and DeerYear as a fixed effect
Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                    + Sex
                    + DaysFrom1May
                    + DeerYear
                    + (1|DeerYear)
                    + (1|MumCode),
                    family = gaussian(), data = birthwt)

# Fit GLMMs with different population metrics as fixed effects
Adults_bw <- glmmTMB(BirthWt ~ Adults
                     + Sex
                     + DaysFrom1May
                     + DeerYear
                     + (1|DeerYear)
                     + (1|MumCode),
                     family = gaussian(), data = birthwt)

Total_bw <- glmmTMB(BirthWt ~ Total
                    + Sex
                    + DaysFrom1May
                    + DeerYear
                    + (1|DeerYear)
                    + (1|MumCode),
                    family = gaussian(), data = birthwt)

LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                       + Sex
                       + DaysFrom1May
                       + DeerYear
                       + (1|DeerYear)
                       + (1|MumCode),
                       family = gaussian(), data = birthwt)

# Summarize model results for each population metric
summary(Hinds_bw)
summary(Adults_bw)
summary(Total_bw)
summary(LU_Total_bw)

# Fit GLMMs including MumAge and MumAgeSquared as covariates
Age_Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

Age_Adults_bw <- glmmTMB(BirthWt ~ Adults
                         + Sex
                         + DaysFrom1May
                         + DeerYear
                         + MumAge
                         + MumAgeSquared
                         + (1|DeerYear)
                         + (1|MumCode),
                         family = gaussian(), data = birthwt)

Age_Total_bw <- glmmTMB(BirthWt ~ Total
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

Age_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + MumAge
                           + MumAgeSquared
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

# Summarize results for models with MumAge included
summary(Age_Hinds_bw)
summary(Age_Adults_bw)
summary(Age_Total_bw)
summary(Age_LU_Total_bw)

# Fit GLMMs including MotherStatus as a covariate
Status_Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + MotherStatus
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

Status_Adults_bw <- glmmTMB(BirthWt ~ Adults
                            + Sex
                            + DaysFrom1May
                            + DeerYear
                            + MotherStatus
                            + (1|DeerYear)
                            + (1|MumCode),
                            family = gaussian(), data = birthwt)

Status_Total_bw <- glmmTMB(BirthWt ~ Total
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + MotherStatus
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

Status_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                              + Sex
                              + DaysFrom1May
                              + DeerYear
                              + MotherStatus
                              + (1|DeerYear)
                              + (1|MumCode),
                              family = gaussian(), data = birthwt)

# Summarize results for models with MotherStatus included
summary(Status_Hinds_bw)
summary(Status_Adults_bw)
summary(Status_Total_bw)
summary(Status_LU_Total_bw)

# Fit GLMMs including MumAge, MumAgeSquared, and MotherStatus as covariates
Mum_Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + MotherStatus
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

Mum_Adults_bw <- glmmTMB(BirthWt ~ Adults
                         + Sex
                         + DaysFrom1May
                         + DeerYear
                         + MumAge
                         + MumAgeSquared
                         + MotherStatus
                         + (1|DeerYear)
                         + (1|MumCode),
                         family = gaussian(), data = birthwt)

Mum_Total_bw <- glmmTMB(BirthWt ~ Total
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + MotherStatus
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

Mum_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + MumAge
                           + MumAgeSquared
                           + MotherStatus
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

# Summarize results for models including MumAge, MumAgeSquared, and MotherStatus
summary(Mum_Hinds_bw)
summary(Mum_Adults_bw)
summary(Mum_Total_bw)
summary(Mum_LU_Total_bw)

# Fit a GLMM without population density metrics, focusing on individual and maternal factors
Mum_noden_bw <- glmmTMB(BirthWt ~ Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + MotherStatus
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)
summary(Mum_noden_bw)


# Fit a GLMM without DeerYear as a fixed effect
Mum_noyr_noden_bw <- glmmTMB(BirthWt ~ Sex
                             + DaysFrom1May
                             + MumAge
                             + MumAgeSquared
                             + MotherStatus
                             + (1|DeerYear)
                             + (1|MumCode),
                             family = gaussian(), data = birthwt)
summary(Mum_noyr_noden_bw)

tab_model(Mum_noyr_noden_bw)
tab_model(Mum_noden_bw)
tab_model(noyr_Mum_Hinds_bw)
tab_model(Mum_Hinds_bw)
tab_model(noyr_Mum_Adults_bw)
tab_model(Mum_Adults_bw)
tab_model(noyr_Mum_Total_bw)
tab_model(Mum_Total_bw)
tab_model(noyr_Mum_LU_Total_bw)
tab_model(Mum_LU_Total_bw)



# Fit GLMMs without DeerYear as a fixed effect but with population density metrics
noyr_Mum_Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                             + Sex
                             + DaysFrom1May
                             + MumAge
                             + MumAgeSquared
                             + MotherStatus
                             + (1|DeerYear)
                             + (1|MumCode),
                             family = gaussian(), data = birthwt)

noyr_Mum_Adults_bw <- glmmTMB(BirthWt ~ Adults
                              + Sex
                              + DaysFrom1May
                              + MumAge
                              + MumAgeSquared
                              + MotherStatus
                              + (1|DeerYear)
                              + (1|MumCode),
                              family = gaussian(), data = birthwt)

noyr_Mum_Total_bw <- glmmTMB(BirthWt ~ Total
                             + Sex
                             + DaysFrom1May
                             + MumAge
                             + MumAgeSquared
                             + MotherStatus
                             + (1|DeerYear)
                             + (1|MumCode),
                             family = gaussian(), data = birthwt)

noyr_Mum_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                                + Sex
                                + DaysFrom1May
                                + MumAge
                                + MumAgeSquared
                                + MotherStatus
                                + (1|DeerYear)
                                + (1|MumCode),
                                family = gaussian(), data = birthwt)

# Summarize results for models without DeerYear as a fixed effect but with population density metrics
summary(noyr_Mum_Hinds_bw)
summary(noyr_Mum_Adults_bw)
summary(noyr_Mum_Total_bw)
summary(noyr_Mum_LU_Total_bw)

# Compare nested models using ANOVA to determine significance of fixed effects
anova(Mum_noyr_noden_bw, reduced_bw_noyr)
anova(Mum_noyr_noden_bw, reduced_bw_nomumid)
anova(Mum_noden_bw, Mum_noyr_noden_bw)
anova(Mum_noyr_noden_bw, noyr_Mum_Hinds_bw)
anova(Mum_noyr_noden_bw, noyr_Mum_Adults_bw)
anova(Mum_noyr_noden_bw, noyr_Mum_Total_bw)
anova(Mum_noyr_noden_bw, noyr_Mum_LU_Total_bw)

# Plot predicted values from the best-fitting model(s)
plot_model(Mum_noyr_noden_bw, type = "pred", terms = c("DeerYear [all]", "MotherStatus"))
plot_model(Mum_noyr_noden_bw, type = "pred", terms = "MotherStatus")
plot_model(Mum_noyr_noden_bw)

# Create a summary table for the best-fitting model(s) and visualize results
summary(Mum_noyr_noden_bw)
tab_model(Mum_noyr_noden_bw, show.stat = TRUE, show.se = TRUE)
plot_model(Mum_noyr_noden_bw)

# Fit GLMMs with interactions between population density and MotherStatus
int_Hinds_bw <- glmmTMB(BirthWt ~ Hinds * MotherStatus
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

int_Adults_bw <- glmmTMB(BirthWt ~ Adults * MotherStatus
                         + Sex
                         + DaysFrom1May
                         + DeerYear
                         + MumAge
                         + MumAgeSquared
                         + (1|DeerYear)
                         + (1|MumCode),
                         family = gaussian(), data = birthwt)

int_Total_bw <- glmmTMB(BirthWt ~ Total * MotherStatus
                        + Sex
                        + DaysFrom1May
                        + DeerYear
                        + MumAge
                        + MumAgeSquared
                        + (1|DeerYear)
                        + (1|MumCode),
                        family = gaussian(), data = birthwt)

int_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total * MotherStatus
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + MumAge
                           + MumAgeSquared
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

# Summarize results for interaction models
summary(int_Hinds_bw)
summary(int_Adults_bw)
summary(int_Total_bw)
summary(int_LU_Total_bw)

# Fit simple GLMMs with individual population density metrics as fixed effects
simple_Hinds_bw <- glmmTMB(BirthWt ~ Hinds
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

simple_Adults_bw <- glmmTMB(BirthWt ~ Adults
                            + Sex
                            + DaysFrom1May
                            + DeerYear
                            + (1|DeerYear)
                            + (1|MumCode),
                            family = gaussian(), data = birthwt)

simple_Total_bw <- glmmTMB(BirthWt ~ Total
                           + Sex
                           + DaysFrom1May
                           + DeerYear
                           + (1|DeerYear)
                           + (1|MumCode),
                           family = gaussian(), data = birthwt)

simple_LU_Total_bw <- glmmTMB(BirthWt ~ LU_Total
                              + Sex
                              + DaysFrom1May
                              + DeerYear
                              + (1|DeerYear)
                              + (1|MumCode),
                              family = gaussian(), data = birthwt)

# Summarize results for simple models
summary(simple_Hinds_bw)
summary(simple_Adults_bw)
summary(simple_Total_bw)
summary(simple_LU_Total_bw)

# Remove rows with missing data
birthwt <- na.omit(birthwt)

# Calculate average birth weight by DeerYear, along with sample size and standard error
AvgBw <- birthwt %>%
  group_by(DeerYear) %>%
  summarise(
    mean_Bw = mean(BirthWt),
    sample_size = n(),
    se_Bw = sd(BirthWt) / sqrt(n())
  )

# Filter out early years (1968-1972) for plotting
AvgBw73 <- AvgBw %>% filter(!DeerYear %in% c(1968, 1969, 1970, 1971, 1972))

# Plot mean birth weight over years with error bars
ggplot(data = AvgBw73, aes(x = DeerYear, y = mean_Bw)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_Bw - se_Bw, ymax = mean_Bw + se_Bw), width = 0.2, color = "black") +
  scale_x_continuous(breaks = seq(1973, 2022, by = 1)) +
  labs(x = "Year of Birth", y = "Mean Birth Weight (kg)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),  # Remove background gridlines
        legend.position = "bottom") +
  geom_text(aes(y = mean_Bw + se_Bw + 0.15, label = paste0("(", sample_size, ")")), size = 3, color = "black")

# Hind Density Predictions and Visualization ----

# Prepare data for predictions by joining AvgBw with birthwt
AvgBw_filtered <- AvgBw %>% select(DeerYear, mean_Bw, sample_size, se_Bw)
Bw_pred <- birthwt %>% left_join(AvgBw_filtered, by = "DeerYear")

# Generate predictions for Hind density with and without year as a fixed effect
predictions_Hinds_Bw_yr <- ggpredict(Mum_Hinds_bw, terms = "Hinds [all]")
predictions_Hinds_Bw_yr$Hinds <- predictions_Hinds_Bw_yr$x

predictions_Hinds_Bw_noyr <- ggpredict(noyr_Mum_Hinds_bw, terms = "Hinds [all]")
predictions_Hinds_Bw_noyr$Hinds <- predictions_Hinds_Bw_noyr$x

# Join predictions with observed data
Bw_pred_Hinds_yr <- Bw_pred %>% left_join(predictions_Hinds_Bw_yr, by = "Hinds")
Bw_pred_Hinds_noyr <- Bw_pred %>% left_join(predictions_Hinds_Bw_noyr, by = "Hinds")

# Add a Type column to differentiate the data sets
Bw_pred_Hinds_yr$Type <- "With Year as Fixed effect"
Bw_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Hinds <- rbind(Bw_pred_Hinds_yr, Bw_pred_Hinds_noyr)

# Keep only unique points for plotting
unique_Bw_Hinds <- combined_Bw_Hinds %>%
  distinct(Hinds, mean_Bw, .keep_all = TRUE)

# Plot the combined data with predictions and confidence intervals
ggplot(combined_Bw_Hinds, aes(x = Hinds, y = mean_Bw)) +
  geom_point(shape = 1) +
  geom_point(data = unique_Bw_Hinds) +
  geom_text(data = unique_Bw_Hinds, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Hind Density", y = "Mean Birth Weight (kg)", title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# Adults Predictions and Visualization ----

# Generate predictions for Adult population size with and without year as a fixed effect
predictions_Adults_Bw_yr <- ggpredict(Mum_Adults_bw, terms = "Adults [all]")
predictions_Adults_Bw_yr$Adults <- predictions_Adults_Bw_yr$x

predictions_Adults_Bw_noyr <- ggpredict(noyr_Mum_Adults_bw, terms = "Adults [all]")
predictions_Adults_Bw_noyr$Adults <- predictions_Adults_Bw_noyr$x

# Join predictions with observed data
Bw_pred_Adults_yr <- Bw_pred %>% left_join(predictions_Adults_Bw_yr, by = "Adults")
Bw_pred_Adults_noyr <- Bw_pred %>% left_join(predictions_Adults_Bw_noyr, by = "Adults")

# Add a Type column to differentiate the data sets
Bw_pred_Adults_yr$Type <- "With Year as Fixed effect"
Bw_pred_Adults_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Adults <- rbind(Bw_pred_Adults_yr, Bw_pred_Adults_noyr)

# Keep only unique points for plotting
unique_Bw_Adults <- combined_Bw_Adults %>%
  distinct(Adults, mean_Bw, .keep_all = TRUE)

# Prepare annotation text
Adults_Bw_terms <- paste(
  "Adjusted for:",
  "Sex = Male",
  "Mother's Reproductive Status = True yeld",
  "Mother's Age = 8",
  "Mother's Age Squared = 64",
  "Days From 1st May = 35",
  "Deer Year = 2001",
  sep = "\n"
)

# Plot the combined data with predictions and confidence intervals
ggplot(combined_Bw_Adults, aes(x = Adults, y = mean_Bw)) +
  geom_point(shape = 1) +
  geom_point(data = unique_Bw_Adults) +
  geom_text(data = unique_Bw_Adults, aes(y = mean_Bw + 0.05, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Adult Population Size", y = "Mean Birth Weight (kg)") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),  # Remove background gridlines
        legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_blank()) +  # Optionally remove the legend title
  annotate("text", x = 185, y = 6.31, label = Adults_Bw_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# Total Density Predictions and Visualization ----

# Generate predictions for Total density with and without year as a fixed effect
predictions_Total_Bw_yr <- ggpredict(Mum_Total_bw, terms = "Total [all]")
predictions_Total_Bw_yr$Total <- predictions_Total_Bw_yr$x

predictions_Total_Bw_noyr <- ggpredict(noyr_Mum_Total_bw, terms = "Total [all]")
predictions_Total_Bw_noyr$Total <- predictions_Total_Bw_noyr$x

# Join predictions with observed data
Bw_pred_Total_yr <- Bw_pred %>% left_join(predictions_Total_Bw_yr, by = "Total")
Bw_pred_Total_noyr <- Bw_pred %>% left_join(predictions_Total_Bw_noyr, by = "Total")

# Add a Type column to differentiate the data sets
Bw_pred_Total_yr$Type <- "With Year as Fixed effect"
Bw_pred_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_Total <- rbind(Bw_pred_Total_yr, Bw_pred_Total_noyr)

# Keep only unique points for plotting
unique_Bw_Total <- combined_Bw_Total %>%
  distinct(Total, mean_Bw, .keep_all = TRUE)

# Plot the combined data with predictions and confidence intervals
ggplot(combined_Bw_Total, aes(x = Total, y = mean_Bw)) +
  geom_point(shape = 1) +
  geom_point(data = unique_Bw_Total) +
  geom_text(data = unique_Bw_Total, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Total Density", y = "Mean Birth Weight (kg)", title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# LU_Total Density Predictions and Visualization ----

# Generate predictions for LU_Total density with and without year as a fixed effect
predictions_LU_Total_Bw_yr <- ggpredict(Mum_LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_Bw_yr$LU_Total <- predictions_LU_Total_Bw_yr$x

predictions_LU_Total_Bw_noyr <- ggpredict(noyr_Mum_LU_Total_bw, terms = "LU_Total [all]")
predictions_LU_Total_Bw_noyr$LU_Total <- predictions_LU_Total_Bw_noyr$x

# Join predictions with observed data
Bw_pred_LU_Total_yr <- Bw_pred %>% left_join(predictions_LU_Total_Bw_yr, by = "LU_Total")
Bw_pred_LU_Total_noyr <- Bw_pred %>% left_join(predictions_LU_Total_Bw_noyr, by = "LU_Total")

# Add a Type column to differentiate the data sets
Bw_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
Bw_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"

# Combine the data frames
combined_Bw_LU_Total <- rbind(Bw_pred_LU_Total_yr, Bw_pred_LU_Total_noyr)

# Keep only unique points for plotting
unique_Bw_LU_Total <- combined_Bw_LU_Total %>%
  distinct(LU_Total, mean_Bw, .keep_all = TRUE)

# Plot the combined data with predictions and confidence intervals
ggplot(combined_Bw_LU_Total, aes(x = LU_Total, y = mean_Bw)) +
  geom_point(shape = 1) +
  geom_point(data = unique_Bw_LU_Total) +
  geom_text(data = unique_Bw_LU_Total, aes(y = mean_Bw + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "LU_Total Density", y = "Mean Birth Weight (kg)", title = "Predicted Mean Birth Weight over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# Print prediction results for inspection
print(predictions_Hinds_Bw_noyr)
print(predictions_Hinds_Bw_yr)
print(predictions_Adults_Bw_noyr)
print(predictions_Adults_Bw_yr)
print(predictions_Total_Bw_noyr)
print(predictions_Total_Bw_yr)
print(predictions_LU_Total_Bw_noyr)
print(predictions_LU_Total_Bw_yr)
