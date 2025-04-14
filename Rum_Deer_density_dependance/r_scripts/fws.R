# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(lme4)        # For linear and generalized linear mixed-effects models
library(ggplot2)     # For data visualization
library(lmerTest)    # For linear mixed-effects models with tests in lme4
library(glmmTMB)     # For fitting generalized linear mixed models using Template Model Builder
library(ggeffects)   # For computing marginal effects from regression models

# Load the data
FWS <- read.csv("data/fws.csv")  # Load first winter survival data
Density <- read.csv("data/pop_est.csv")  # Load population density data

# Preprocess FWS data
FWS <- FWS %>% filter(!DeerYear %in% c(2022, 2023))  # Remove data from years 2022 and 2023
FWS$MumAgeSquared <- (FWS$MumAge) * (FWS$MumAge)  # Create a squared term for mother's age

# Select relevant columns from the Density data
PopD <- Density %>% select(DeerYear, Hinds, Stags, Calves, Calves_M, Calves_F, Adults, Total, LU_Total)

# Display the structure of the FWS data
str(FWS)

# Join FWS data with population density data
FWS <- FWS %>%
  left_join(PopD, by = "DeerYear")

# Remove rows with missing values
FWS <- na.omit(FWS)

# Check correlations between selected columns in the density data
cor(PopD[, c("Hinds", "Adults", "Total", "LU_Total")])
cor(Density[, c("Total", "Adults", "Hinds", "Stags", "Calves", "Calves_M", "Calves_F", "LU_Total")])

# Fit GLMMs with different population density metrics as predictors
# Including birth weight but excluding year as a fixed effect
Hinds <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear) + (1|MumCode), family = binomial(link = "logit"), data = FWS)
Adults <- glmmTMB(FWSurvival ~ Adults + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
Total <- glmmTMB(FWSurvival ~ Total + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
LU_Total <- glmmTMB(FWSurvival ~ LU_Total + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)

# Summarize the models
summary(Hinds)
summary(Adults)
summary(Total)
summary(LU_Total)

# Fit GLMMs excluding birth weight and year as a fixed effect
Hinds_noWt <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
Adults_noWt <- glmmTMB(FWSurvival ~ Adults + Sex + MotherStatus + MumAge + MumAgeSquared + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
Total_noWt <- glmmTMB(FWSurvival ~ Total + Sex + MotherStatus + MumAge + MumAgeSquared + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
LU_Total_noWt <- glmmTMB(FWSurvival ~ LU_Total + Sex + MotherStatus + MumAge + MumAgeSquared + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)

# Summarize the models without birth weight and year
summary(Hinds_noWt)
summary(Adults_noWt)
summary(Total_noWt)
summary(LU_Total_noWt)

# Compare models using AIC
AIC(Hinds_noWt, Adults_noWt, Total_noWt, LU_Total_noWt)

# Fit GLMMs excluding birth weight but including year as a fixed effect
yr_Hinds_noWt <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
yr_Adults_noWt <- glmmTMB(FWSurvival ~ Adults + Sex + MotherStatus + MumAge + MumAgeSquared + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
yr_Total_noWt <- glmmTMB(FWSurvival ~ Total + Sex + MotherStatus + MumAge + MumAgeSquared + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
yr_LU_Total_noWt <- glmmTMB(FWSurvival ~ LU_Total + Sex + MotherStatus + MumAge + MumAgeSquared + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)

# Summarize the models with year as a fixed effect
summary(yr_Hinds_noWt)
summary(yr_Adults_noWt)
summary(yr_Total_noWt)
summary(yr_LU_Total_noWt)

# Compare models using AIC
AIC(Hinds_noWt, Adults_noWt, Total_noWt, LU_Total_noWt)

# Fit GLMMs including both birth weight and year as fixed effects
yr_Hinds <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial(link = "logit"), data = FWS)
yr_Adults <- glmmTMB(FWSurvival ~ Adults + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
yr_Total <- glmmTMB(FWSurvival ~ Total + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)
yr_LU_Total <- glmmTMB(FWSurvival ~ LU_Total + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial, data = FWS)

# Summarize the models with both birth weight and year
summary(yr_Hinds)
summary(yr_Adults)
summary(yr_Total)
summary(yr_LU_Total)

# Fit a GLMM without density predictors but with birth weight and year as fixed effects
yr_noden <- glmmTMB(FWSurvival ~ Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), family = binomial(link = "logit"), data = FWS)
summary(yr_noden)

# Fit a GLMM without density predictors and without year as a fixed effect
noyr_noden <- glmmTMB(FWSurvival ~ Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear) + (1|MumCode), family = binomial(link = "logit"), data = FWS)
summary(noyr_noden)

# Plotting First Winter Survival (FWS) rate over years
rate <- FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),                         # Total number of observations
    survived = sum(FWSurvival, na.rm = TRUE),  # Total number of calves that survived
    rate = survived / count,             # Survival rate
    Mrate = 1 - rate                     # Mortality rate
  ) %>%
  left_join(PopD, by = "DeerYear")

rate <- na.omit(rate)

PopD49 <- PopD%>% filter(!DeerYear %in% c(2022))

cor(rate$rate,PopD49$Hinds)

# Create similar plots for male and female calves separately
male_rate <- male_FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived / count,
    Mrate = 1 - rate
  ) %>%
  left_join(PopD, by = "DeerYear")

fem_rate <- fem_FWS %>%
  group_by(DeerYear) %>%
  summarize(
    count = n(),
    survived = sum(FWSurvival, na.rm = TRUE),
    rate = survived / count,
    Mrate = 1 - rate
  ) %>%
  left_join(PopD, by = "DeerYear")

# Plot overall first winter survival rate over years
ggplot(rate, aes(x = DeerYear, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "First Winter Survival Rate over Year") +
  ylab("First Winter Survival Rate") + xlab("Year") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot average first winter survival probability over years with error bars
AvgFWS <- FWS %>%
  group_by(DeerYear) %>%
  summarise(
    mean_FWS = mean(FWSurvival),
    sample_size = n(),
    se_FWS = sd(FWSurvival) / sqrt(n())
  )

ggplot(data = AvgFWS, aes(x = DeerYear, y = mean_FWS)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_FWS - se_FWS, ymax = mean_FWS + se_FWS), width = 0.2, color = "black") +
  scale_x_continuous(breaks = seq(1973, 2021, by = 1)) +
  labs(x = "Year", y = "Probability of Surviving First Winter") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),
        legend.position = "bottom") +
  geom_text(aes(y = mean_FWS + se_FWS + 0.05, label = paste0("(", sample_size, ")")), size = 3, color = "black")

# Plot FWS rate over different density measures
ggplot(rate, aes(x = Hinds, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "First Winter Survival Rate over Density") +
  ylab("First Winter Survival Rate") + xlab("Number of Hinds") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(rate, aes(x = Adults, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "First Winter Survival Rate over Density") +
  ylab("First Winter Survival Rate") + xlab("Number of Hinds and Stags") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(rate, aes(x = Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "First Winter Survival Rate over Density") +
  ylab("First Winter Survival Rate") + xlab("Number of Hinds, Stags, and Calves") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(rate, aes(x = LU_Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "First Winter Survival Rate over Density") +
  ylab("First Winter Survival Rate") + xlab("Livestock Unit") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot FWS rate over different density measures for female calves
ggplot(fem_rate, aes(x = Hinds, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Female First Winter Survival Rate over Density") +
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(fem_rate, aes(x = Adults, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Female First Winter Survival Rate over Density") +
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds and Stags") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(fem_rate, aes(x = Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Female First Winter Survival Rate over Density") +
  ylab("Female First Winter Survival Rate") + xlab("Number of Hinds, Stags, and Calves") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(fem_rate, aes(x = LU_Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Female First Winter Survival Rate over Density") +
  ylab("Female First Winter Survival Rate") + xlab("Livestock Unit") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Plot FWS rate over different density measures for male calves
ggplot(male_rate, aes(x = Hinds, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Male First Winter Survival Rate over Density") +
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(male_rate, aes(x = Adults, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Male First Winter Survival Rate over Density") +
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds and Stags") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(male_rate, aes(x = Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Male First Winter Survival Rate over Density") +
  ylab("Male First Winter Survival Rate") + xlab("Number of Hinds, Stags, and Calves") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

ggplot(male_rate, aes(x = LU_Total, y = rate)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  labs(title = "Male First Winter Survival Rate over Density") +
  ylab("Male First Winter Survival Rate") + xlab("Livestock Unit") +
  theme_minimal() +
  geom_text(aes(y = rate + 0.03, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black")

# Generate predictions for Hinds density models with and without year as a fixed effect
rate_filtered <- rate %>% select(DeerYear, rate)
FWS_pred <- FWS %>% left_join(rate_filtered, by = "DeerYear")

pred_Hinds <- ggpredict(Hinds, terms = "Hinds [all]")
pred_yr_Hinds <- ggpredict(yr_Hinds, terms = "Hinds [all]")
pred_Hinds$Hinds <- pred_Hinds$x
pred_yr_Hinds$Hinds <- pred_yr_Hinds$x

# Summarize the model for Hinds without year as a fixed effect
summary(Hinds)

# Create a table of the Hinds model using tab_model
tab_model(Hinds, transform = NULL, digits = 4, show.stat = TRUE, show.se = TRUE)

# Join the predictions with the original FWS data
pred_Hinds <- pred_Hinds %>% left_join(FWS_pred, by = "Hinds")
pred_yr_Hinds <- pred_yr_Hinds %>% left_join(FWS_pred, by = "Hinds")

# Convert predictions to data frames
df_Hinds <- as.data.frame(pred_Hinds)
df_yr_Hinds <- as.data.frame(pred_yr_Hinds)

# Add a Type column to differentiate the data sets with descriptive labels
df_Hinds$Type <- "Without Year as Fixed Effect"
df_yr_Hinds$Type <- "With Year as Fixed Effect"

# Combine the data frames for plotting
combined_data <- rbind(df_Hinds, df_yr_Hinds)

# Keep only unique points for plotting
unique_points <- combined_data %>%
  distinct(Hinds, rate, .keep_all = TRUE)

# Prepare annotation text for the plot
Hind_FWS_terms <- paste(
  "Adjusted for:",
  "Sex = Male",
  "Mother's Reproductive Status = True yeld",
  "Mother's Age = 8",
  "Mother's Age Squared = 64",
  "Birth Weight = 6.51 kg",
  "Deer Year = 1999",
  sep = "\n"
)

# Plot combined predictions with different effects for Hinds density
ggplot(combined_data, aes(x = x, y = rate)) +
  geom_jitter(aes(y = FWSurvival), width = 1, height = 0.01, alpha = 0.008, size = 2) +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Hind Population Size",
    y = "Predicted Probability of Surviving First Winter"
  ) +
  scale_color_manual(values = c("With Year as Fixed Effect" = "blue", "Without Year as Fixed Effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed Effect" = "lightblue", "Without Year as Fixed Effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.5),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  annotate("text", x = 80, y = 0.3, label = Hind_FWS_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# Repeat similar processes for Adult, Total, and LU_Total density models


#Adult Plots

pred_Adults <- ggpredict(Adults, terms = "Adults [all]")
pred_yr_Adults <- ggpredict(yr_Adults, terms = "Adults [all]")

pred_Adults$Adults <- pred_Adults$x
pred_yr_Adults$Adults <- pred_yr_Adults$x

pred_Adults<- pred_Adults %>% left_join(FWS_pred, by= "Adults")
pred_yr_Adults<- pred_yr_Adults %>% left_join(FWS_pred, by= "Adults")

# Convert to data frames
df_Adults <- as.data.frame(pred_Adults)
df_yr_Adults <- as.data.frame(pred_yr_Adults)

# Add a Type column to differentiate the data sets with descriptive labels
df_Adults$Type <- "Without Year as Fixed Effect"
df_yr_Adults$Type <- "With Year as Fixed Effect"

# Combine the data frames
combined_Adults <- rbind(df_Adults, df_yr_Adults)

# Keep only unique points for plotting
unique_Adults <- combined_Adults %>%
  distinct(Adults, rate, .keep_all = TRUE)

ggplot(combined_Adults, aes(x = x, y = rate)) +
  geom_point(data = unique_Adults, aes(y = rate)) +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Adults Density",
    y = "Predicted Survival Probability",
    title = "Predicted Survival Probability over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed Effect" = "blue", "Without Year as Fixed Effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed Effect" = "lightblue", "Without Year as Fixed Effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )


#Total Plots
pred_Total <- ggpredict(Total, terms = "Total [all]")
pred_yr_Total <- ggpredict(yr_Total, terms = "Total [all]")

pred_Total$Total <- pred_Total$x
pred_yr_Total$Total <- pred_yr_Total$x

pred_Total<- pred_Total %>% left_join(FWS_pred, by= "Total")
pred_yr_Total<- pred_yr_Total %>% left_join(FWS_pred, by= "Total")

# Convert to data frames
df_Total <- as.data.frame(pred_Total)
df_yr_Total <- as.data.frame(pred_yr_Total)

# Add a Type column to differentiate the data sets with descriptive labels
df_Total$Type <- "Without Year as Fixed Effect"
df_yr_Total$Type <- "With Year as Fixed Effect"

# Combine the data frames
combined_Total <- rbind(df_Total, df_yr_Total)

# Keep only unique points for plotting
unique_Total <- combined_Total %>%
  distinct(Total, rate, .keep_all = TRUE)

ggplot(combined_Total, aes(x = x, y = rate)) +
  geom_point(data = unique_Total, aes(y = rate)) +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Total Density",
    y = "Predicted Survival Probability",
    title = "Predicted Survival Probability over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed Effect" = "blue", "Without Year as Fixed Effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed Effect" = "lightblue", "Without Year as Fixed Effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )


#LU_Total Plots
pred_LU_Total <- ggpredict(LU_Total, terms = "LU_Total [all]")
pred_yr_LU_Total <- ggpredict(yr_LU_Total, terms = "LU_Total [all]")

pred_LU_Total$LU_Total <- pred_LU_Total$x
pred_yr_LU_Total$LU_Total <- pred_yr_LU_Total$x

pred_LU_Total<- pred_LU_Total %>% left_join(FWS_pred, by= "LU_Total")
pred_yr_LU_Total<- pred_yr_LU_Total %>% left_join(FWS_pred, by= "LU_Total")

# Convert to data frames
df_LU_Total <- as.data.frame(pred_LU_Total)
df_yr_LU_Total <- as.data.frame(pred_yr_LU_Total)

# Add a Type column to differentiate the data sets with descriptive labels
df_LU_Total$Type <- "Without Year as Fixed Effect"
df_yr_LU_Total$Type <- "With Year as Fixed Effect"

# Combine the data frames
combined_LU_Total <- rbind(df_LU_Total, df_yr_LU_Total)

# Keep only unique points for plotting
unique_LU_Total <- combined_LU_Total %>%
  distinct(LU_Total, rate, .keep_all = TRUE)

ggplot(combined_LU_Total, aes(x = x, y = rate)) +
  geom_point(data = unique_LU_Total, aes(y = rate)) +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "LU_Total Density",
    y = "Predicted Survival Probability",
    title = "Predicted Survival Probability over Density\nwith 95% Confidence Interval"
  ) +
  scale_color_manual(values = c("With Year as Fixed Effect" = "blue", "Without Year as Fixed Effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed Effect" = "lightblue", "Without Year as Fixed Effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Optionally remove the legend title
  )



print(pred_Hinds)
print(pred_yr_Hinds)
print(pred_Adults)
print(pred_yr_Adults)
print(pred_Total)
print(pred_yr_Total)
print(pred_LU_Total)
print(pred_yr_LU_Total)

# Compare best model ANOVA to determine significance of fixed effects
anova(noyr_noden, Hinds)
# Compare nested models using ANOVA to determine significance of random effects
Hinds_no_yrrandom <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|MumCode), family = binomial(link = "logit"), data = FWS)
Hinds_no_mumrandom <- glmmTMB(FWSurvival ~ Hinds + Sex + MotherStatus + MumAge + MumAgeSquared + BirthWt + (1|DeerYear), family = binomial(link = "logit"), data = FWS)

anova(Hinds,Hinds_no_yrrandom)
anova(Hinds,Hinds_no_mumrandom)
