# Load required packages for data manipulation, statistical modeling, and visualization
library(tidyverse)  # Comprehensive collection of R packages for data science
library(lme4)       # For fitting linear mixed-effects models
library(ggplot2)    # For creating graphics and visualizations
library(lmerTest)   # Provides p-values in linear mixed models
library(glmmTMB)    # For fitting generalized linear mixed models (GLMMs)
library(ggeffects)  # For creating marginal effects plots
library(cowplot)    # For combining multiple plots

# Load the datasets used in the analysis
Spike <- read.csv("data/spike.csv")               # Primary data including spike length
nowt <- read.csv("data/spike_nowt.csv")           # Data without birth weight to replicate Schmidt et al. 2001
birth <- read.csv("data/spike_nowt.csv")          # Used to replicate tests on birth year in Schmidt et al. 2001
birth_w_wt <- read.csv("data/spike.csv")          # Data with birth weight
Density <- read.csv("data/pop_est.csv")  # Deer population density data

# Adjust the birth year by subtracting 1 from the observed year of yearlings
birth$DeerYear <- birth$DeerYear - 1
birth_w_wt$DeerYear <- birth_w_wt$DeerYear - 1

# Remove data without density records (specifically the year 1969)
Spike <- Spike %>% filter(!DeerYear %in% c(1969))
nowt <- nowt %>% filter(!DeerYear %in% c(1969))

# Extract relevant density metrics from the population density dataset
PopD <- Density %>% select(DeerYear, Stags, Hinds, Calves, Adults, Total, LU_Total)

# Add additional hind density records for the Schmidt paper
PopD7172 <- data.frame(
  DeerYear = c(1971, 1972),
  Hinds = c(57, 66)
)

# Combine the new data with the existing population density data
PopD_new <- bind_rows(PopD7172, PopD)

# Load the dataset that includes data for years 1971-1972
nowt7172 <- read.csv("data/Spike_nowt.csv")

# Plot the population density metrics over the years
ggplot(PopD, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 1) +
  geom_line(aes(y = Adults, color = "Adults"), size = 1) + 
  geom_line(aes(y = Total, color = "Total"), size = 1) +
  geom_line(aes(y = LU_Total, color = "LU_Total"), size = 1) +
  geom_hline(yintercept = c(100, 200, 300, 400), color = 'darkgrey', linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1973, 2022, by = 1)) +
  labs(x = "Year", y = "Population Size") +
  scale_color_manual(name = "Density Metrics", values = c("Hinds" = "red", "Adults" = "black", "Total" = "#ff7f0e", "LU_Total" = "#2ca02c"),
                     breaks = c("Hinds", "Stags", "Adults", "Calves", "Total", "LU_Total"),
                     labels = c("Hinds", "Stags", "Adults", "Calves", "Total", "Livestock Units")) +
  theme(axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),  # Remove background gridlines
        legend.position = "bottom")

# Plot the number of stags, hinds, and calves over the years
ggplot(PopD, aes(x = DeerYear)) +
  geom_line(aes(y = Hinds, color = "Hinds"), size = 1) +
  geom_line(aes(y = Stags, color = "Stags"), size = 1) +
  geom_line(aes(y = Calves, color = "Calves"), size = 1) +
  geom_hline(yintercept = c(50, 100, 150, 200), color = 'darkgrey', linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1973, 2022, by = 1)) +
  labs(x = "Year", y = "Population Size") +
  scale_color_manual(name = "Age and Sex Classes",
                     values = c("Hinds" = "red", "Stags" = "blue", "Calves" = "brown"),
                     breaks = c("Hinds", "Stags", "Calves"),
                     labels = c("Hinds", "Stags", "Calves")) +
  theme(axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),  # Remove background gridlines
        legend.position = "bottom")

# Join density data with the spike data and remove rows with missing data
Spike <- Spike %>%
  left_join(PopD, by = "DeerYear") %>%
  na.omit()

nowt <- nowt %>%
  left_join(PopD, by = "DeerYear")

birth <- birth %>%
  left_join(PopD, by = "DeerYear")

birth_w_wt <- birth_w_wt %>%
  left_join(PopD, by = "DeerYear")

nowt7172 <- nowt7172 %>%
  left_join(PopD_new, by = "DeerYear")

# Filter data to include only years up to 1996 for replication of Schmidt paper
Spike96 <- Spike %>% filter(DeerYear <= 1996)
nowt96 <- nowt %>% filter(DeerYear <= 1996)
birth96 <- birth %>% filter(DeerYear <= 1996)
birth_w_wt96 <- birth_w_wt %>% filter(DeerYear <= 1996)
nowt717296 <- nowt7172 %>% filter(DeerYear <= 1996)

# Fit simple models to check the direction and effect of hind density on spike length
nowt7172Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = nowt7172)
nowt717296Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = nowt717296)

# Summarize the model results
summary(nowt7172Hinds_spike)
summary(nowt717296Hinds_spike)

# Calculate average spike length per year for predictions and graphs
AvgSpike <- Spike %>%
  group_by(DeerYear) %>%
  summarise(
    mean_spike = mean(AvgSpike),
    sample_size = n(),
    se_spike = sd(AvgSpike) / sqrt(n())
  ) %>%
  left_join(PopD, by = "DeerYear")

AvgSpike96 <- AvgSpike %>% filter(DeerYear <= 1996)

# Plot mean yearling antler length over the years with error bars
ggplot(data = AvgSpike, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  scale_x_continuous(breaks = seq(1973, 2022, by = 1)) +
  labs(x = "Year", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black", size = 0.5),
        panel.background = element_blank(),  # Remove background gridlines
        legend.position = "bottom") +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black")

# Plot mean yearling antler length over the years for the filtered data (1973-1996)
ggplot(data = AvgSpike96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling antler length for cohorts born 1973-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black")

# Calculate and plot the average spike length without birth weight over time
Avg_nowt <- nowt %>%
  group_by(DeerYear) %>%
  summarise(
    mean_spike = mean(AvgSpike),
    sample_size = n(),
    se_spike = sd(AvgSpike) / sqrt(n())
  ) %>%
  left_join(PopD, by = "DeerYear")

Avg_nowt96 <- Avg_nowt %>% filter(DeerYear <= 1996)

ggplot(data = Avg_nowt, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling antler length for cohorts born 1970-2021", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black")

ggplot(data = Avg_nowt96, aes(x = DeerYear, y = mean_spike)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  geom_errorbar(aes(ymin = mean_spike - se_spike, ymax = mean_spike + se_spike), width = 0.2, color = "black") +
  labs(title = "Mean cohort yearling antler length for cohorts born 1970-1996", x = "Year of Birth", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + se_spike + 0.5, label = paste0("(", sample_size, ")")), size = 3, color = "black")

# Plot mean yearling antler length over hind density with fitted linear model
ggplot(data = Avg_nowt, aes(x = Hinds, y = mean_spike)) +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  labs(title = "Mean cohort yearling antler length over Hind Density", x = "Hind Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black")

# Plot mean yearling antler length over adult density with fitted linear model
ggplot(data = Avg_nowt, aes(x = Adults, y = mean_spike)) +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  labs(title = "Mean cohort yearling antler length over Adult Density", x = "Adult Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black")

# Plot mean yearling antler length over total density with fitted linear model
ggplot(data = Avg_nowt, aes(x = Total, y = mean_spike)) +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  labs(title = "Mean cohort yearling antler length over Total Density", x = "Total Density", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black")

# Plot mean yearling antler length over livestock units with fitted linear model
ggplot(data = Avg_nowt, aes(x = LU_Total, y = mean_spike)) +
  geom_smooth(method = "lm", color = "black", size = 0.75) +
  geom_point(color = "black", size = 2, shape = 15) +
  labs(title = "Mean cohort yearling antler length over Livestock units", x = "Livestock units", y = "Yearling Mean Antler Length (inches)") +
  theme_minimal() +
  geom_text(aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2, color = "black")

# Fit a simple linear model of spike length over hind density
SpikeByYear <- glm(mean_spike ~ Hinds, data = AvgSpike)
summary(SpikeByYear)

# Fit the same model but only for years up to 1996
SpikeByYear96 <- glm(mean_spike ~ Hinds, data = AvgSpike96)
summary(SpikeByYear96)

# Fit GLMMs for spike length without birth weight, including year and mother as random effects
Spike_noden <- glmmTMB(AvgSpike ~ BirthWt + DeerYear + (1|DeerYear) + (1|MumCode), data = Spike)
summary(Spike_noden)

# Summarize the model in a table and plot the model results
tab_model(Spike_noden, digits = 4, show.stat = TRUE, show.se = TRUE)
plot_model(Spike_noden)

# Fit the model without DeerYear as a fixed effect
Spike_noyr_noden <- glmmTMB(AvgSpike ~ BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
summary(Spike_noyr_noden)

# Fit simple GLMMs with individual population density metrics as fixed effects
Hinds_spike_simple <- glmmTMB(AvgSpike ~ Hinds + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
Adults_spike_simple <- glmmTMB(AvgSpike ~ Adults + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
Total_spike_simple <- glmmTMB(AvgSpike ~ Total + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
LU_Total_spike_simple <- glmmTMB(AvgSpike ~ LU_Total + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)

# Summarize the results of the simple models
summary(Hinds_spike_simple)
summary(Adults_spike_simple)
summary(Total_spike_simple)
summary(LU_Total_spike_simple)

# Fit similar models for the data limited to years up to 1996
Hinds_spike_simple96 <- glmmTMB(AvgSpike ~ Hinds + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
Adults_spike_simple96 <- glmmTMB(AvgSpike ~ Adults + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
Total_spike_simple96 <- glmmTMB(AvgSpike ~ Total + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
LU_Total_spike_simple96 <- glmmTMB(AvgSpike ~ LU_Total + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)

# Summarize the models for the 1996 data
summary(Hinds_spike_simple96)
summary(Adults_spike_simple96)
summary(Total_spike_simple96)
summary(LU_Total_spike_simple96)

# Initial Models with Year and Weight
Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
Adults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
Total_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)
LU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike)

# Summarize the initial models
summary(Hinds_spike)
summary(Adults_spike)
summary(Total_spike)
summary(LU_Total_spike)

# Generate predictions using ggeffects for the models
AvgSpike_filtered <- AvgSpike %>% select(DeerYear, mean_spike, sample_size, se_spike)
Spike_pred <- Spike %>% left_join(AvgSpike_filtered, by = "DeerYear")

predictions_Hinds_yr <- ggpredict(Hinds_spike, terms = "Hinds [all]")
predictions_Hinds_yr$Hinds <- predictions_Hinds_yr$x

predictions_Hinds_noyr <- ggpredict(Hinds_spike_simple, terms = "Hinds [all]")
predictions_Hinds_noyr$Hinds <- predictions_Hinds_noyr$x

Spike_pred_Hinds_yr <- Spike_pred %>% left_join(predictions_Hinds_yr, by = "Hinds")
Spike_pred_Hinds_noyr <- Spike_pred %>% left_join(predictions_Hinds_noyr, by = "Hinds")

# Plot the predictions with and without year as a fixed effect
ggplot(Spike_pred_yr, aes(x = Hinds, y = mean_spike)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +  # Predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "lightblue") +  # Confidence interval
  labs(x = "Hinds", y = "Predicted Average Spike", title = "Predicted Average Spike with 95% Confidence Interval") +
  theme_minimal()

ggplot(Spike_pred_noyr, aes(x = Hinds, y = mean_spike)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red") +  # Predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "lightpink") +  # Confidence interval
  labs(x = "Hinds", y = "Predicted Average Spike", title = "Predicted Average Spike with 95% Confidence Interval") +
  theme_minimal()

# Combine and plot the predictions with and without year as a fixed effect
Spike_pred_Hinds_yr$Type <- "With Year as Fixed effect"
Spike_pred_Hinds_noyr$Type <- "Without Year as Fixed effect"
combined_spike_Hinds <- rbind(Spike_pred_Hinds_yr, Spike_pred_Hinds_noyr)
unique_spike_Hinds <- combined_spike_Hinds %>%
  distinct(Hinds, mean_spike, .keep_all = TRUE)

ggplot(combined_spike_Hinds, aes(x = Hinds, y = mean_spike)) +
  geom_point(data = unique_spike_Hinds) +
  geom_text(data = unique_spike_Hinds, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Hind Density", y = "Mean Yearling Antler Length (inches)", title = "Predicted Mean Yearling Antler Length over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# Predictions and visualization for adults
predictions_Adults_yr <- ggpredict(Adults_spike, terms = "Adults [all]")
predictions_Adults_yr$Adults <- predictions_Adults_yr$x

predictions_Adults_noyr <- ggpredict(Adults_spike_simple, terms = "Adults [all]")
predictions_Adults_noyr$Adults <- predictions_Adults_noyr$x

Spike_pred_Adults_yr <- Spike_pred %>% left_join(predictions_Adults_yr, by = "Adults")
Spike_pred_Adults_noyr <- Spike_pred %>% left_join(predictions_Adults_noyr, by = "Adults")

# Combine and plot predictions for adult population size with and without year as a fixed effect
Spike_pred_Adults_yr$Type <- "With Year as Fixed effect"
Spike_pred_Adults_noyr$Type <- "Without Year as Fixed effect"
combined_spike_Adults <- rbind(Spike_pred_Adults_yr, Spike_pred_Adults_noyr)
unique_spike_Adults <- combined_spike_Adults %>%
  distinct(Adults, mean_spike, .keep_all = TRUE)

Adults_Spike_terms <- paste("Adjusted for:", "Birth Weight = 6.99 kg", "Deer Year = 2001", sep = "\n")

ggplot(combined_spike_Adults, aes(x = Adults, y = mean_spike)) +
  geom_point(data = unique_spike_Adults) +
  geom_text(data = unique_spike_Adults, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Adults Population Size", y = "Mean Yearling Spike Length (inches)") +
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
  annotate("text", x = 185, y = 1.1, label = Adults_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# Predictions and visualization for total density
predictions_Total_yr <- ggpredict(Total_spike, terms = "Total [all]")
predictions_Total_yr$Total <- predictions_Total_yr$x

predictions_Total_noyr <- ggpredict(Total_spike_simple, terms = "Total [all]")
predictions_Total_noyr$Total <- predictions_Total_noyr$x

Spike_pred_Total_yr <- Spike_pred %>% left_join(predictions_Total_yr, by = "Total")
Spike_pred_Total_noyr <- Spike_pred %>% left_join(predictions_Total_noyr, by = "Total")

# Combine and plot predictions for total density with and without year as a fixed effect
Spike_pred_Total_yr$Type <- "With Year as Fixed effect"
Spike_pred_Total_noyr$Type <- "Without Year as Fixed effect"
combined_spike_Total <- rbind(Spike_pred_Total_yr, Spike_pred_Total_noyr)
unique_spike_Total <- combined_spike_Total %>%
  distinct(Total, mean_spike, .keep_all = TRUE)

ggplot(combined_spike_Total, aes(x = Total, y = mean_spike)) +
  geom_point(data = unique_spike_Total) +
  geom_text(data = unique_spike_Total, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Total Density", y = "Mean Yearling Antler Length (inches)", title = "Predicted Mean Yearling Antler Length over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# Predictions and visualization for LU_Total density
predictions_LU_Total_yr <- ggpredict(LU_Total_spike, terms = "LU_Total [all]")
predictions_LU_Total_yr$LU_Total <- predictions_LU_Total_yr$x

predictions_LU_Total_noyr <- ggpredict(LU_Total_spike_simple, terms = "LU_Total [all]")
predictions_LU_Total_noyr$LU_Total <- predictions_LU_Total_noyr$x

Spike_pred_LU_Total_yr <- Spike_pred %>% left_join(predictions_LU_Total_yr, by = "LU_Total")
Spike_pred_LU_Total_noyr <- Spike_pred %>% left_join(predictions_LU_Total_noyr, by = "LU_Total")

# Combine and plot predictions for LU_Total density with and without year as a fixed effect
Spike_pred_LU_Total_yr$Type <- "With Year as Fixed effect"
Spike_pred_LU_Total_noyr$Type <- "Without Year as Fixed effect"
combined_spike_LU_Total <- rbind(Spike_pred_LU_Total_yr, Spike_pred_LU_Total_noyr)
unique_spike_LU_Total <- combined_spike_LU_Total %>%
  distinct(LU_Total, mean_spike, .keep_all = TRUE)

ggplot(combined_spike_LU_Total, aes(x = LU_Total, y = mean_spike)) +
  geom_point(data = unique_spike_LU_Total) +
  geom_text(data = unique_spike_LU_Total, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "LU_Total Density", y = "Mean Yearling Antler Length (inches)", title = "Predicted Mean Yearling Antler Length over Density\nwith 95% Confidence Interval") +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Optionally remove the legend title

# Inspect the prediction results
print(predictions_Hinds_noyr)
print(predictions_Hinds_yr)
print(predictions_Adults_noyr)
print(predictions_Adults_yr)
print(predictions_Total_noyr)
print(predictions_Total_yr)
print(predictions_LU_Total_noyr)
print(predictions_LU_Total_yr)

# Fit models for the data limited to years up to 1996
Hinds_spike96 <- glmmTMB(AvgSpike ~ Hinds + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
Adults_spike96 <- glmmTMB(AvgSpike ~ Adults + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
Total_spike96 <- glmmTMB(AvgSpike ~ Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)
LU_Total_spike96 <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = Spike96)

# Summarize the models for the 1996 data
summary(Hinds_spike96)
summary(Adults_spike96)
summary(Total_spike96)
summary(LU_Total_spike96)

# Fit GLMMs for birth year with weight and without year as a fixed effect
birthHinds_spike <- glmmTMB(AvgSpike ~ Hinds + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
birthAdults_spike <- glmmTMB(AvgSpike ~ Adults + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
birthTotal_spike <- glmmTMB(AvgSpike ~ Total + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
birthLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)

# Summarize the models
summary(birthHinds_spike)
summary(birthAdults_spike)
summary(birthTotal_spike)
summary(birthLU_Total_spike)

# Fit similar models for the data limited to years up to 1996
birthHinds_spike96 <- glmmTMB(AvgSpike ~ Hinds + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
birthAdults_spike96 <- glmmTMB(AvgSpike ~ Adults + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
birthTotal_spike96 <- glmmTMB(AvgSpike ~ Total + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
birthLU_Total_spike96 <- glmmTMB(AvgSpike ~ LU_Total + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)

# Summarize the models for the 1996 data
summary(birthHinds_spike96)
summary(birthAdults_spike96)
summary(birthTotal_spike96)
summary(birthLU_Total_spike96)

# Fit GLMMs for birth year with weight and year as a fixed effect
yrbirthHinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
yrbirthAdults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
yrbirthTotal_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)
yrbirthLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt)

# Summarize the models
summary(yrbirthHinds_spike)
summary(yrbirthAdults_spike)
summary(yrbirthTotal_spike)
summary(yrbirthLU_Total_spike)

# Structure of the birth_w_wt data
str(birth_w_wt)

# Fit similar models for the data limited to years up to 1996
yrbirthHinds_spike96 <- glmmTMB(AvgSpike ~ Hinds + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
yrbirthAdults_spike96 <- glmmTMB(AvgSpike ~ Adults + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
yrbirthTotal_spike96 <- glmmTMB(AvgSpike ~ Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)
yrbirthLU_Total_spike96 <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + BirthWt + (1|DeerYear) + (1|MumCode), data = birth_w_wt96)

# Summarize the models for the 1996 data
summary(yrbirthHinds_spike96)
summary(yrbirthAdults_spike96)
summary(yrbirthTotal_spike96)
summary(yrbirthLU_Total_spike96)

# Fit GLMMs for spike length without birth weight but with DeerYear as a fixed effect
nowtHinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + (1|DeerYear), data = nowt)
nowtAdults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + (1|DeerYear), data = nowt)
nowtTotal_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + (1|DeerYear), data = nowt)
nowtLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + (1|DeerYear), data = nowt)

# Summarize the models
summary(nowtHinds_spike)
summary(nowtAdults_spike)
summary(nowtTotal_spike)
summary(nowtLU_Total_spike)

# Fit similar models for the data limited to years up to 1996
nowt96Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + (1|DeerYear), data = nowt96)
nowt96Adults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + (1|DeerYear), data = nowt96)
nowt96Total_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + (1|DeerYear), data = nowt96)
nowt96LU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + (1|DeerYear), data = nowt96)

# Summarize the models for the 1996 data
summary(nowt96Hinds_spike)
summary(nowt96Adults_spike)
summary(nowt96Total_spike)
summary(nowt96LU_Total_spike)

# Fit GLMMs for spike length using birth year without birth weight
nowtbirthHinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + (1|DeerYear), data = birth)
nowtbirthAdults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + (1|DeerYear), data = birth)
nowtbirthTotal_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + (1|DeerYear), data = birth)
nowtbirthLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + (1|DeerYear), data = birth)

# Summarize the models
summary(nowtbirthHinds_spike)
summary(nowtbirthAdults_spike)
summary(nowtbirthTotal_spike)
summary(nowtbirthLU_Total_spike)

# Fit similar models for the data limited to years up to 1996
nowtbirth96Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + DeerYear + (1|DeerYear), data = birth96)
nowtbirth96Adults_spike <- glmmTMB(AvgSpike ~ Adults + DeerYear + (1|DeerYear), data = birth96)
nowtbirth96Total_spike <- glmmTMB(AvgSpike ~ Total + DeerYear + (1|DeerYear), data = birth96)
nowtbirth96LU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + DeerYear + (1|DeerYear), data = birth96)

# Summarize the models for the 1996 data
summary(nowtbirth96Hinds_spike)
summary(nowtbirth96Adults_spike)
summary(nowtbirth96Total_spike)
summary(nowtbirth96LU_Total_spike)

# Fit GLMMs without additional year and weight as fixed effects
noyr_nowtHinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = nowt)
noyr_nowtAdults_spike <- glmmTMB(AvgSpike ~ Adults + (1|DeerYear), data = nowt)
noyr_nowtTotal_spike <- glmmTMB(AvgSpike ~ Total + (1|DeerYear), data = nowt)
noyr_nowtLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + (1|DeerYear), data = nowt)

# Summarize the models
summary(noyr_nowtHinds_spike)
summary(noyr_nowtAdults_spike)
summary(noyr_nowtTotal_spike)
summary(noyr_nowtLU_Total_spike)

# Fit similar models for the data limited to years up to 1996
noyr_nowt96Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = nowt96)
noyr_nowt96Adults_spike <- glmmTMB(AvgSpike ~ Adults + (1|DeerYear), data = nowt96)
noyr_nowt96Total_spike <- glmmTMB(AvgSpike ~ Total + (1|DeerYear), data = nowt96)
noyr_nowt96LU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + (1|DeerYear), data = nowt96)

# Summarize the models for the 1996 data
summary(noyr_nowt96Hinds_spike)
summary(noyr_nowt96Adults_spike)
summary(noyr_nowt96Total_spike)
summary(noyr_nowt96LU_Total_spike)

# Fit GLMMs for birth year without additional year and weight as fixed effects
noyr_birthHinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = birth)
noyr_birthAdults_spike <- glmmTMB(AvgSpike ~ Adults + (1|DeerYear), data = birth)
noyr_birthTotal_spike <- glmmTMB(AvgSpike ~ Total + (1|DeerYear), data = birth)
noyr_birthLU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + (1|DeerYear), data = birth)

# Summarize the models
summary(noyr_birthHinds_spike)
summary(noyr_birthAdults_spike)
summary(noyr_birthTotal_spike)
summary(noyr_birthLU_Total_spike)

# Fit similar models for the data limited to years up to 1996
noyr_birth96Hinds_spike <- glmmTMB(AvgSpike ~ Hinds + (1|DeerYear), data = birth96)
noyr_birth96Adults_spike <- glmmTMB(AvgSpike ~ Adults + (1|DeerYear), data = birth96)
noyr_birth96Total_spike <- glmmTMB(AvgSpike ~ Total + (1|DeerYear), data = birth96)
noyr_birth96LU_Total_spike <- glmmTMB(AvgSpike ~ LU_Total + (1|DeerYear), data = birth96)

# Summarize the models for the 1996 data
summary(noyr_birth96Hinds_spike)
summary(noyr_birth96Adults_spike)
summary(noyr_birth96Total_spike)
summary(noyr_birth96LU_Total_spike)



# Combine and plot predictions for adults with and without year as a fixed effect, using FWS
Spike_pred_Adults_yr$Type <- "With Year as Fixed effect"
Spike_pred_Adults_noyr$Type <- "Without Year as Fixed effect"
combined_spike_Adults <- rbind(Spike_pred_Adults_yr, Spike_pred_Adults_noyr)
unique_spike_Adults <- combined_spike_Adults %>%
  distinct(Adults, mean_spike, .keep_all = TRUE)

Adults_Spike_terms <- paste("Adjusted for:", "Birth Weight = 6.99 kg", "Deer Year = 2001", sep = "\n")

ggplot(combined_spike_Adults, aes(x = Adults, y = mean_spike)) +
  geom_point(data = unique_spike_Adults) +
  geom_text(data = unique_spike_Adults, aes(y = mean_spike + 0.1, label = paste0("(", DeerYear, ")")), size = 2.5, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(x = "Adults Population Size", y = "Mean Yearling Spike Length (inches)") +
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
  annotate("text", x = 185, y = 1.1, label = Adults_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1)

# DeerYear as fixed effect: with and without year data
predictions_spike_noden <- ggpredict(Spike_noden, terms = "DeerYear [all]")
predictions_spike_noden$DeerYear <- predictions_spike_noden$x

predictions_spike_noyr_noden <- ggpredict(Spike_noyr_noden, terms = "DeerYear [all]")
predictions_spike_noyr_noden$DeerYear <- predictions_spike_noyr_noden$x

# Add a Type column to differentiate the data sets
predictions_spike_noden$Type <- "With Year as Fixed effect"
predictions_spike_noyr_noden$Type <- "Without Year as Fixed effect"

# Join predictions with the original spike data
predictions_spike_noden <- Spike_pred %>% left_join(predictions_spike_noden, by = "DeerYear")
predictions_spike_noyr_noden <- Spike_pred %>% left_join(predictions_spike_noyr_noden, by = "DeerYear")

# Combine and plot the predictions for DeerYear as a fixed effect
combined_spike_noden <- rbind(predictions_spike_noden, predictions_spike_noyr_noden)
unique_spike_noden <- combined_spike_noden %>%
  distinct(DeerYear, mean_spike, .keep_all = TRUE)

noden_Spike_terms <- paste("Adjusted for:", "Birth Weight = 6.99 kg", sep = "\n")

plot_model(Spike_noden, type = "pred")

ggplot(combined_spike_noden, aes(x = DeerYear, y = mean_spike)) +
  geom_point(data = unique_spike_noden) +
  geom_errorbar(data = unique_spike_noden, aes(ymin = mean_spike - std.error, ymax = mean_spike + std.error), width = 0.2) +  # Add error bars
  geom_text(data = unique_spike_noden,aes(y = mean_spike + std.error + 0.1, label = paste0("(", sample_size, ")")), size = 3, color = "black") +
  geom_line(aes(y = predicted, color = Type)) +  # Predicted values with different colors
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Type), alpha = 0.2) +  # Confidence intervals with different fills
  labs(
    x = "Deer Year",
    y = "Mean Yearling Spike Length (inches)"  ) +
  scale_color_manual(values = c("With Year as Fixed effect" = "blue", "Without Year as Fixed effect" = "red")) +  # Customize line colors
  scale_fill_manual(values = c("With Year as Fixed effect" = "lightblue", "Without Year as Fixed effect" = "lightpink")) +  # Customize ribbon fills
  theme_minimal() +
  theme( axis.title.x = element_text(size = 15),  
         axis.title.y = element_text(size = 15),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.text = element_text(size = 10),
         axis.line = element_line(color = "black", size = 0.5),
         panel.background = element_blank(),  # Remove background gridlines
         legend.position = "bottom",  # Position the legend at the bottom
         legend.title = element_blank())+ # Optionally remove the legend title
  annotate("text", x = 1972, y = 4.2, label = noden_Spike_terms, size = 4, color = "#8A7D23", hjust = 0, vjust = 1) 
              

# Compare best model using ANOVA to determine significance of fixed effects
anova(Spike_noyr_noden, Spike_noden)

# Compare nested models using ANOVA to determine significance of random effects
Spike_noden_no_yrrandom <- glmmTMB(AvgSpike ~ BirthWt + DeerYear +(1|MumCode), data = Spike)
Spike_noden_no_mumrandom <- glmmTMB(AvgSpike ~ BirthWt + DeerYear + (1|DeerYear), data = Spike)

anova(Spike_noden,Spike_noden_no_yrrandom)
anova(Spike_noden,Spike_noden_no_mumrandom)
