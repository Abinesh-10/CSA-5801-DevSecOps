# ==========================================
# MODULE 2: PREPROCESSING AND MODELING
# ==========================================
library(mgcv)
library(tidyverse)
library(knitr)
library(reshape2)

# 1. Feature Transformation Pipeline
df_processed <- df_base %>%
  mutate(
    norm_energy = (attack_energy - min(attack_energy)) / (max(attack_energy) - min(attack_energy)),
    norm_power = (attack_power - min(attack_power)) / (max(attack_power) - min(attack_power)),
    is_network = if_else(transmission_medium == "Network", 1, 0),
    is_external = if_else(threat_type == "External: Ransomware & Zero-Day Injection", 1, 0)
  )

# 2. Mathematical Predictive Modeling (Risk Target Derivation)
risk_model <- gam(impact_magnitude ~ threat_type + s(speed) + s(attack_strength) + is_network, data = df_processed)
df_processed$predicted_impact <- predict(risk_model, newdata = df_processed)
df_processed$risk_tier <- cut(df_processed$predicted_impact, breaks = c(-Inf, 0.35, 0.65, Inf), labels = c("Low", "Medium", "High"))

# ------------------------------------------
# VISUALIZATIONS GENERATED NATIVELY
# ------------------------------------------
# Plot 1: Feature Matrix Correlation Heatmap
numeric_cols <- df_processed %>% select(attack_strength, speed, impact_magnitude, norm_energy, norm_power, mutation_rate)
corr_matrix <- melt(cor(numeric_cols))
p3 <- ggplot(corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
  theme_minimal() +
  labs(title = "Preprocessed Parameters Correlation Heatmap", x = "", y = "", fill = "Pearson R")
print(p3)

# Plot 2: Target Prediction Tier Split Boxplot
p4 <- ggplot(df_processed, aes(x = risk_tier, y = predicted_impact, fill = risk_tier)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Engineered Risk Tier Target Categorization Profiles", x = "Derived Risk Level", y = "Predicted Impact Magnitude")
print(p4)

# ------------------------------------------
# IN-CODE TABLES DISPLAY
# ------------------------------------------
cat("\n=== TABLE 2.2: MODULE 2 SUMMARY ===\n")
mod2_summary <- data.frame(
  Objective = "Normalize physical dimensions & model risk tiers",
  Methods_Used = "Min-Max scaling, GAM Regression modeling",
  Threat_Scope = "Segmented across Specific Ransomware (Ext) and Privilege Escalation (Int) baselines",
  Target_Field = "risk_tier (Low, Medium, High)"
)
print(kable(mod2_summary, format = "simple"))

cat("\n=== TABLE 2.3: FINAL MODEL RISK TIER DISTRIBUTION ===\n")
tier_dist <- df_processed %>%
  group_by(risk_tier) %>%
  summarize(
    Sample_Count = n(),
    Mean_Predicted_Impact = mean(predicted_impact),
    Max_Observed_Power = max(norm_power),
    .groups = 'drop'
  )
print(kable(tier_dist, format = "simple"))

cat("\n=== TABLE 2.4: STATISTICAL INDEPENDENCE ANALYSIS ===\n")
chi_test_preprocessing <- chisq.test(table(df_processed$transmission_medium, df_processed$risk_tier))
stat_table_2 <- data.frame(
  Comparison_Features = "transmission_medium vs risk_tier",
  Test_Type = "Pearson's Chi-Square Test",
  Chi2_Statistic = chi_test_preprocessing$statistic,
  df = chi_test_preprocessing$parameter,
  p_value = chi_test_preprocessing$p.value
)
print(kable(stat_table_2, format = "simple"))