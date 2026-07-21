# ==========================================
# MODULE 4: EVALUATION AND ANALYSIS
# ==========================================
library(tidyverse)
library(knitr)
library(reshape2)

evaluation_metrics <- df_final_simulation %>%
  group_by(threat_type, transmission_medium) %>%
  summarize(
    total_incidents = n(),
    breach_count = sum(breached),
    breach_probability = mean(breached),
    avg_speed = mean(speed),
    avg_impact = mean(impact_magnitude),
    .groups = 'drop'
  )

# ------------------------------------------
# VISUALIZATIONS GENERATED NATIVELY
# ------------------------------------------
# Plot 1: Attack Vector Vulnerability Breach Heatmap Matrix
p7 <- ggplot(evaluation_metrics, aes(x = transmission_medium, y = threat_type, fill = breach_probability)) +
  geom_tile(color = "gray") +
  scale_fill_gradient(low = "green", high = "red") +
  geom_text(aes(label = round(breach_probability, 2)), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Defensive Vulnerability Vector Matrix (Breach Ratios)", x = "Transmission Medium", y = "Threat Vector", fill = "Vulnerability index")
print(p7)

# Plot 2: Metric Trade-offs Across Actor Groups
melted_eval <- melt(evaluation_metrics, id.vars = c("threat_type", "transmission_medium"), measure.vars = c("avg_speed", "avg_impact"))
p8 <- ggplot(melted_eval, aes(x = transmission_medium, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~threat_type, ncol = 1) +
  theme_minimal() +
  labs(title = "Speed vs Impact Comparison Across Media Vectors", x = "Transmission Channels", y = "Normalized Scale Matrix", fill = "Metric")
print(p8)

# ------------------------------------------
# IN-CODE TABLES DISPLAY
# ------------------------------------------
cat("\n=== TABLE 4.2: MODULE 4 SUMMARY ===\n")
mod4_summary <- data.frame(
  Objective = "Quantify defensive posture vulnerability limits",
  Analysis_Focus = "Ransomware vs Malicious Privilege Escalation Profiling",
  Key_Metric_Evaluated = "Dynamic Breach Distribution Ratios",
  Global_System_Breach_Score = round(mean(df_final_simulation$breached), 4)
)
print(kable(mod4_summary, format = "simple"))

cat("\n=== TABLE 4.3: AGGREGATED ASSESSMENT SUMMARY TABLE ===\n")
print(kable(evaluation_metrics, format = "simple"))

cat("\n=== TABLE 4.4: STATISTICAL SIGNIFICANCE (PROPORTION TEST) ===\n")
prop_table <- table(df_final_simulation$threat_type == "External: Ransomware & Zero-Day Injection", df_final_simulation$breached)
prop_test_eval <- prop.test(prop_table)
stat_table_4 <- data.frame(
  Hypothesis_Evaluated = "Ransomware variants yield significantly higher breach execution velocity",
  Test_Type = "Two-sample Proportion Chi2 Test",
  Chi2_Score = prop_test_eval$statistic,
  p_value = prop_test_eval$p.value
)
print(kable(stat_table_4, format = "simple"))