# ==========================================
# MODULE 5: VISUALIZATION
# ==========================================
library(ggplot2)
library(knitr)

# Plot 1: Overall System Security Boundary Limitation Frontier
p9 <- ggplot(df_final_simulation, aes(x = boundary_limit, y = steps_to_stop, color = factor(breached))) +
  geom_jitter(alpha = 0.4) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Contained Safely", "Perimeter Breached")) +
  labs(title = "System Security Boundary Limitations vs Traversal Retainability", x = "Node Boundary Limit Threshold", y = "Steps to Process End Terminus", color = "System Status Outcome")
print(p9)

# Plot 2: Composite Energy-Power Density Inflection Points
p10 <- ggplot(df_final_simulation, aes(x = norm_energy, y = norm_power, color = risk_tier)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~breached, labeller = labeller(breached = c("0" = "Contained Safe Path", "1" = "Breached Fatal Path"))) +
  theme_dark() +
  labs(title = "Energy-Power Inflection Frontiers Across Threat Boundaries", x = "Normalized Kinetic Energy", y = "Normalized Force Power")
print(p10)

# ------------------------------------------
# IN-CODE TABLES DISPLAY
# ------------------------------------------
cat("\n=== TABLE 5.2: MODULE 5 SUMMARY ===\n")
mod5_summary <- data.frame(
  Objective = "Deliver visual analytics on simulation runs",
  Plot_Types_Generated = "Non-linear regression plots, Side-by-Side Factor counts",
  Input_Data_Scope = "Full processed simulation matrix",
  Target_Highlight = "Boundary limits and target state distribution peaks"
)
print(kable(mod5_summary, format = "simple"))

cat("\n=== TABLE 5.3: VISUAL ASSETS GENERATED MAP ===\n")
visual_map <- data.frame(
  Figure_Name = c("GMM Density", "GAM Spline", "Corr Heatmap", "Risk Boxplot", "Violin States", "State Heatmap", "Vector Matrix", "Speed/Impact Facets", "Boundary Frontier", "Energy-Power Split"),
  Plot_Methodology = c("Density Splines", "GAM Scatter Smoothing", "Correlation Matrix", "Boxplot Quantiles", "Violin Density Metrics", "Viridis Raster Grid", "Text Overlaid Heat Tiles", "Faceted Bar Ranges", "Jitter Plot Scatter", "Dark-themed Facet Scatter Grid"),
  Threat_Representation = "Explicitly tracking Ransomware (Ext) and Privilege Escalation (Int) dynamic properties"
)
print(kable(visual_map, format = "simple"))

cat("\n=== TABLE 5.4: STATISTICAL RUN VALIDATION (K-S DISTRIBUTION CHECK) ===\n")
speed_breached <- df_final_simulation$speed[df_final_simulation$breached == 1]
speed_contained <- df_final_simulation$speed[df_final_simulation$breached == 0]
ks_result <- ks.test(speed_breached, speed_contained)

stat_table_5 <- data.frame(
  Evaluation_Targets = "Propagation Speed Distribution Comparison",
  Test_Framework = "Two-sample Kolmogorov-Smirnov Test",
  D_Statistic = ks_result$statistic,
  p_value = ks_result$p.value
)
print(kable(stat_table_5, format = "simple"))