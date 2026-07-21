# ==========================================
# MODULE 1: SYNTHETIC DATASET GENERATION
# ==========================================
if(!require(mclust)) install.packages("mclust")
if(!require(mgcv)) install.packages("mgcv")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(knitr)) install.packages("knitr")

library(mclust)
library(mgcv)
library(tidyverse)
library(knitr)

set.seed(2026)
n_records <- 1050 

# 1. GMM Sampling for Multimodal Structure
# 1 = External (Ransomware & Zero-Day Injection)
# 2 = Internal (Privilege Escalation & Malicious Data Exfiltration)
# 3 = Credential Abuse / Phishing
cluster_assignment <- sample(1:3, n_records, replace = TRUE, prob = c(0.5, 0.2, 0.3))
attack_strength <- numeric(n_records)
attack_strength[cluster_assignment == 1] <- rnorm(sum(cluster_assignment == 1), mean = 0.8, sd = 0.1)
attack_strength[cluster_assignment == 2] <- rnorm(sum(cluster_assignment == 2), mean = 0.4, sd = 0.15)
attack_strength[cluster_assignment == 3] <- rnorm(sum(cluster_assignment == 3), mean = 0.6, sd = 0.1)
attack_strength <- pmin(pmax(attack_strength, 0), 1) 

df_base <- data.frame(
  threat_id = 1:n_records,
  threat_type = factor(cluster_assignment, labels = c(
    "External: Ransomware & Zero-Day Injection", 
    "Internal: Privilege Escalation & Malicious Exfiltration", 
    "Credential Abuse"
  )),
  attack_strength = attack_strength
)

# 2. GAM Injecting Non-linear Propagation Rules
df_base$speed <- with(df_base, 0.2 + 0.6 * sin(attack_strength * pi / 2) + rnorm(n_records, 0, 0.05))
df_base$speed <- pmin(pmax(df_base$speed, 0), 1)

df_base$impact_magnitude <- with(df_base, 0.1 + 0.8 * (attack_strength^2) + rnorm(n_records, 0, 0.08))
df_base$impact_magnitude <- pmin(pmax(df_base$impact_magnitude, 0), 1)

# Factor Variables Formulation
df_base$direction <- sample(c("Northbound", "Southbound", "East-West"), n_records, replace = TRUE, prob = c(0.2, 0.2, 0.6))
df_base$mutation_rate <- runif(n_records, 0.0, 0.5)
df_base$attack_energy <- df_base$attack_strength * 100 + rnorm(n_records, 0, 5)
df_base$attack_power <- (df_base$attack_energy * df_base$speed) / 2
df_base$transmission_medium <- sample(c("Network", "Endpoint", "Cloud", "USB"), n_records, replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1))
df_base$boundary_limit <- runif(n_records, 10, 100) 
df_base$pattern_complexity <- runif(n_records, 0.1, 0.9)

# ------------------------------------------
# VISUALIZATIONS GENERATED NATIVELY
# ------------------------------------------
# Plot 1: GMM Density Distinguishable Signature Modes
p1 <- ggplot(df_base, aes(x = attack_strength, fill = threat_type)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "GMM Thread Archetype Vector Profiles", x = "Attack Strength", y = "Density", fill = "Threat Identity") +
  theme(legend.position = "bottom", legend.direction = "vertical")
print(p1)

# Plot 2: Non-linear GAM Spline Profile Verification
p2 <- ggplot(df_base, aes(x = attack_strength, y = speed, color = threat_type)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "black") +
  theme_minimal() +
  labs(title = "GAM Characterization: Strength vs. Propagation Speed", x = "Attack Strength", y = "Speed", color = "Threat Identity") +
  theme(legend.position = "bottom", legend.direction = "vertical")
print(p2)

# ------------------------------------------
# IN-CODE TABLES DISPLAY
# ------------------------------------------
cat("\n=== TABLE 1.2: MODULE 1 SUMMARY ===\n")
mod1_summary <- data.frame(
  Objective = "Generate high-fidelity synthetic security threats",
  Methods_Used = "GMM (Clustering), GAM (Non-linear mapping)",
  Record_Count = n_records,
  Threats_Modeled = "External (Ransomware & Zero-Day Injection) & Internal (Privilege Escalation & Malicious Exfiltration)"
)
print(kable(mod1_summary, format = "simple"))

cat("\n=== TABLE 1.3: FINAL DATASET PREVIEW (FIRST 5 ROWS) ===\n")
print(kable(head(df_base[, c("threat_id", "threat_type", "attack_strength", "speed", "impact_magnitude", "direction", "transmission_medium")], 5), format = "simple"))

cat("\n=== TABLE 1.4: STATISTICAL ANALYSIS (GAM EVALUATION) ===\n")
gam_check_speed <- gam(speed ~ threat_type + s(attack_strength), data = df_base)
summary_gam <- summary(gam_check_speed)

stat_table_1 <- data.frame(
  Factor_Vector = c(rownames(summary_gam$p.table)[2:3], "s(attack_strength)"),
  Test_Type = c("Parametric Wald", "Parametric Wald", "Non-parametric Smooth Spline"),
  Statistic = c(summary_gam$p.table[2, 3], summary_gam$p.table[3, 3], summary_gam$s.table[1, 3]),
  p_value = c(summary_gam$p.table[2, 4], summary_gam$p.table[3, 4], summary_gam$s.table[1, 4])
)
print(kable(stat_table_1, format = "simple"))
