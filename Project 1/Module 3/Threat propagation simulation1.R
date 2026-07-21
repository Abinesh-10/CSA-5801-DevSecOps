# ==========================================
# MODULE 3: THREAT PROPAGATION SIMULATION
# ==========================================
library(tidyverse)
library(knitr)
library(reshape2)

set.seed(2026)

# Discrete Event Simulation Engine utilizing Markovian Random Steps
run_propagation_des <- function(threat_record, max_steps = 10) {
  speed_factor <- threat_record$speed
  strength_factor <- threat_record$attack_strength
  p_move <- 0.4 + (0.3 * speed_factor)
  p_breach <- 0.1 + (0.4 * strength_factor)
  
  state <- 1 
  steps_taken <- 0
  
  for(t in 1:max_steps) {
    if (state == 4) break 
    mutation_shift <- runif(1, 0, threat_record$mutation_rate)
    
    if (state == 1) {
      probs <- c(1 - p_move, p_move, 0, 0)
    } else if (state == 2) {
      probs <- c(0.1, 0.5 - mutation_shift, 0.4 + mutation_shift, 0)
    } else if (state == 3) {
      probs <- c(0.05, 0.05, 1 - p_breach, p_breach)
    }
    state <- sample(1:4, 1, prob = probs)
    steps_taken <- steps_taken + 1
  }
  
  return(data.frame(
    threat_id = threat_record$threat_id,
    final_state = state,
    steps_to_stop = steps_taken,
    breached = if_else(state == 4, 1, 0)
  ))
}

sim_results_list <- lapply(1:nrow(df_processed), function(i) run_propagation_des(df_processed[i, ]))
df_sim_output <- bind_rows(sim_results_list)
df_final_simulation <- inner_join(df_processed, df_sim_output, by = "threat_id")

# ------------------------------------------
# VISUALIZATIONS GENERATED NATIVELY
# ------------------------------------------
# Plot 1: Markov Transition State Convergence Across Vectors
p5 <- ggplot(df_final_simulation, aes(x = factor(final_state), y = steps_to_stop, fill = threat_type)) +
  geom_violin(draw_quantiles = 0.5, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Simulation Runtime Steps by Absorbing Final State", x = "Final Steady State", y = "Discrete Time Steps Taken", fill = "Threat Vectors") +
  theme(legend.position = "bottom", legend.direction = "vertical")
print(p5)

# Plot 2: Micro-Level Threat Traversal Heatmap
sample_matrix <- table(df_final_simulation$risk_tier, df_final_simulation$final_state)
melted_sample <- melt(sample_matrix)
p6 <- ggplot(melted_sample, aes(x = factor(Var2), y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Threat Tier vs. Convergence State Heatmap Matrix", x = "Markov End State", y = "Initial Risk Tier", fill = "Simulation Count")
print(p6)

# ------------------------------------------
# IN-CODE TABLES DISPLAY
# ------------------------------------------
cat("\n=== TABLE 3.2: MODULE 3 SUMMARY ===\n")
mod3_summary <- data.frame(
  Objective = "Model structural network traversal risks",
  Methods_Used = "Discrete Event Simulation + Dynamic Markov State Shifts",
  Parameters = "Variable Mutation Rates, Traversal speed scales",
  Scope_Bound = paste(nrow(df_final_simulation), "active simulation paths evaluated")
)
print(kable(mod3_summary, format = "simple"))

cat("\n=== TABLE 3.3: FINAL SIMULATION STATE METRICS ===\n")
state_metrics <- df_final_simulation %>%
  group_by(final_state) %>%
  summarize(
    Run_Count = n(),
    Average_Steps_Taken = mean(steps_to_stop),
    Breach_Success_Rate = mean(breached),
    .groups = 'drop'
  )
print(kable(state_metrics, format = "simple"))

cat("\n=== TABLE 3.4: STATISTICAL ANALYSIS (PROPAGATION SPEED COMPARISON) ===\n")
# Comparing target External Ransomware vs target Internal Privilege Escalation
t_test_sim <- t.test(steps_to_stop ~ is_external, data = df_final_simulation %>% filter(breached == 1))
stat_table_3 <- data.frame(
  Metric_Under_Review = "Steps to Breach (Ransomware vs Escalation Attacks)",
  Test_Type = "Welch Two Sample t-test",
  t_Statistic = t_test_sim$statistic,
  df = t_test_sim$parameter,
  p_value = t_test_sim$p.value
)
print(kable(stat_table_3, format = "simple"))