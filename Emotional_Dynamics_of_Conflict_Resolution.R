
# (1)Load necessary libraries
library(dplyr)       
library(readxl)
library(readr)
library(broom)       
library(ggplot2)      
library(tidyr)
library(car)
library(multcomp)
library(stats)
library(stringr)
library(syuzhet)
library(knitr)
library(kableExtra)
library(forcats)
library(patchwork)
library(cowplot)
library(purrr)

# (2)Load data
file_path <- 'C:/Users/Louanna/Documents/GitHub/LanPan_program_example/Data - Fall 2023.xlsx'
data <- read_excel(file_path, sheet = "Data")
# Initial data cleanup
cleaned_data <- filter(data, passedattn != 'no')
# Summary statistics
summary_statistics <- summary(cleaned_data)
print(summary_statistics)
# Reshaping the data
melted_data <- gather(cleaned_data, feelings_scenario, emotional_response, feelings_youalone, feelings_youaloneforgiven, feelings_bothyoufirst, feelings_themalone, feelings_boththemfirst, feelings_neither, factor_key=TRUE)
# Create a mapping for scenarios
scenario_mapping <- c(
  feelings_youalone = "You Apology Alone",
  feelings_bothyoufirst = "You Received Return Apology",
  feelings_youaloneforgiven = "Forgiveness Without Apology",
  feelings_themalone = "Other Apology Alone",
  feelings_boththemfirst = "Other Received Return Apology",
  feelings_neither = "Neither Apology"
)
# Apply the mapping to the melted data
melted_data$feelings_scenario <- factor(melted_data$feelings_scenario, levels = names(scenario_mapping), labels = scenario_mapping)


# (3)Task 1 q1:Paired t-test
sex_count <- table(cleaned_data$sex)
q1_t_test <- t.test(cleaned_data$feelings_youalone, cleaned_data$feelings_bothyoufirst, paired = TRUE)
cat("q1-t-statistic:", q1_t_test$statistic, "\n")
cat("q1-p-value:", q1_t_test$p.value, "\n")
#Boxplot
filtered_data <- melted_data %>%
  filter(feelings_scenario %in% c("You Apology Alone", "You Received Return Apology"))
ggplot(filtered_data, aes(x = feelings_scenario, y = emotional_response, fill = feelings_scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c("You Apology Alone" = "#ADD8E6", "You Received Return Apology" = "#90EE90")) +
  labs(title = 'Comparative Analysis of Emotional Responses', 
       x = 'Feeling scenario', 
       y = 'Emotional Response (-30 to 30)') +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) + 
  theme_minimal() +  # White background with minimal elements
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

# (4)Task 1 q2:Paired t-test
# Calculating grouped descriptive statistics
grouped_des <- filtered_data %>%
  group_by(initiator_type) %>%
  summarise(mean = mean(emotional_response), std = sd(emotional_response))
# Define the model for ANOVA analysis
model <- aov(emotional_response ~ feelings_scenario * initiator_type, data=filtered_data)
# Perform ANOVA analysis
anova_results <- Anova(model, type="II")
print(anova_results)
# Tukey HSD Post Hoc Test for initiator_type
tukey_1 <- TukeyHSD(aov(emotional_response ~ initiator_type, data=filtered_data))
print(tukey_1)
# Tukey HSD Post Hoc Test for scenario
tukey_2 <- TukeyHSD(aov(emotional_response ~ feelings_scenario, data=filtered_data))
print(tukey_2)
# Create a boxplot 
ggplot(filtered_data, aes(x=initiator_type, y=emotional_response, fill=feelings_scenario)) +
  geom_boxplot() +
  scale_fill_manual(values=c("You Apology Alone" = "#ADD8E6", "You Received Return Apology" = "#90EE90")) +
  theme_minimal() +
  labs(title='Emotional Responses Across Scenarios and Initiator Types',
       x='Initiator Type',
       y='Emotional Response') +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) + 
  theme(
    legend.position = c(0.98, 0.98),  # Position the legend inside the plot at the top right
    legend.justification = c("right", "top"),  # Anchor the legend at its top right corner
    legend.box.just = c("right", "top"),  # Justify the legend box
    legend.background = element_rect(fill = NA, linetype = "solid", colour = "lightblue"),
    legend.margin = margin(3, 3, 3, 3),  # Adjust the margin around the legend
    plot.title = element_text(hjust = 0.5, size = 10),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  scale_x_discrete(limits = c("always", "conditional", "never"))

# (5)Task 1 q3:Paired t-test
q3_t_test <- t.test(cleaned_data$feelings_youaloneforgiven, cleaned_data$feelings_bothyoufirst, paired = TRUE)
cat("q3-t-statistic:", q3_t_test$statistic, "\n")
cat("q3-p-value:", q3_t_test$p.value, "\n")
# Create separate boxplots for each scenario and compare them across the different initiator types
filtered_data_2 <- melted_data %>%
  filter(feelings_scenario %in% c("You Apology Alone", "Forgiveness Without Apology", "You Received Return Apology"))
ggplot(filtered_data_2, aes(x = initiator_type, y = emotional_response, fill = feelings_scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "You Apology Alone" = "#ADD8E6",
    "Forgiveness Without Apology" = "#FADAA5",
    "You Received Return Apology" = "#90EE90"
  )) +
  theme_minimal() +
  labs(title = 'Comparative Analysis of Emotional Responses by Initiator Type',
       y = 'Emotional Response (-30 to 30)',
       x = 'Initiator Type',
       fill = 'Feelings Scenario') +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) + 
  theme(legend.position = c(0.98, 0.98),  
        legend.justification = c("right", "top"),  
        legend.box.just = c("right", "top"), 
        legend.background = element_rect(fill = NA, linetype = "solid", colour = "lightblue"),
        legend.margin = margin(3, 3, 3, 3), 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  scale_x_discrete(limits = c("always", "conditional", "never"))


# (6) Task 2a
scenario_stats <- melted_data %>%
  group_by(feelings_scenario) %>%
  summarise(mean = mean(emotional_response, na.rm = TRUE),
            sem = sd(emotional_response, na.rm = TRUE) / sqrt(n()))
ggplot(scenario_stats, aes(x = reorder(feelings_scenario, -mean), y = mean, fill = feelings_scenario)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c(
    "You Apology Alone" = "#ADD8E6",           
    "Forgiveness Without Apology" = "#FADAA5", 
    "You Received Return Apology" = "#90EE90", 
    "Other Apology Alone" = "#DDA0DD",         
    "Other Received Return Apology" = "#FFB6C1", 
    "Neither Apology" = "#FFFFE0"              
  )) +
  labs(title = 'Average Feelings for Six Scenarios',
       x = 'Scenarios',
       y = 'Average Feelings (-30 to 30)') +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=30, hjust = 1),
        legend.position = "none",  
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

# (7)Task 2b
#（ANOVA）
anova_result <- aov(emotional_response ~ feelings_scenario, data = melted_data)
summary(anova_result)

#Pairwise t-tests with Bonferroni Correction
pairwise_results <- list()
scenarios_to_compare <- setdiff(unique(melted_data$feelings_scenario), "You Apology Alone")

for (scenario in scenarios_to_compare) {
  comparison_data <- subset(melted_data, feelings_scenario %in% c("You Apology Alone", scenario))
  test_result <- t.test(emotional_response ~ feelings_scenario, data = comparison_data)
  pairwise_results[[scenario]] <- test_result
}

p_values <- sapply(pairwise_results, function(x) x$p.value)
p_adjusted <- p.adjust(p_values, method = "bonferroni")

results_df <- data.frame(
  Scenario_Comparison = names(p_adjusted),
  T_Value = sapply(pairwise_results, function(x) x$statistic),
  Degrees_of_Freedom = sapply(pairwise_results, function(x) x$parameter),
  Original_P_Value = p_values,
  Adjusted_P_Value = p_adjusted,
  Significance = p_adjusted < 0.05
)

kable(results_df, caption = "Pairwise T-Test Results with Bonferroni Correction", align = "c",
      format = "html", row.names = FALSE, digits = 3)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# (8)Task 2c
binary_outcomes_mapping <- c(
  "^I apologize first, then .+ apologizes\\.$" = "You Received Return Apology", 
  "^Neither I nor .+ apologizes\\.$" = "Neither Apology"
)
cleaned_data <- cleaned_data %>%
  mutate(outcome_binary1 = str_replace_all(outcome_binary1, binary_outcomes_mapping))
outcome_counts <- cleaned_data %>%
  group_by(outcome_binary1) %>%
  summarise(Count = n()) %>%
  ungroup()

# Calculate proportions
total_responses <- sum(outcome_counts$Count)
outcome_counts <- outcome_counts %>%
  mutate(Proportion = Count / total_responses)

# Create a bar graph of the proportions
p1 <- ggplot(outcome_counts, aes(x = outcome_binary1, y = Proportion, fill = outcome_binary1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "You Received Return Apology" = "#90EE90",
    "Neither Apology" = "#FFFFE0" 
  )) +
  labs(title = "Proportion of Choices\nfor outcome_binary1",
       x = "Option",
       y = "Proportion") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) + 
  ylim(0,1)+
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_x_discrete(labels = function(x) gsub("Received", "Received\n", x))

# Conduct a Chi-square test of independence
chisq_test_result <- chisq.test(outcome_counts$Count)

# Output the result of the chi-square test
chisq_test_result

# The difference with initiator_type
outcome_by_initiator <- cleaned_data %>%
  group_by(initiator_type, outcome_binary1) %>%
  summarise(Count = n()) %>%
  ungroup()

outcome_by_initiator <- outcome_by_initiator %>%
  group_by(initiator_type) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Proportion = Count / Total)

p2 <- ggplot(outcome_by_initiator, aes(x = initiator_type, y = Proportion, fill = outcome_binary1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "You Received Return Apology" = "#90EE90",
    "Neither Apology" = "#FFFFE0"
  )) +
  labs(title = "Proportion of Choices\nby Initiator Type",
       x = "Initiator Type",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

chisq_test_result <- chisq.test(xtabs(~ initiator_type + outcome_binary1, data = cleaned_data))

print(chisq_test_result)

p <- (p1 | p2) + 
  plot_layout(widths = c(2,3),guides = 'collect') &
  theme(legend.position = "bottom")
p


# (9) Task 2d
# I used VADER and gpt-3.5-turbo to conduct sentiment analysis on describe in python. 
# I inserted the obtained data here and analyzed it.
file_path_2 <- 'C:/Users/Louanna/Documents/GitHub/LanPan_program_example/updated_cleaned_data.csv'
updated_cleaned_data <- read_csv(file_path_2)

ggplot(updated_cleaned_data, aes(x = sentiment_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", color = "black") +
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = -0.05, color = "Negative Threshold"), linetype = "dashed") +
  geom_vline(aes(xintercept = 0.05, color = "Positive Threshold"), linetype = "dashed") +
  labs(
    title = 'Distribution of Sentiment Scores(VADER)',
    x = 'Sentiment Score',
    y = 'Density',
    color = "Threshold"
  ) +
  scale_color_manual(
    values = c("Negative Threshold" = "red", "Positive Threshold" = "green"),
    labels = c("Negative Threshold x = -0.05", "Positive Threshold x = 0.05") # Set custom labels for the legend
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = c(0.02, 0.98), 
        legend.justification = c("left", "top"),  
        legend.box.just = c("left", "top"),  
        legend.background = element_rect(fill = NA, linetype = "solid", colour = "lightblue"),
        legend.margin = margin(3, 3, 3, 3), 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 


ggplot(updated_cleaned_data, aes(x = fct_relevel(initiator_type, "always", "conditional", "never"),
                                 y = Emotion_Score, fill = Labels)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Negative" = "#90EE90",
    "Neutral" = "#ADD8E6",
    "Positive" = "#FFFFE0"
  )) +
  labs(
    title = 'Emotion Score Distribution by Initiator Type(gpt-3.5-turbo)',
    x = 'Initiator Type',
    y = 'Emotion Score'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = c(0.98, 0.02),
        legend.justification = c("right", "bottom"),
        legend.box.just = c("right", "bottom"),
        legend.background = element_rect(fill = NA, linetype = "solid", colour = "lightblue"),
        legend.margin = margin(3, 3, 3, 3),
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

perform_t_test <- function(data, group1, group2, label) {
  sub_data <- data %>% filter(initiator_type %in% c(group1, group2), Labels == label)
  t_test_result <- t.test(Emotion_Score ~ initiator_type, data = sub_data, var.equal = FALSE)
  tidy(t_test_result) %>% 
    mutate(Group1 = group1, Group2 = group2, Label = label)
}

# Preallocate the dataframe with the expected columns
results <- data.frame(
  Label = character(),
  Group1 = character(),
  Group2 = character(),
  statistic = numeric(),
  p.value = numeric(),
  parameter = numeric(),
  stringsAsFactors = FALSE
)

# Populate the dataframe
for(label in unique(updated_cleaned_data$Labels)) {
  label_data <- updated_cleaned_data %>% filter(Labels == label)
  initiator_types <- unique(label_data$initiator_type)
  
  combos <- combn(initiator_types, 2, simplify = FALSE)
  for(combo in combos) {
    test_result <- perform_t_test(label_data, combo[1], combo[2], label)
    results <- rbind(results, test_result)
  }
}

# Apply the Bonferroni correction directly to the dataframe
Adjusted_P_Value <- p.adjust(results$p.value, method = "bonferroni")

# Select and rename the columns as needed
results_df_2 <- data.frame(
  Describe_Emotion = results$Label,
  Initiator_type1 = results$Group1,
  Initiator_type2 = results$Group2,
  T_Value = results$statistic,
  Degrees_of_Freedom = results$parameter,
  Original_P_Value = results$p.value,
  Adjusted_P_Value = Adjusted_P_Value,
  Significance = Adjusted_P_Value < 0.05
)

kable(results_df_2, caption = "Comparison of Emotion Scores Across Initiator Types.BC mean Bonferroni Corrected.", align = "c",
      format = "html", row.names = FALSE, digits = 3)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

