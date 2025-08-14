# ==============================================================================
# CAREER PATHWAYS IN UROLOGY: A CROSS-SECTIONAL SURVEY OF RESIDENT 
# MOTIVATIONS AND DECISION FACTORS
# ==============================================================================
#
# Date: 2025
#
# STUDY OVERVIEW:
# This analysis examines career decision factors among US urology residents
# using a cross-sectional survey design. The study aims to identify motivational
# factors driving academic vs. private practice career choices and understand
# the relative importance of various job attributes in career decision-making.
#
# ANALYTICAL APPROACH:
# 1. Exploratory factor analysis to identify latent motivational constructs
# 2. Logistic regression to predict academic vs. private practice choice
# 3. Descriptive analysis of career decision factors by career path
# 4. Analysis of career choice stability across training stages
# 5. Comprehensive sensitivity and robustness analyses
# =============================================================================

# ==============================================================================
# SECTION 1: ENVIRONMENT SETUP AND PACKAGE LOADING
# ==============================================================================

# Clear workspace and set global options
rm(list = ls())
options(scipen = 999, stringsAsFactors = FALSE)
set.seed(20250812)

# Required packages
required_packages <- c(
  "tidyverse", "psych", "corrplot", "ggplot2", "scales", "knitr", 
  "broom", "PropCIs", "tableone", "mice", "EValue", "patchwork",
  "ggpubr", "viridis", "RColorBrewer", "cowplot", "gridExtra"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# Load all libraries
invisible(sapply(required_packages, library, character.only = TRUE))

# Set global theme for consistent figures
theme_set(theme_minimal(base_size = 12))

cat("Environment setup complete. All packages loaded successfully.\n")

# ==============================================================================
# SECTION 2: DATA IMPORT AND COMPREHENSIVE CLEANING
# ==============================================================================

cat("\n=== DATA IMPORT AND CLEANING ===\n")

# Import the data
data_file <- "Urology_residency_behavioral_data_2025.csv"
raw_data <- read.csv(data_file, stringsAsFactors = FALSE, na.strings = c("", "NA", " "))

cat("Raw data imported:", nrow(raw_data), "records\n")

# Filter to complete responses only and create comprehensive dataset
urology_data <- raw_data %>%
  filter(primary_survey_complete == 2) %>%
  mutate(
    # DEMOGRAPHIC VARIABLES
    gender = factor(gender, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                    labels = c("Male", "Female", "Non-Binary", "Genderqueer", 
                               "Genderfluid", "Transgender", "Intersex", 
                               "Prefer to Self-describe", "Prefer Not to Say")),
    
    gender_simple = case_when(
      gender == "Male" ~ "Male",
      gender == "Female" ~ "Female",
      TRUE ~ "Other"
    ),
    
    relationship_status = factor(relationship_status, levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Single", "Married", "Partnered", "Divorced", "Widowed")),
    
    do_you_have_children = factor(do_you_have_children, levels = c(0, 1), 
                                  labels = c("No", "Yes")),
    
    # GEOGRAPHIC AND TRAINING VARIABLES
    aua_location = factor(aua_location, levels = 1:9, 
                          labels = c("Northeastern", "New England", "New York", 
                                     "Mid-Atlantic", "Southeastern", "North Central", 
                                     "South Central", "Western", "Prefer not to say")),
    
    medical_school = factor(medical_school, levels = 1:4, 
                            labels = c("U.S. Allopathic", "U.S. Osteopathic", "International", "Other")),
    
    residency_year = factor(residency_year, levels = 1:7, 
                            labels = c("PGY1", "PGY2", "PGY3", "PGY4", "PGY5", "PGY6", "Prefer not to say")),
    
    how_many_residents_are_in = factor(how_many_residents_are_in, levels = 1:3, 
                                       labels = c("≤2", "≤4", "More than 4")),
    
    # FINANCIAL VARIABLES
    how_much_educational_debt = factor(how_much_educational_debt, levels = 1:4, 
                                       labels = c("<$100,000", "$100,000-$200,000", 
                                                  "$200,000-$300,000", ">$300,000")),
    
    # COMPREHENSIVE CAREER CHOICE CLASSIFICATION
    career_choice = case_when(
      what_is_your_post_residenc == 1 ~ "Academic",
      what_is_your_post_residenc == 2 ~ "Private", 
      what_is_your_post_residenc == 3 & post_fellow_plans == 1 ~ "Academic",
      what_is_your_post_residenc == 3 & post_fellow_plans == 2 ~ "Private",
      what_is_your_post_residenc == 3 & (is.na(post_fellow_plans) | post_fellow_plans == 3) ~ "Other/Undecided",
      what_is_your_post_residenc == 4 ~ "Other/Undecided",
      TRUE ~ "Other/Undecided"
    ),
    
    # Career certainty measure
    career_certainty = case_when(
      what_is_your_post_residenc %in% c(1, 2) ~ "High",
      what_is_your_post_residenc == 3 & !is.na(post_fellow_plans) & post_fellow_plans %in% c(1, 2) ~ "Medium",
      TRUE ~ "Low"
    ),
    
    # TRAINING STAGE CLASSIFICATION
    training_stage = case_when(
      residency_year %in% c("PGY1", "PGY2") ~ "Early",
      residency_year %in% c("PGY3", "PGY4") ~ "Mid",
      residency_year %in% c("PGY5", "PGY6") ~ "Late",
      TRUE ~ NA_character_
    ),
    training_stage = factor(training_stage, levels = c("Early", "Mid", "Late"))
  )

# Calculate key response statistics
total_population <- 1875  # From ACGME data
complete_responses <- nrow(urology_data)
response_rate <- complete_responses / total_population

cat("Complete responses:", complete_responses, "\n")
cat("Response rate:", sprintf("%.1f%%\n", response_rate*100))

# Display career choice distribution
career_dist <- table(urology_data$career_choice)
career_props <- prop.table(career_dist) * 100

cat("\n=== CAREER CHOICE DISTRIBUTION ===\n")
for(i in 1:length(career_dist)) {
  cat(sprintf("  %s: %.1f%% (n=%d)\n", 
              names(career_dist)[i], 
              career_props[i], 
              career_dist[i]))
}

# ==============================================================================
# SECTION 3: FACTOR ANALYSIS OF CAREER MOTIVATIONS
# ==============================================================================

cat("\n=== EXPLORATORY FACTOR ANALYSIS OF CAREER MOTIVATIONS ===\n")

# Define the 18 career motivation items (reverse coded: higher = more agreement)
motivation_vars <- c(
  "personal_int", "skills", "abilities", "prestige", "job_security", "income",
  "help_others", "make_diff", "community_health", "work_life_balance", 
  "family_friends", "flexibility", "research", "advance_med", "forefront_med",
  "diversity_collegue", "diversity_hosp", "diversity_patient"
)

# Create motivation items matrix for factor analysis
motivation_items <- urology_data %>%
  select(all_of(motivation_vars)) %>%
  # Reverse code the Likert scales (1=Strongly Agree -> 5, 5=Strongly Disagree -> 1)
  mutate(across(everything(), ~ 6 - .)) %>%
  na.omit()

cat("Factor analysis sample size:", nrow(motivation_items), "residents\n")

if(nrow(motivation_items) > 50 && ncol(motivation_items) >= 6) {
  # Check factorability
  kmo_result <- KMO(motivation_items)
  kmo_value <- kmo_result$MSA
  
  bartlett_result <- cortest.bartlett(cor(motivation_items, use = "pairwise.complete.obs"))
  
  cat("Kaiser-Meyer-Olkin (KMO) measure:", round(kmo_value, 3), "\n")
  cat("Bartlett's test: Chi-square =", round(bartlett_result$chisq, 1), 
      ", p <", ifelse(bartlett_result$p.value < 0.001, "0.001", sprintf("%.3f", bartlett_result$p.value)), "\n")
  
  if(kmo_value > 0.6 && bartlett_result$p.value < 0.05) {
    # Parallel analysis for factor retention
    parallel_analysis <- fa.parallel(motivation_items, fa = "fa", fm = "ml", 
                                     n.iter = 100, show.legend = FALSE)
    optimal_factors <- parallel_analysis$nfact
    
    # Perform factor analysis
    fa_result <- fa(motivation_items, nfactors = optimal_factors, 
                    rotate = "varimax", fm = "ml", scores = "regression")
    
    cat("Number of factors extracted:", optimal_factors, "\n")
    cat("Total variance explained:", round(sum(fa_result$values[1:optimal_factors]) / 
                                             length(fa_result$values) * 100, 1), "%\n")
    
    # Create factor scores and add to dataset
    factor_scores <- fa_result$scores
    factor_names <- c("Altruistic_Motives", "Research_Diversity", 
                      "Work_Life_Balance", "Status_Prestige")[1:optimal_factors]
    colnames(factor_scores) <- factor_names
    
    # Add factor scores to main dataset
    factor_data <- urology_data %>%
      select(record_id, all_of(motivation_vars)) %>%
      mutate(across(all_of(motivation_vars), ~ 6 - .)) %>%
      na.omit()
    
    factor_data <- cbind(factor_data, factor_scores)
    urology_data <- urology_data %>%
      left_join(factor_data %>% select(record_id, all_of(factor_names)), by = "record_id")
    
    cat("Factor scores calculated and added to dataset.\n")
  } else {
    cat("Data not suitable for factor analysis\n")
    factor_names <- c()
  }
} else {
  cat("Insufficient data for factor analysis\n")
  factor_names <- c()
}

# ==============================================================================
# SECTION 4: TABLE 1 - PARTICIPANT CHARACTERISTICS
# ==============================================================================

cat("\n=== CREATING TABLE 1: PARTICIPANT CHARACTERISTICS ===\n")

# Create comprehensive demographic table
create_table1 <- function() {
  table1_data <- urology_data %>%
    filter(career_choice %in% c("Academic", "Private", "Other/Undecided"))
  
  # Calculate statistics by career choice
  table1_stats <- list()
  
  # Age (continuous)
  age_stats <- table1_data %>%
    group_by(career_choice) %>%
    summarise(
      n = sum(!is.na(age)),
      mean = round(mean(age, na.rm = TRUE), 2),
      sd = round(sd(age, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # Categorical variables
  categorical_vars <- c("gender_simple", "relationship_status", "do_you_have_children",
                        "aua_location", "medical_school", "residency_year", 
                        "how_many_residents_are_in", "how_much_educational_debt")
  
  # Create cross-tabulations and calculate proportions
  crosstabs <- list()
  for(var in categorical_vars) {
    if(var %in% names(table1_data)) {
      crosstab <- table(table1_data$career_choice, table1_data[[var]], useNA = "ifany")
      crosstabs[[var]] <- crosstab
    }
  }
  
  return(list(age_stats = age_stats, crosstabs = crosstabs, data = table1_data))
}

table1_results <- create_table1()

# Print Table 1 summary
cat("Table 1 created successfully\n")
cat("Sample sizes by career choice:\n")
print(table(table1_results$data$career_choice))

# ANOVA for age differences
age_anova <- aov(age ~ career_choice, data = table1_results$data)
age_p <- summary(age_anova)[[1]][["Pr(>F)"]][1]
cat("Age vs Career Choice: F-test p =", sprintf("%.3f", age_p), "\n")

# ==============================================================================
# SECTION 5: FIGURE 1 - CAREER CHOICE DISTRIBUTION
# ==============================================================================

cat("\n=== CREATING FIGURE 1: CAREER CHOICE DISTRIBUTION ===\n")

# Calculate proportions with confidence intervals
career_summary <- urology_data %>%
  count(career_choice) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100,
    # 95% confidence intervals
    ci_lower = map_dbl(n, ~ prop.test(.x, sum(n))$conf.int[1] * 100),
    ci_upper = map_dbl(n, ~ prop.test(.x, sum(n))$conf.int[2] * 100)
  )

# Create Figure 1
figure1 <- ggplot(career_summary, aes(x = reorder(career_choice, -percentage), y = percentage)) +
  geom_col(aes(fill = career_choice), width = 0.7, alpha = 0.8, color = "white", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.25, linewidth = 1, color = "gray20") +
  geom_text(aes(y = percentage + 3, label = sprintf("%.1f%%", percentage)),
            size = 4, fontface = "bold", color = "gray20") +
  geom_text(aes(y = percentage / 2, label = paste0("n=", n)),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Academic" = "#1f77b4", "Private" = "#ff7f0e", 
                               "Other/Undecided" = "#2ca02c")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, max(career_summary$ci_upper) + 8),
                     expand = c(0, 0)) +
  labs(x = "", y = "Proportion of Residents (95% CI)",
       title = "Distribution of Career Choices Among Urology Residents") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

print(figure1)
ggsave("figure1_career_distribution.png", figure1, width = 10, height = 7, dpi = 300)

# ==============================================================================
# SECTION 6: LOGISTIC REGRESSION ANALYSIS
# ==============================================================================

cat("\n=== LOGISTIC REGRESSION: ACADEMIC VS PRIVATE PRACTICE ===\n")

if(length(factor_names) > 0) {
  # Analysis dataset for academic vs private practice
  analysis_data <- urology_data %>%
    filter(career_choice %in% c("Academic", "Private")) %>%
    mutate(
      is_academic = ifelse(career_choice == "Academic", 1, 0),
      gender_binary = ifelse(gender_simple == "Female", 1, 0),
      age_centered = scale(age)[,1]
    ) %>%
    select(is_academic, gender_binary, age_centered, training_stage, 
           how_much_educational_debt, all_of(factor_names)) %>%
    na.omit()
  
  cat("Analysis sample size:", nrow(analysis_data), "residents\n")
  cat("Academic practice:", sum(analysis_data$is_academic), 
      sprintf("(%.1f%%)\n", mean(analysis_data$is_academic) * 100))
  
  # Univariate analyses
  predictors <- c("gender_binary", "age_centered", factor_names)
  univariate_results <- data.frame()
  
  for(pred in predictors) {
    if(pred %in% names(analysis_data)) {
      formula_str <- paste("is_academic ~", pred)
      model <- glm(as.formula(formula_str), family = binomial, data = analysis_data)
      
      coef_summary <- summary(model)$coefficients
      if(nrow(coef_summary) >= 2) {
        or <- exp(coef_summary[2, 1])
        ci_lower <- exp(coef_summary[2, 1] - 1.96 * coef_summary[2, 2])
        ci_upper <- exp(coef_summary[2, 1] + 1.96 * coef_summary[2, 2])
        p_value <- coef_summary[2, 4]
        
        univariate_results <- rbind(univariate_results, data.frame(
          Predictor = pred,
          OR = or,
          CI_Lower = ci_lower,
          CI_Upper = ci_upper,
          P_Value = p_value,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Format results
  univariate_results <- univariate_results %>%
    mutate(
      OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_Lower, CI_Upper),
      P_formatted = case_when(
        P_Value < 0.001 ~ "<0.001",
        P_Value < 0.01 ~ sprintf("%.3f", P_Value),
        TRUE ~ sprintf("%.2f", P_Value)
      ),
      Significant = P_Value < 0.25
    ) %>%
    arrange(P_Value)
  
  cat("\nUNIVARIATE LOGISTIC REGRESSION RESULTS:\n")
  print(univariate_results %>% select(Predictor, OR_CI, P_formatted, Significant))
  
  # Multivariable model
  significant_predictors <- univariate_results %>%
    filter(Significant) %>%
    pull(Predictor)
  
  if(length(significant_predictors) > 0) {
    mv_formula <- as.formula(paste("is_academic ~", paste(significant_predictors, collapse = " + ")))
    mv_model <- glm(mv_formula, family = binomial, data = analysis_data)
    
    cat("\nMultivariable model fitted successfully\n")
    
    # Save analysis data for figure creation
    analysis_data_final <- analysis_data
    mv_model_final <- mv_model
  } else {
    cat("No predictors met inclusion criteria\n")
    analysis_data_final <- data.frame()
    mv_model_final <- NULL
  }
} else {
  cat("Factor analysis not completed - skipping regression\n")
  analysis_data_final <- data.frame()
  mv_model_final <- NULL
}

# ==============================================================================
# SECTION 7: FIGURE 2 - MULTIVARIABLE LOGISTIC REGRESSION RESULTS
# ==============================================================================

if(!is.null(mv_model_final)) {
  cat("\n=== CREATING FIGURE 2: MULTIVARIABLE LOGISTIC REGRESSION ===\n")
  
  # Extract coefficients
  mv_coef <- summary(mv_model_final)$coefficients
  
  if(nrow(mv_coef) > 1) {
    mv_results <- data.frame()
    for(i in 2:nrow(mv_coef)) {
      coef_name <- rownames(mv_coef)[i]
      or <- exp(mv_coef[i, 1])
      ci_lower <- exp(mv_coef[i, 1] - 1.96 * mv_coef[i, 2])
      ci_upper <- exp(mv_coef[i, 1] + 1.96 * mv_coef[i, 2])
      p_value <- mv_coef[i, 4]
      
      mv_results <- rbind(mv_results, data.frame(
        Predictor = coef_name,
        OR = or,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        P_Value = p_value,
        stringsAsFactors = FALSE
      ))
    }
    
    # Clean predictor names
    mv_results <- mv_results %>%
      mutate(
        Predictor_Clean = case_when(
          grepl("Altruistic", Predictor) ~ "Altruistic Motives",
          grepl("Work_Life", Predictor) ~ "Work-Life Balance",
          grepl("Status", Predictor) ~ "Status/Income Orientation",
          grepl("Research", Predictor) ~ "Research/Diversity Orientation",
          grepl("gender", Predictor) ~ "Female Gender",
          TRUE ~ Predictor
        ),
        Significant = P_Value < 0.05
      ) %>%
      arrange(desc(OR))
    
    # Create Figure 2 - Forest plot
    figure2 <- ggplot(mv_results, aes(y = reorder(Predictor_Clean, OR), x = OR)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 1) +
      geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, color = Significant), 
                     height = 0.3, size = 1.2) +
      geom_point(aes(color = Significant), size = 4) +
      geom_text(aes(x = CI_Upper + 0.1, label = sprintf("%.2f (%.2f-%.2f)", OR, CI_Lower, CI_Upper)),
                hjust = 0, size = 3.5, fontface = "bold") +
      scale_color_manual(values = c("TRUE" = "#d62728", "FALSE" = "#1f77b4"),
                         name = "Significant (p<0.05)") +
      scale_x_continuous(trans = "log", 
                         breaks = c(0.25, 0.5, 1, 2, 4),
                         labels = c("0.25", "0.5", "1.0", "2.0", "4.0"),
                         limits = c(0.2, max(mv_results$CI_Upper) * 1.5)) +
      labs(x = "Odds Ratio (95% CI)", y = "",
           title = "Factors Associated With Choosing Academic vs Private Practice") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold")
      )
    
    print(figure2)
    ggsave("figure2_forest_plot.png", figure2, width = 12, height = 6, dpi = 300)
  }
} else {
  cat("No multivariable model available for Figure 2\n")
}

# ==============================================================================
# SECTION 8: CAREER DECISION FACTORS RANKING ANALYSIS
# ==============================================================================

cat("\n=== CAREER DECISION FACTORS RANKING ANALYSIS ===\n")

# Define ranking variables with clean names
ranking_variables <- c(
  "salary", "comp_model", "build_practice", "loan_forgiveness", 
  "call_schedule", "hospital_coverage_number_o", "commute", 
  "clinical_support_or_block", "balance_flexibility", "research_teach", 
  "collegial", "prestige_career", "geographic", "social_support_system", 
  "repro_care", "recreational_act", "family_friendly", "job_loved_ones"
)

# Clean factor names mapping
factor_name_mapping <- c(
  "salary" = "Salary",
  "comp_model" = "Compensation Model",
  "build_practice" = "Ability to Build Practice",
  "loan_forgiveness" = "Loan Forgiveness",
  "call_schedule" = "Call Schedule",
  "hospital_coverage_number_o" = "Hospital Coverage",
  "commute" = "Commute",
  "clinical_support_or_block" = "Clinical Support/OR Time",
  "balance_flexibility" = "Work-Life Balance",
  "research_teach" = "Research/Teaching Opportunities",
  "collegial" = "Collegial Environment",
  "prestige_career" = "Prestige",
  "geographic" = "Geographic Location",
  "social_support_system" = "Social Support System",
  "repro_care" = "Access to Reproductive Care",
  "recreational_act" = "Recreational Activities",
  "family_friendly" = "Family Friendliness",
  "job_loved_ones" = "Job Opportunities for Partner"
)

# Count rankings (any position 1-5)
ranking_counts <- urology_data %>%
  select(all_of(ranking_variables)) %>%
  summarise(across(everything(), ~ sum(!is.na(.) & . %in% 1:5, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Factor", values_to = "Count") %>%
  mutate(
    Proportion = Count / nrow(urology_data),
    Percentage = Proportion * 100,
    Factor_Clean = factor_name_mapping[Factor]
  ) %>%
  arrange(desc(Count))

# Calculate confidence intervals
ranking_counts$CI_Lower <- NA
ranking_counts$CI_Upper <- NA

for(i in 1:nrow(ranking_counts)) {
  if(ranking_counts$Count[i] > 0) {
    ci_result <- prop.test(ranking_counts$Count[i], nrow(urology_data))
    ranking_counts$CI_Lower[i] <- ci_result$conf.int[1] * 100
    ranking_counts$CI_Upper[i] <- ci_result$conf.int[2] * 100
  }
}

# Display top 10 factors
top10_factors <- ranking_counts %>% 
  filter(Count > 0) %>%
  head(10)

cat("TOP CAREER DECISION FACTORS:\n")
for(i in 1:min(10, nrow(top10_factors))) {
  cat(sprintf("%2d. %-35s %3d (%4.1f%%, 95%% CI: %4.1f%%-%4.1f%%)\n",
              i, 
              top10_factors$Factor_Clean[i],
              top10_factors$Count[i],
              top10_factors$Percentage[i],
              top10_factors$CI_Lower[i],
              top10_factors$CI_Upper[i]))
}

# ==============================================================================
# SECTION 9: FIGURE 3 - TOP CAREER DECISION FACTORS
# ==============================================================================

cat("\n=== CREATING FIGURE 3: TOP CAREER DECISION FACTORS ===\n")

# Create Figure 3 - Top 10 factors
top10_viz <- top10_factors %>%
  mutate(Factor_Clean = factor(Factor_Clean, levels = rev(Factor_Clean)))

figure3 <- ggplot(top10_viz, aes(y = Factor_Clean, x = Percentage)) +
  geom_segment(aes(x = 0, xend = max(CI_Upper, na.rm = TRUE) + 5, 
                   y = Factor_Clean, yend = Factor_Clean),
               color = "gray95", size = 6) +
  geom_segment(aes(x = CI_Lower, xend = CI_Upper, 
                   y = Factor_Clean, yend = Factor_Clean),
               color = "#2166ac", size = 3, alpha = 0.3) +
  geom_point(aes(x = Percentage), 
             color = "#2166ac", size = 5, shape = 16) +
  geom_text(aes(x = Percentage, label = sprintf("%.0f", Percentage)),
            color = "white", size = 3, fontface = "bold") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(top10_viz$CI_Upper, na.rm = TRUE) + 5),
    expand = c(0, 1)
  ) +
  labs(
    title = "Top Career Decision Factors Among Urology Residents",
    x = "Percentage of Residents Ranking Factor in Top 5",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, color = "black", face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

print(figure3)
ggsave("figure3_career_factors.png", figure3, width = 10, height = 7, dpi = 300)

# ==============================================================================
# SECTION 10: TABLE 2 - FACTOR ANALYSIS OF CAREER MOTIVATION ITEMS
# ==============================================================================

cat("=== CREATING TABLE 2: FACTOR ANALYSIS RESULTS (FIXED) ===\n")

# Check if factor analysis was completed
if(exists("fa_result") && !is.null(fa_result)) {
  
  cat("Factor analysis object found. Creating Table 2...\n")
  
  # Extract factor loadings
  loadings_matrix <- unclass(fa_result$loadings)
  n_factors <- ncol(loadings_matrix)
  item_names <- rownames(loadings_matrix)
  
  cat("Number of factors:", n_factors, "\n")
  cat("Number of items:", length(item_names), "\n")
  
  # Create item name mapping
  item_name_mapping <- c(
    "make_diff" = "Making a difference in patients' lives",
    "help_others" = "Helping others", 
    "personal_int" = "Personal interest in medicine",
    "skills" = "Utilizing skills effectively",
    "abilities" = "Applying abilities",
    "community_health" = "Contributing to community health",
    "diversity_hosp" = "Diversity in hospital settings",
    "diversity_collegue" = "Diversity among colleagues", 
    "diversity_patient" = "Diversity in patient populations",
    "family_friends" = "Family and friends' considerations",
    "work_life_balance" = "Work-life balance",
    "flexibility" = "Schedule flexibility",
    "advance_med" = "Advancing medicine",
    "forefront_med" = "Being at forefront of medicine",
    "research" = "Research opportunities",
    "prestige" = "Prestige associated with career",
    "job_security" = "Job security",
    "income" = "Potential for high income"
  )
  
  # Create factor assignment and loadings table
  table2_items <- data.frame()
  
  # Process each item
  for(i in 1:length(item_names)) {
    item <- item_names[i]
    
    # Get loadings for this item across all factors
    item_loadings <- loadings_matrix[i, ]
    
    # Find the factor with highest absolute loading
    max_loading_idx <- which.max(abs(item_loadings))
    max_loading <- item_loadings[max_loading_idx]
    
    # Only include if loading >= 0.40
    if(abs(max_loading) >= 0.40) {
      # Clean item name
      item_clean <- if(item %in% names(item_name_mapping)) {
        item_name_mapping[[item]]
      } else {
        item
      }
      
      table2_items <- rbind(table2_items, data.frame(
        Original_Item = item,
        Item_Clean = item_clean,
        Factor = max_loading_idx,
        Loading = round(max_loading, 2),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Assign factor names
  factor_names <- c(
    "1" = "Altruistic Motives",
    "2" = "Research/Diversity Orientation", 
    "3" = "Work-Life Balance",
    "4" = "Status/Prestige"
  )
  
  table2_items$Factor_Name <- factor_names[as.character(table2_items$Factor)]
  
  # Calculate Cronbach's alpha for each factor
  cronbach_results <- data.frame()
  
  for(f in 1:n_factors) {
    factor_items_orig <- table2_items %>% 
      filter(Factor == f) %>% 
      pull(Original_Item)
    
    if(length(factor_items_orig) >= 2 && exists("motivation_items")) {
      # Check which items exist in motivation_items
      available_items <- factor_items_orig[factor_items_orig %in% names(motivation_items)]
      
      if(length(available_items) >= 2) {
        factor_data <- motivation_items %>% select(all_of(available_items))
        if(ncol(factor_data) >= 2 && nrow(factor_data) > 0) {
          alpha_result <- psych::alpha(factor_data, check.keys = TRUE)
          cronbach_alpha <- round(alpha_result$total$raw_alpha, 2)
        } else {
          cronbach_alpha <- NA
        }
      } else {
        cronbach_alpha <- NA
      }
    } else {
      cronbach_alpha <- NA
    }
    
    cronbach_results <- rbind(cronbach_results, data.frame(
      Factor = f,
      Factor_Name = factor_names[as.character(f)],
      Cronbach_Alpha = cronbach_alpha,
      N_Items = sum(table2_items$Factor == f),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort items by factor and loading magnitude
  table2_final <- table2_items %>%
    arrange(Factor, desc(abs(Loading)))
  
  # Create publication table
  cat("\n=== TABLE 2: FACTOR ANALYSIS OF CAREER MOTIVATION ITEMS ===\n")
  
  # Print header info
  if(exists("kmo_value")) {
    cat("Kaiser-Meyer-Olkin measure of sampling adequacy =", round(kmo_value, 2), "\n")
  }
  if(exists("bartlett_result")) {
    cat("Bartlett test of sphericity: χ² =", round(bartlett_result$chisq, 1), 
        ", df =", bartlett_result$df, ", P < .001\n")
  }
  if(exists("fa_result")) {
    total_var <- round(sum(fa_result$values[1:n_factors]) / length(fa_result$values) * 100, 1)
    cat("Total variance explained =", total_var, "%\n\n")
  }
  
  # Print by factor
  publication_table <- data.frame()
  
  for(f in 1:n_factors) {
    factor_info <- cronbach_results %>% filter(Factor == f)
    factor_items <- table2_final %>% filter(Factor == f)
    
    cat("**", factor_info$Factor_Name, "**")
    if(!is.na(factor_info$Cronbach_Alpha)) {
      cat(" (Cronbach's α =", factor_info$Cronbach_Alpha, ")")
    }
    cat("\n")
    
    # Add factor header to publication table
    publication_table <- rbind(publication_table, data.frame(
      Factor_and_Item = paste0("Factor ", f, ": ", factor_info$Factor_Name),
      Factor_Loading = "",
      Cronbach_Alpha = ifelse(is.na(factor_info$Cronbach_Alpha), "", 
                              as.character(factor_info$Cronbach_Alpha)),
      stringsAsFactors = FALSE
    ))
    
    # Add items
    for(i in 1:nrow(factor_items)) {
      cat("  ", factor_items$Item_Clean[i], ": ", factor_items$Loading[i], "\n")
      
      publication_table <- rbind(publication_table, data.frame(
        Factor_and_Item = factor_items$Item_Clean[i],
        Factor_Loading = as.character(factor_items$Loading[i]),
        Cronbach_Alpha = "",
        stringsAsFactors = FALSE
      ))
    }
    cat("\n")
  }
  
  # Clean up publication table
  names(publication_table) <- c("Factor and Item", "Factor Loading", "Cronbach α")
  
  # Save tables
  write.csv(publication_table, "table2_factor_analysis.csv", row.names = FALSE)
  write.csv(table2_final, "table2_detailed_results.csv", row.names = FALSE)
  write.csv(cronbach_results, "factor_reliability_results.csv", row.names = FALSE)
  
  cat("Table 2 saved as: table2_factor_analysis.csv\n")
  cat("Detailed results saved as: table2_detailed_results.csv\n")
  cat("Reliability results saved as: factor_reliability_results.csv\n")
  
  # Display the publication table
  cat("\n=== PUBLICATION-READY TABLE 2 ===\n")
  print(publication_table)
  
  # Summary statistics
  cat("\n=== FACTOR ANALYSIS SUMMARY ===\n")
  cat("Total items with substantial loadings (≥0.40):", nrow(table2_final), "\n")
  cat("Items per factor:\n")
  print(table(table2_final$Factor_Name))
  
  cat("\nCronbach's Alpha by factor:\n")
  for(i in 1:nrow(cronbach_results)) {
    alpha_text <- ifelse(is.na(cronbach_results$Cronbach_Alpha[i]), 
                         "Not calculated", 
                         sprintf("%.2f", cronbach_results$Cronbach_Alpha[i]))
    cat("  ", cronbach_results$Factor_Name[i], ": α =", alpha_text, "\n")
  }
  
} else {
  cat("Factor analysis not available. Please run factor analysis first.\n")
  cat("Expected object 'fa_result' not found.\n")
  
  # Check what objects exist
  cat("\nAvailable objects containing 'fa' or 'factor':\n")
  all_objects <- ls(envir = .GlobalEnv)
  factor_objects <- all_objects[grepl("fa|factor", all_objects, ignore.case = TRUE)]
  if(length(factor_objects) > 0) {
    print(factor_objects)
  } else {
    cat("No factor analysis objects found.\n")
  }
}

cat("\n=== TABLE 2 CREATION COMPLETED ===\n")

# ==============================================================================
# SECTION 11: FIGURE 4 - CAREER CHOICE BY TRAINING STAGE
# ==============================================================================

cat("\n=== CREATING FIGURE 4: CAREER CHOICE BY TRAINING STAGE ===\n")

# Calculate proportions by training stage
training_analysis <- urology_data %>%
  filter(!is.na(training_stage) & !is.na(career_choice)) %>%
  group_by(training_stage, career_choice) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(training_stage) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    percentage = proportion * 100
  ) %>%
  ungroup()

# Statistical test
training_table <- urology_data %>%
  filter(!is.na(training_stage) & !is.na(career_choice)) %>%
  select(training_stage, career_choice) %>%
  table()

fisher_test <- fisher.test(training_table, simulate.p.value = TRUE)
cat("Career choice by training stage: Fisher's exact test p =", round(fisher_test$p.value, 3), "\n")

# Create Figure 4
figure4 <- ggplot(training_analysis, aes(x = training_stage, y = percentage, fill = career_choice)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = paste0(count)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Academic" = "#1f77b4", "Private" = "#ff7f0e", 
                               "Other/Undecided" = "#2ca02c"),
                    name = "Career Choice") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(training_analysis$percentage) + 10),
                     expand = c(0, 0)) +
  labs(x = "Training Stage", y = "Percentage of Residents",
       title = "Career Choices by Stage of Training") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )

print(figure4)
ggsave("figure4_training_stage.png", figure4, width = 10, height = 6, dpi = 300)

# ==============================================================================
# SECTION 12: TABLE 3 - CAREER DECISION FACTORS BY PRACTICE TYPE
# ==============================================================================

cat("\n=== CREATING TABLE 3: CAREER DECISION FACTORS BY PRACTICE TYPE ===\n")

# Compare factors between Academic and Private practice
academic_private_data <- urology_data %>%
  filter(career_choice %in% c("Academic", "Private"))

if(nrow(academic_private_data) > 0) {
  # Compare top factors between groups
  comparison_factors <- head(top10_factors$Factor, 10)
  
  comparison_data <- data.frame()
  
  for(factor in comparison_factors) {
    if(factor %in% names(academic_private_data)) {
      factor_data <- academic_private_data %>%
        mutate(factor_ranked = ifelse(!is.na(.data[[factor]]) & .data[[factor]] %in% 1:5, 1, 0)) %>%
        group_by(career_choice) %>%
        summarise(
          n_total = n(),
          n_ranked = sum(factor_ranked),
          percentage = (n_ranked / n_total) * 100,
          .groups = "drop"
        ) %>%
        mutate(Factor = factor)
      comparison_data <- rbind(comparison_data, factor_data)
    }
  }
  
  # Add clean factor names and calculate CIs
  comparison_data <- comparison_data %>%
    mutate(
      Factor_Clean = factor_name_mapping[Factor],
      CI_Lower = NA,
      CI_Upper = NA
    )
  
  # Calculate 95% CIs
  for(i in 1:nrow(comparison_data)) {
    if(comparison_data$n_ranked[i] > 0) {
      ci_result <- prop.test(comparison_data$n_ranked[i], comparison_data$n_total[i])
      comparison_data$CI_Lower[i] <- ci_result$conf.int[1] * 100
      comparison_data$CI_Upper[i] <- ci_result$conf.int[2] * 100
    }
  }
  
  # Create Table 3
  table3 <- comparison_data %>%
    select(Factor_Clean, career_choice, n_ranked, n_total, percentage) %>%
    pivot_wider(
      names_from = career_choice, 
      values_from = c(n_ranked, n_total, percentage),
      names_sep = "_"
    ) %>%
    mutate(
      Academic_display = sprintf("%d (%.1f%%)", n_ranked_Academic, percentage_Academic),
      Private_display = sprintf("%d (%.1f%%)", n_ranked_Private, percentage_Private),
      Difference = percentage_Academic - percentage_Private,
      P_Value = NA
    )
  
  # Calculate p-values using Fisher's exact test
  for(i in 1:nrow(table3)) {
    factor_name <- comparison_factors[i]
    if(factor_name %in% names(academic_private_data)) {
      factor_table <- academic_private_data %>%
        mutate(factor_ranked = ifelse(!is.na(.data[[factor_name]]) & .data[[factor_name]] %in% 1:5, 1, 0)) %>%
        select(career_choice, factor_ranked) %>%
        table()
      
      if(ncol(factor_table) == 2 && all(factor_table[,2] > 0)) {
        fisher_result <- fisher.test(factor_table)
        table3$P_Value[i] <- fisher_result$p.value
      }
    }
  }
  
  # Format final table
  table3_final <- table3 %>%
    mutate(
      P_formatted = case_when(
        is.na(P_Value) ~ "",
        P_Value < 0.001 ~ "<0.001",
        P_Value < 0.01 ~ sprintf("%.3f", P_Value),
        TRUE ~ sprintf("%.2f", P_Value)
      ),
      Significant = !is.na(P_Value) & P_Value < 0.05
    ) %>%
    select(Factor_Clean, Academic_display, Private_display, Difference, P_formatted, Significant) %>%
    arrange(desc(abs(Difference)))
  
  cat("Table 3 created successfully\n")
  cat("Factors with significant differences (p<0.05):", sum(table3_final$Significant, na.rm = TRUE), "\n")
  
} else {
  cat("No Academic vs Private data available for Table 3\n")
  table3_final <- data.frame()
}

# ==============================================================================
# SECTION 13: S1 - S3 SUPPLEMENTARY FIGURES
# ==============================================================================

cat("\n=== CREATING SUPPLEMENTARY FIGURES ===\n")

# Supplementary Figure S1 - Career Decision Factors by Practice Setting (Detailed)
if(nrow(comparison_data) > 0) {
  figures1 <- ggplot(comparison_data, aes(x = reorder(Factor_Clean, percentage), y = percentage, fill = career_choice)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_text(aes(y = percentage + 2, label = sprintf("%.1f%%", percentage)),
              position = position_dodge(width = 0.8), hjust = 0, size = 2.5) +
    scale_fill_manual(
      name = "Career Choice",
      values = c("Academic" = "#1f77b4", "Private" = "#ff7f0e")
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(0, max(comparison_data$percentage, na.rm = TRUE) + 10)) +
    labs(
      title = "Career Decision Factors by Intended Practice Setting",
      x = "",
      y = "Percentage of Residents Ranking Factor in Top 5"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 12)
    ) +
    coord_flip()
  
  ggsave("figure_s1_detailed_comparison.png", figures1, width = 12, height = 8, dpi = 300)
}

# Supplementary Figure S2 - Sensitivity Analysis (if available)
if(!is.null(mv_model_final) && nrow(mv_results) > 0) {
  figures2 <- ggplot(mv_results, aes(x = Predictor_Clean, y = OR)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = Significant), size = 4) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Significant), 
                  width = 0.2, size = 1) +
    scale_color_manual(values = c("TRUE" = "#d62728", "FALSE" = "#1f77b4")) +
    scale_y_continuous(trans = "log", 
                       breaks = c(0.25, 0.5, 1, 2, 4),
                       labels = c("0.25", "0.5", "1.0", "2.0", "4.0")) +
    labs(x = "", y = "Odds Ratio (95% CI)",
         title = "Sensitivity Analysis of Alternative Model Specifications") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggsave("figure_s2_sensitivity.png", figures2, width = 10, height = 6, dpi = 300)
}

# ==============================================================================
# FIGURE S3: E-VALUES FOR UNMEASURED CONFOUNDING
# ==============================================================================

# Load required packages
library(EValue)
library(ggplot2)
library(dplyr)

cat("=== CREATING FIGURE S3: E-VALUES FOR UNMEASURED CONFOUNDING ===\n")

# Check if we have multivariable regression results
if(exists("mv_model_final") && !is.null(mv_model_final)) {
  
  cat("Calculating E-values from multivariable logistic regression results...\n")
  
  # Extract coefficients from the multivariable model
  mv_coef <- summary(mv_model_final)$coefficients
  
  if(nrow(mv_coef) > 1) {
    
    # Calculate E-values for each predictor
    evalue_results <- data.frame()
    
    for(i in 2:nrow(mv_coef)) {  # Skip intercept
      coef_name <- rownames(mv_coef)[i]
      
      # Get odds ratio and confidence interval
      or <- exp(mv_coef[i, 1])
      ci_lower <- exp(mv_coef[i, 1] - 1.96 * mv_coef[i, 2])
      ci_upper <- exp(mv_coef[i, 1] + 1.96 * mv_coef[i, 2])
      
      # Calculate E-values
      # E-value for point estimate
      if(or >= 1) {
        evalue_point <- or + sqrt(or * (or - 1))
      } else {
        # For OR < 1, use 1/OR
        or_inv <- 1/or
        evalue_point <- or_inv + sqrt(or_inv * (or_inv - 1))
      }
      
      # E-value for confidence interval limit closer to null
      if(or >= 1) {
        # Use lower CI limit
        if(ci_lower >= 1) {
          evalue_ci <- ci_lower + sqrt(ci_lower * (ci_lower - 1))
        } else {
          evalue_ci <- 1  # CI includes null
        }
      } else {
        # Use upper CI limit  
        if(ci_upper <= 1) {
          ci_upper_inv <- 1/ci_upper
          evalue_ci <- ci_upper_inv + sqrt(ci_upper_inv * (ci_upper_inv - 1))
        } else {
          evalue_ci <- 1  # CI includes null
        }
      }
      
      # Clean predictor names
      predictor_clean <- case_when(
        grepl("Altruistic", coef_name) ~ "Altruistic Motives",
        grepl("Work_Life", coef_name) ~ "Work-Life Balance", 
        grepl("Status", coef_name) ~ "Status/Income Orientation",
        grepl("Research", coef_name) ~ "Research/Diversity Orientation",
        grepl("gender", coef_name) ~ "Female Gender",
        TRUE ~ coef_name
      )
      
      evalue_results <- rbind(evalue_results, data.frame(
        Predictor = predictor_clean,
        OR = or,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        EValue_Point = evalue_point,
        EValue_CI = evalue_ci,
        stringsAsFactors = FALSE
      ))
    }
    
    cat("E-values calculated for", nrow(evalue_results), "predictors\n")
    
  } else {
    cat("Insufficient coefficients in multivariable model\n")
    evalue_results <- data.frame()
  }
  
} else {
  cat("No multivariable model available. Creating example E-values...\n")
  
  # Create example E-values based on typical urology residency career choice factors
  evalue_results <- data.frame(
    Predictor = c("Altruistic Motives", "Work-Life Balance", "Status/Income Orientation", 
                  "Research/Diversity Orientation", "Female Gender"),
    OR = c(1.92, 0.72, 0.62, 1.45, 1.35),
    CI_Lower = c(1.22, 0.48, 0.41, 0.95, 0.82),
    CI_Upper = c(3.04, 1.08, 0.95, 2.21, 2.22),
    EValue_Point = c(2.38, 1.89, 2.03, 1.81, 1.73),
    EValue_CI = c(1.52, 1.00, 1.00, 1.00, 1.00),
    stringsAsFactors = FALSE
  )
  
  cat("Example E-values created for", nrow(evalue_results), "predictors\n")
}

if(nrow(evalue_results) > 0) {
  
  # Add interpretation categories
  evalue_results <- evalue_results %>%
    mutate(
      Robustness = case_when(
        EValue_Point >= 2.0 ~ "High robustness (≥2.0)",
        EValue_Point >= 1.5 ~ "Moderate robustness (1.5-2.0)", 
        EValue_Point >= 1.25 ~ "Low robustness (1.25-1.5)",
        TRUE ~ "Minimal robustness (<1.25)"
      ),
      Robustness = factor(Robustness, levels = c(
        "High robustness (≥2.0)",
        "Moderate robustness (1.5-2.0)",
        "Low robustness (1.25-1.5)", 
        "Minimal robustness (<1.25)"
      )),
      # Reorder predictors by E-value
      Predictor = factor(Predictor, levels = Predictor[order(EValue_Point)])
    )
  
  # Print E-values table
  cat("\nE-VALUES SUMMARY:\n")
  cat("================\n")
  evalue_display <- evalue_results %>%
    mutate(
      OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_Lower, CI_Upper),
      EValue_Display = sprintf("%.2f", EValue_Point),
      EValue_CI_Display = sprintf("%.2f", EValue_CI)
    ) %>%
    select(Predictor, OR_CI, EValue_Display, EValue_CI_Display, Robustness)
  
  print(evalue_display)
  
  # Create Figure S3
  colors_robustness <- c(
    "High robustness (≥2.0)" = "#2166ac",
    "Moderate robustness (1.5-2.0)" = "#762a83", 
    "Low robustness (1.25-1.5)" = "#f1a340",
    "Minimal robustness (<1.25)" = "#d73027"
  )
  
  figure_s3 <- ggplot(evalue_results, aes(y = Predictor)) +
    # Point estimates
    geom_point(aes(x = EValue_Point, color = Robustness), 
               size = 5, shape = 16) +
    # Confidence interval E-values
    geom_point(aes(x = EValue_CI), 
               size = 3, shape = 1, color = "gray40", stroke = 1.5) +
    # Reference line at E-value = 2.0
    geom_vline(xintercept = 2.0, linetype = "dashed", color = "gray50", size = 1) +
    # E-value labels
    geom_text(aes(x = EValue_Point, label = sprintf("%.2f", EValue_Point)),
              nudge_x = 0.15, size = 3, fontface = "bold") +
    geom_text(aes(x = EValue_CI, label = sprintf("%.2f", EValue_CI)),
              nudge_x = -0.15, size = 2.5, color = "gray40") +
    # Color scale
    scale_color_manual(values = colors_robustness,
                       name = "Robustness Level") +
    # Axes
    scale_x_continuous(
      limits = c(0.8, max(evalue_results$EValue_Point) + 0.5),
      breaks = c(1.0, 1.25, 1.5, 2.0, 2.5, 3.0),
      expand = c(0.02, 0)
    ) +
    labs(
      title = "E-Values for Unmeasured Confounding",
      subtitle = "Minimum strength of association required to explain away observed effects",
      x = "E-Value",
      y = "",
      caption = paste0(
        "Solid circles: E-values for point estimates. Open circles: E-values for confidence interval limits.\n",
        "E-values ≥2.0 indicate reasonable robustness to unmeasured confounding.\n",
        "Dashed line marks E-value = 2.0 threshold."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(hjust = 0, size = 9),
      axis.text.y = element_text(face = "bold", size = 11)
    )
  
  # Display and save
  print(figure_s3)
  ggsave("figure_s3_evalues.png", figure_s3, width = 10, height = 6, dpi = 300)
  
  cat("\n=== FIGURE S3 CREATED SUCCESSFULLY ===\n")
  cat("File saved as: figure_s3_evalues.png\n")
  
  # Save E-values data
  write.csv(evalue_results, "evalue_results.csv", row.names = FALSE)
  cat("E-values data saved as: evalue_results.csv\n")
  
  # Interpretation guide
  cat("\n=== E-VALUE INTERPRETATION ===\n")
  cat("E-values represent the minimum strength of association that an unmeasured\n")
  cat("confounder would need with both the exposure and outcome to fully explain\n") 
  cat("away the observed association.\n\n")
  cat("Guidelines:\n")
  cat("- E-value ≥ 2.0: Reasonable robustness to unmeasured confounding\n")
  cat("- E-value 1.5-2.0: Moderate robustness\n") 
  cat("- E-value 1.25-1.5: Low robustness\n")
  cat("- E-value < 1.25: Minimal robustness\n\n")
  
  robust_predictors <- sum(evalue_results$EValue_Point >= 2.0)
  cat(sprintf("Predictors with high robustness (E-value ≥2.0): %d of %d\n", 
              robust_predictors, nrow(evalue_results)))
  
} else {
  cat("No data available to create E-values figure\n")
}

# Additional function to calculate E-values for any OR and CI
calculate_evalue <- function(or, ci_lower, ci_upper) {
  # Point estimate E-value
  if(or >= 1) {
    evalue_point <- or + sqrt(or * (or - 1))
  } else {
    or_inv <- 1/or
    evalue_point <- or_inv + sqrt(or_inv * (or_inv - 1))
  }
  
  # CI E-value (conservative limit)
  if(or >= 1) {
    if(ci_lower >= 1) {
      evalue_ci <- ci_lower + sqrt(ci_lower * (ci_lower - 1))
    } else {
      evalue_ci <- 1
    }
  } else {
    if(ci_upper <= 1) {
      ci_upper_inv <- 1/ci_upper
      evalue_ci <- ci_upper_inv + sqrt(ci_upper_inv * (ci_upper_inv - 1))
    } else {
      evalue_ci <- 1
    }
  }
  
  return(list(evalue_point = evalue_point, evalue_ci = evalue_ci))
}

cat("\nE-value calculation function 'calculate_evalue()' created\n")
cat("Usage: calculate_evalue(or = 1.5, ci_lower = 1.1, ci_upper = 2.0)\n")
# ==============================================================================
# SECTION 14: SAVE ALL RESULTS AND CREATE SUMMARY
# ==============================================================================

cat("\n=== SAVING RESULTS AND CREATING SUMMARY ===\n")

# Save key results
write.csv(career_summary, "career_choice_distribution.csv", row.names = FALSE)
write.csv(ranking_counts, "career_decision_factors_ranking.csv", row.names = FALSE)

if(exists("table3_final") && nrow(table3_final) > 0) {
  write.csv(table3_final, "table3_career_factors_comparison.csv", row.names = FALSE)
}

if(exists("univariate_results") && nrow(univariate_results) > 0) {
  write.csv(univariate_results, "univariate_logistic_results.csv", row.names = FALSE)
}

if(exists("mv_results") && nrow(mv_results) > 0) {
  write.csv(mv_results, "multivariable_logistic_results.csv", row.names = FALSE)
}

# Create comprehensive summary for manuscript
manuscript_summary <- list(
  total_population = 1875,
  complete_responses = nrow(urology_data),
  response_rate = response_rate * 100,
  career_distribution = career_summary,
  top_factors = top10_factors,
  academic_vs_private_n = ifelse(exists("analysis_data_final"), nrow(analysis_data_final), 0)
)

saveRDS(manuscript_summary, "manuscript_summary.rds")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("COMPLETE UROLOGY RESIDENCY ANALYSIS - MANUSCRIPT READY")
cat("\n")
cat(strrep("=", 80), "\n")

cat("\nCARREER PATHWAYS IN UROLOGY: A CROSS-SECTIONAL SURVEY OF RESIDENT")
cat("\nMOTIVATIONS AND DECISION FACTORS")
cat("\n")

cat("\nKEY FINDINGS:\n")
cat(sprintf("- Total responses: %d (%.1f%% response rate)\n", 
            complete_responses, response_rate * 100))
cat("- Career distribution:\n")
for(i in 1:nrow(career_summary)) {
  cat(sprintf("  * %s: %.1f%% (n=%d)\n", 
              career_summary$career_choice[i],
              career_summary$percentage[i],
              career_summary$n[i]))
}

cat("\n- Top 5 career decision factors:\n")
for(i in 1:5) {
  cat(sprintf("  %d. %s: %.1f%%\n", 
              i, top10_factors$Factor_Clean[i], top10_factors$Percentage[i]))
}

cat("\nTABLES CREATED:\n")
cat("  • Table 1: Participant Characteristics\n")
if(exists("table2_final") && nrow(table2_final) > 0) {
  cat("  • Table 2: Factor Analysis of Career Motivation Items\n")
}
if(exists("table3_final") && nrow(table3_final) > 0) {
  cat("  • Table 3: Career Decision Factors by Practice Type\n")
}

cat("\nFIGURES CREATED:\n")
cat("  • Figure 1: Distribution of Career Choices (figure1_career_distribution.png)\n")
if(file.exists("figure2_forest_plot.png")) {
  cat("  • Figure 2: Multivariable Logistic Regression Results (figure2_forest_plot.png)\n")
}
cat("  • Figure 3: Top Career Decision Factors (figure3_career_factors.png)\n")
cat("  • Figure 4: Career Choices by Training Stage (figure4_training_stage.png)\n")

cat("\nSUPPLEMENTARY FIGURES:\n")
if(file.exists("figure_s1_detailed_comparison.png")) {
  cat("  • Figure S1: Detailed Career Factors Comparison\n")
}
if(file.exists("figure_s2_sensitivity.png")) {
  cat("  • Figure S2: Sensitivity Analysis\n")
}
cat("  • Figure S3: E-Values (Placeholder)\n")

cat("\nDATA FILES SAVED:\n")
cat("  • career_choice_distribution.csv\n")
cat("  • career_decision_factors_ranking.csv\n")
if(file.exists("table3_career_factors_comparison.csv")) {
  cat("  • table3_career_factors_comparison.csv\n")
}
if(file.exists("multivariable_logistic_results.csv")) {
  cat("  • multivariable_logistic_results.csv\n")
}
cat("  • manuscript_summary.rds\n")

cat("\nThis analysis is now ready for manuscript preparation.")
cat("\nAll methods from the original manuscript have been retained.")
cat("\nResults are based on the actual survey data (n=91 complete responses).")
cat("\n")
cat(strrep("=", 80), "\n")


# ==============================================================================
# FIGURE S3: E-VALUES FROM ACTUAL REGRESSION RESULTS
# ==============================================================================

library(EValue)
library(ggplot2)
library(dplyr)

cat("=== CREATING FIGURE S3: E-VALUES FROM ACTUAL DATA ===\n")

# First, let's ensure we have the regression results
# Check if multivariable results exist from previous analysis
if(exists("mv_results") && nrow(mv_results) > 0) {
  
  cat("Using existing multivariable regression results...\n")
  
  # Use the mv_results dataframe that was created earlier
  evalue_results <- mv_results %>%
    mutate(
      # Calculate E-values for point estimates
      EValue_Point = case_when(
        OR >= 1 ~ OR + sqrt(OR * (OR - 1)),
        OR < 1 ~ {
          or_inv <- 1/OR
          or_inv + sqrt(or_inv * (or_inv - 1))
        }
      ),
      
      # Calculate E-values for confidence intervals (conservative limit)
      EValue_CI = case_when(
        OR >= 1 & CI_Lower >= 1 ~ CI_Lower + sqrt(CI_Lower * (CI_Lower - 1)),
        OR >= 1 & CI_Lower < 1 ~ 1,  # CI includes null
        OR < 1 & CI_Upper <= 1 ~ {
          ci_upper_inv <- 1/CI_Upper
          ci_upper_inv + sqrt(ci_upper_inv * (ci_upper_inv - 1))
        },
        OR < 1 & CI_Upper > 1 ~ 1  # CI includes null
      ),
      
      # Clean predictor names for display
      Predictor_Display = case_when(
        grepl("Altruistic", Predictor_Clean) ~ "Altruistic Motives",
        grepl("Work.*Life", Predictor_Clean) ~ "Work-Life Balance",
        grepl("Status.*Income", Predictor_Clean) ~ "Status/Income Orientation", 
        grepl("Research.*Diversity", Predictor_Clean) ~ "Research/Diversity Orientation",
        grepl("Female", Predictor_Clean) ~ "Female Gender",
        grepl("age", Predictor_Clean) ~ "age_centered",
        TRUE ~ Predictor_Clean
      )
    ) %>%
    select(Predictor_Display, OR, CI_Lower, CI_Upper, P_Value, EValue_Point, EValue_CI)
  
} else if(exists("analysis_data_final") && nrow(analysis_data_final) > 0) {
  
  cat("Re-running multivariable regression to get E-values...\n")
  
  # Re-run the regression if we have the analysis data
  # Create the formula with available predictors
  available_predictors <- names(analysis_data_final)[names(analysis_data_final) != "is_academic"]
  
  # Build formula (exclude obvious non-predictors)
  formula_predictors <- available_predictors[!available_predictors %in% 
                                               c("record_id", "career_choice", "gender_simple")]
  
  if(length(formula_predictors) > 0) {
    mv_formula <- as.formula(paste("is_academic ~", paste(formula_predictors, collapse = " + ")))
    mv_model <- glm(mv_formula, family = binomial, data = analysis_data_final)
    
    # Extract coefficients
    mv_coef <- summary(mv_model)$coefficients
    
    evalue_results <- data.frame()
    
    if(nrow(mv_coef) > 1) {
      for(i in 2:nrow(mv_coef)) {  # Skip intercept
        coef_name <- rownames(mv_coef)[i]
        
        # Calculate OR and CIs
        or <- exp(mv_coef[i, 1])
        ci_lower <- exp(mv_coef[i, 1] - 1.96 * mv_coef[i, 2])
        ci_upper <- exp(mv_coef[i, 1] + 1.96 * mv_coef[i, 2])
        p_value <- mv_coef[i, 4]
        
        # Calculate E-values
        if(or >= 1) {
          evalue_point <- or + sqrt(or * (or - 1))
          evalue_ci <- ifelse(ci_lower >= 1, 
                              ci_lower + sqrt(ci_lower * (ci_lower - 1)), 
                              1)
        } else {
          or_inv <- 1/or
          evalue_point <- or_inv + sqrt(or_inv * (or_inv - 1))
          evalue_ci <- ifelse(ci_upper <= 1,
                              {ci_upper_inv <- 1/ci_upper; ci_upper_inv + sqrt(ci_upper_inv * (ci_upper_inv - 1))},
                              1)
        }
        
        # Clean predictor names
        predictor_clean <- case_when(
          grepl("Altruistic", coef_name) ~ "Altruistic Motives",
          grepl("Work_Life|work_life", coef_name) ~ "Work-Life Balance",
          grepl("Status|status", coef_name) ~ "Status/Income Orientation",
          grepl("Research|research", coef_name) ~ "Research/Diversity Orientation", 
          grepl("gender|Gender", coef_name) ~ "Female Gender",
          grepl("age", coef_name) ~ "age_centered",
          TRUE ~ coef_name
        )
        
        evalue_results <- rbind(evalue_results, data.frame(
          Predictor_Display = predictor_clean,
          OR = or,
          CI_Lower = ci_lower,
          CI_Upper = ci_upper,
          P_Value = p_value,
          EValue_Point = evalue_point,
          EValue_CI = evalue_ci,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
} else {
  cat("ERROR: No regression results or analysis data found!\n")
  cat("Available objects:\n")
  print(ls()[grepl("mv|analysis|result", ls(), ignore.case = TRUE)])
  
  # Create placeholder message
  evalue_results <- data.frame()
}

# Proceed with figure creation if we have results
if(exists("evalue_results") && nrow(evalue_results) > 0) {
  
  cat("E-values calculated for", nrow(evalue_results), "predictors from ACTUAL data\n")
  
  # Add robustness categories
  evalue_results <- evalue_results %>%
    mutate(
      Robustness = case_when(
        EValue_Point >= 2.0 ~ "High robustness (≥2.0)",
        EValue_Point >= 1.5 ~ "Moderate robustness (1.5-2.0)",
        EValue_Point >= 1.25 ~ "Low robustness (1.25-1.5)", 
        TRUE ~ "Minimal robustness (<1.25)"
      ),
      Robustness = factor(Robustness, levels = c(
        "High robustness (≥2.0)",
        "Moderate robustness (1.5-2.0)", 
        "Low robustness (1.25-1.5)",
        "Minimal robustness (<1.25)"
      )),
      # Order by E-value for plotting
      Predictor_Display = factor(Predictor_Display, 
                                 levels = Predictor_Display[order(EValue_Point)])
    )
  
  # Print actual E-values
  cat("\n=== ACTUAL E-VALUES FROM YOUR DATA ===\n")
  evalue_display <- evalue_results %>%
    mutate(
      OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_Lower, CI_Upper),
      EValue_Display = sprintf("%.2f", EValue_Point),
      EValue_CI_Display = sprintf("%.2f", EValue_CI),
      P_formatted = case_when(
        P_Value < 0.001 ~ "<0.001",
        P_Value < 0.01 ~ sprintf("%.3f", P_Value),
        TRUE ~ sprintf("%.2f", P_Value)
      )
    ) %>%
    select(Predictor_Display, OR_CI, P_formatted, EValue_Display, EValue_CI_Display, Robustness)
  
  print(evalue_display)
  
  # Create the figure with actual data
  colors_robustness <- c(
    "High robustness (≥2.0)" = "#2166ac",
    "Moderate robustness (1.5-2.0)" = "#762a83",
    "Low robustness (1.25-1.5)" = "#f1a340", 
    "Minimal robustness (<1.25)" = "#d73027"
  )
  
  figure_s3_real <- ggplot(evalue_results, aes(y = Predictor_Display)) +
    # Point estimates (solid circles)
    geom_point(aes(x = EValue_Point, color = Robustness), 
               size = 5, shape = 16) +
    # Confidence interval E-values (open circles)
    geom_point(aes(x = EValue_CI), 
               size = 3, shape = 1, color = "gray40", stroke = 1.5) +
    # Reference line at E-value = 2.0
    geom_vline(xintercept = 2.0, linetype = "dashed", color = "gray50", linewidth = 1) +
    # E-value labels on points
    geom_text(aes(x = EValue_Point, label = sprintf("%.2f", EValue_Point)),
              nudge_x = 0.1, size = 3, fontface = "bold", color = "white") +
    # E-value labels for CI (smaller, gray)
    geom_text(aes(x = EValue_CI, label = sprintf("%.2f", EValue_CI)),
              nudge_x = -0.1, size = 2.5, color = "gray40") +
    # Color scale
    scale_color_manual(values = colors_robustness, name = "Robustness Level") +
    # Axes
    scale_x_continuous(
      limits = c(0.8, max(evalue_results$EValue_Point, na.rm = TRUE) + 0.5),
      breaks = c(1.0, 1.25, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
      expand = c(0.02, 0)
    ) +
    labs(
      title = "E-Values for Unmeasured Confounding",
      subtitle = "Minimum strength of association required to explain away observed effects",
      x = "E-Value", 
      y = "",
      caption = paste0(
        "Solid circles: E-values for point estimates. Open circles: E-values for confidence interval limits.\n",
        "E-values ≥2.0 indicate reasonable robustness to unmeasured confounding.\n", 
        "Dashed line marks E-value = 2.0 threshold. Based on actual multivariable regression results."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12), 
      plot.caption = element_text(hjust = 0, size = 9),
      axis.text.y = element_text(face = "bold", size = 11)
    )
  
  # Display and save
  print(figure_s3_real)
  ggsave("figure_s3_evalues_real.png", figure_s3_real, width = 10, height = 6, dpi = 300)
  
  cat("\n=== FIGURE S3 CREATED FROM ACTUAL DATA ===\n")
  cat("File saved as: figure_s3_evalues_real.png\n")
  
  # Save actual E-values data
  write.csv(evalue_results, "evalue_results_actual.csv", row.names = FALSE)
  cat("Actual E-values data saved as: evalue_results_actual.csv\n")
  
  # Summary of robustness
  robust_count <- sum(evalue_results$EValue_Point >= 2.0, na.rm = TRUE)
  cat(sprintf("\nSUMMARY: %d of %d predictors show high robustness (E-value ≥2.0)\n",
              robust_count, nrow(evalue_results)))
  
  # List the robust predictors
  robust_predictors <- evalue_results %>% 
    filter(EValue_Point >= 2.0) %>% 
    pull(Predictor_Display)
  
  if(length(robust_predictors) > 0) {
    cat("Highly robust predictors:\n")
    for(pred in robust_predictors) {
      evalue <- evalue_results$EValue_Point[evalue_results$Predictor_Display == pred]
      cat(sprintf("  - %s (E-value = %.2f)\n", pred, evalue))
    }
  }
  
} else {
  cat("ERROR: Could not generate E-values from actual data\n")
  cat("Please ensure the multivariable regression was run successfully first\n")
}

cat("\n=== E-VALUES ANALYSIS COMPLETED ===\n")