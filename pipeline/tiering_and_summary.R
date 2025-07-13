# Load required libraries
library(dplyr)

# =============================================================================
# MODULE 4: SAMPLE SIZE TIERS
# =============================================================================

#' Create confidence tiers based on sample size
#' @param data Data frame with sample size information
#' @param sample_col String name of sample size column
#' @param method String method for creating tiers ("quartile" or "fixed")
#' @return Data frame with tier assignments added
create_sample_tiers <- function(data, sample_col, method = "quartile") {
  
  if (!sample_col %in% colnames(data)) {
    stop(paste("Sample column", sample_col, "not found in data"))
  }
  
  if (method == "quartile") {
    quartiles <- quantile(data[[sample_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    result <- data %>%
      mutate(
        confidence_tier = case_when(
          .data[[sample_col]] >= quartiles[3] ~ "High Confidence",
          .data[[sample_col]] >= quartiles[2] ~ "Moderate Confidence", 
          .data[[sample_col]] >= quartiles[1] ~ "Low Confidence",
          .data[[sample_col]] >= 10 ~ "Preliminary Only",
          TRUE ~ "Insufficient Data"
        )
      )
  } else if (method == "fixed") {
    result <- data %>%
      mutate(
        confidence_tier = case_when(
          .data[[sample_col]] >= 100 ~ "High Confidence",
          .data[[sample_col]] >= 50 ~ "Moderate Confidence",
          .data[[sample_col]] >= 25 ~ "Low Confidence", 
          .data[[sample_col]] >= 10 ~ "Preliminary Only",
          TRUE ~ "Insufficient Data"
        )
      )
  } else {
    stop("Method must be 'quartile' or 'fixed'")
  }
  
  # Add tier metadata
  result <- result %>%
    mutate(
      tier_numeric = case_when(
        confidence_tier == "High Confidence" ~ 4,
        confidence_tier == "Moderate Confidence" ~ 3,
        confidence_tier == "Low Confidence" ~ 2,
        confidence_tier == "Preliminary Only" ~ 1,
        TRUE ~ 0
      ),
      tier_color = case_when(
        confidence_tier == "High Confidence" ~ "#2E8B57",
        confidence_tier == "Moderate Confidence" ~ "#FFB347",
        confidence_tier == "Low Confidence" ~ "#FFB6C1",
        confidence_tier == "Preliminary Only" ~ "#D3D3D3",
        TRUE ~ "#FFFFFF"
      )
    )
  
  return(result)
}

#' Calculate tier thresholds for a dataset
#' @param data Data frame with sample size information
#' @param sample_col String name of sample size column
#' @param method String method for creating tiers ("quartile" or "fixed")
#' @return Named vector of threshold values
calculate_tier_thresholds <- function(data, sample_col, method = "quartile") {
  
  if (!sample_col %in% colnames(data)) {
    stop(paste("Sample column", sample_col, "not found in data"))
  }
  
  if (method == "quartile") {
    quartiles <- quantile(data[[sample_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    return(c(
      "25th_percentile" = quartiles[1],
      "50th_percentile" = quartiles[2], 
      "75th_percentile" = quartiles[3]
    ))
  } else if (method == "fixed") {
    return(c(
      "preliminary_threshold" = 10,
      "low_threshold" = 25,
      "moderate_threshold" = 50,
      "high_threshold" = 100
    ))
  } else {
    stop("Method must be 'quartile' or 'fixed'")
  }
}

# =============================================================================
# MODULE 7: SUMMARY AND ANALYSIS FUNCTIONS
# =============================================================================

#' Summarize results by confidence tier
#' @param data Data frame with tier assignments
#' @param question_stem String stem of the question (needed to find correct columns)
#' @return Summary data frame
summarize_by_tier <- function(data, question_stem) {
  # Determine the dynamic column name based on the question stem
  percent_col_name <- paste0("percent_top_n_", substr(question_stem, 1, 2))
  
  # Check if the column exists
  if (!percent_col_name %in% colnames(data)) {
    # Fallback to legacy name if dynamic name doesn't exist
    if ("percent_top_n" %in% colnames(data)) {
      percent_col_name <- "percent_top_n"
    } else {
      stop(paste("Neither", percent_col_name, "nor percent_top_n column found in data"))
    }
  }
  
  # Use rlang's sym() and !! to properly handle dynamic column names
  percent_sym <- rlang::sym(percent_col_name)
  
  data %>%
    group_by(confidence_tier) %>%
    summarise(
      n_groups = n(),
      mean_sample_size = mean(totalStudents, na.rm = TRUE),
      median_sample_size = median(totalStudents, na.rm = TRUE),
      mean_percent_top_n = mean(!!percent_sym, na.rm = TRUE),
      median_percent_top_n = median(!!percent_sym, na.rm = TRUE),
      mean_ci_width = mean(ci_width_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_groups))
}

#' Main function that ties everything together
#' @param data Raw evaluation data
#' @param group_var String name of grouping variable
#' @param question_stem String stem of question to analyze
#' @param top_n Integer number of top levels to include (default 2)
#' @param conf_level Confidence level for Wilson CI (default 0.95)
#' @param tier_method String method for sample size tiers ("quartile" or "fixed")
#' @param include_correlations Boolean whether to calculate polychoric correlations (default FALSE)
#' @param correlation_questions Vector of questions for correlation analysis (NULL for all)
#' @param min_responses Minimum responses for correlation analysis (default 30)
#' @return List containing analysis results
analyze_evaluations <- function(data, 
                               group_var, 
                               question_stem, 
                               top_n = 2,
                               conf_level = 0.95,
                               tier_method = "quartile",
                               include_correlations = FALSE,
                               correlation_questions = NULL,
                               min_responses = 30) {
  
  # 1. Aggregate by grouping variable
  aggregated <- aggregate_evaluations(data, group_var)
  
  # 2. Calculate proportions
  with_proportions <- calculate_top_n_proportion(aggregated, question_stem, top_n)
  
  # 3. Add Wilson CI
  with_ci <- add_wilson_ci(with_proportions, 
                          success_col = "top_n_count",
                          total_col = "total_responses",
                          conf_level = conf_level)
  
  # 4. Create sample size tiers
  with_tiers <- create_sample_tiers(with_ci, 
                                   sample_col = "totalStudents",
                                   method = tier_method)
  
  # 5. Calculate thresholds
  thresholds <- calculate_tier_thresholds(with_ci, "totalStudents", tier_method)
  
  # 6. Create summary by tier
  tier_summary <- summarize_by_tier(with_tiers, question_stem)
  
  # 7. Calculate polychoric correlations if requested
  correlations <- NULL
  if (include_correlations) {
    cat("Calculating polychoric correlations...\n")
    tryCatch({
      correlations <- calculate_polychoric_correlations(
        data = data,
        question_stems = correlation_questions,
        group_var = group_var,
        conf_level = conf_level,
        min_responses = min_responses
      )
    }, error = function(e) {
      warning(paste("Could not calculate correlations:", e$message))
      correlations <- NULL
    })
  }
  
  # 8. Return analysis object
  return(list(
    data = with_tiers,
    group_var = group_var,
    question = question_stem,
    top_n = top_n,
    thresholds = thresholds,
    summary = tier_summary,
    method = tier_method,
    conf_level = conf_level,
    correlations = correlations,
    include_correlations = include_correlations
  ))
}

#' Print a summary of analysis results
#' @param analysis_result Result object from analyze_evaluations()
print_evaluation_summary <- function(analysis_result) {
  cat("=== EVALUATION ANALYSIS SUMMARY ===\n\n")
  
  cat("Grouping Variable:", analysis_result$group_var, "\n")
  cat("Question Analyzed:", analysis_result$question, "\n")
  cat("Top N Levels:", analysis_result$top_n, "\n")
  cat("Confidence Level:", analysis_result$conf_level, "\n")
  cat("Tier Method:", analysis_result$method, "\n\n")
  
  if (analysis_result$method == "quartile") {
    cat("Sample Size Thresholds (Quartiles):\n")
    thresholds <- analysis_result$thresholds
    cat(sprintf("  25th percentile: %.0f responses\n", thresholds[1]))
    cat(sprintf("  50th percentile: %.0f responses\n", thresholds[2]))
    cat(sprintf("  75th percentile: %.0f responses\n", thresholds[3]))
  } else {
    cat("Sample Size Thresholds (Fixed):\n")
    thresholds <- analysis_result$thresholds
    for (i in 1:length(thresholds)) {
      cat(sprintf("  %s: %.0f\n", names(thresholds)[i], thresholds[i]))
    }
  }
  cat("\n")
  
  cat("Confidence Tier Distribution:\n")
  print(analysis_result$summary)
  cat("\n")
  
  # Show top performers in high confidence tier
  # Determine the dynamic column name based on the question stem
  question_stem <- analysis_result$question
  percent_col_name <- paste0("percent_top_n_", substr(question_stem, 1, 2))
  
  # Find the mean score column for this question
  mean_col_name <- paste0(question_stem, "_mean")
  
  # Check if the columns exist
  if (!percent_col_name %in% colnames(analysis_result$data)) {
    # Fallback to legacy name if dynamic name doesn't exist
    if ("percent_top_n" %in% colnames(analysis_result$data)) {
      percent_col_name <- "percent_top_n"
    } else {
      warning(paste("Neither", percent_col_name, "nor percent_top_n column found in data"))
      return(invisible(NULL))
    }
  }
  
  # Check if mean column exists
  has_mean_col <- mean_col_name %in% colnames(analysis_result$data)
  
  # Use dynamic column selection using tidy evaluation
  percent_sym <- rlang::sym(percent_col_name)
  
  # Create mean column symbol if it exists
  if (has_mean_col) {
    mean_sym <- rlang::sym(mean_col_name)
  }
  
  top_performers <- analysis_result$data %>%
    filter(confidence_tier == "High Confidence") %>%
    arrange(desc(!!percent_sym)) %>%
    slice(1:min(10, nrow(.)))
    
  # Select columns with dynamic name
  if (percent_col_name != "percent_top_n") {
    # Need to handle the dynamic column name in select
    if (has_mean_col) {
      top_performers <- top_performers %>%
        select(1, confidence_tier, !!percent_sym, !!mean_sym, ci_lower_pct, ci_upper_pct, totalStudents)
    } else {
      top_performers <- top_performers %>%
        select(1, confidence_tier, !!percent_sym, ci_lower_pct, ci_upper_pct, totalStudents)
    }
    # Rename the dynamic column to the generic name for consistent output
    names(top_performers)[names(top_performers) == percent_col_name] <- "percent_top_n"
  } else {
    if (has_mean_col) {
      top_performers <- top_performers %>%
        select(1, confidence_tier, percent_top_n, !!mean_sym, ci_lower_pct, ci_upper_pct, totalStudents)
    } else {
      top_performers <- top_performers %>%
        select(1, confidence_tier, percent_top_n, ci_lower_pct, ci_upper_pct, totalStudents)
    }
  }
  
  # Rename the mean column to a generic name if it exists
  if (has_mean_col) {
    names(top_performers)[names(top_performers) == mean_col_name] <- "mean_score"
  }
  
  if (nrow(top_performers) > 0) {
    cat("Top Performers (High Confidence Tier):\n")
    print(top_performers)
  }
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Quick instructor analysis using the pipeline
#' @param data Raw evaluation data
#' @param question_stem Question to analyze (default: overall instructor rating)
#' @param ... Additional arguments passed to analyze_evaluations
analyze_instructors <- function(data, question_stem = "Q6_overall_instructor_rating", ...) {
  analyze_evaluations(data, "instructor", question_stem, ...)
}

#' Quick subject analysis using the pipeline
#' @param data Raw evaluation data  
#' @param question_stem Question to analyze (default: overall instructor rating)
#' @param ... Additional arguments passed to analyze_evaluations
analyze_subjects <- function(data, question_stem = "Q6_overall_instructor_rating", ...) {
  analyze_evaluations(data, "subject", question_stem, ...)
}

#' Get all available questions for analysis
#' @param data Raw evaluation data
#' @return Vector of available question stems
get_available_questions <- function(data) {
  identify_ordinal_questions(data)
}

