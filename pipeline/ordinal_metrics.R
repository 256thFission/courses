# Load required libraries
library(dplyr)

# =============================================================================
# MODULE 2: ORDINAL METRICS
# =============================================================================

#' Calculate proportions for ordinal questions (top N responses)
#' @param data Data frame with aggregated level counts
#' @param question_stem String stem of the question (e.g., "Q6_overall_instructor_rating")
#' @param top_n Integer number of top levels to include (default 2)
#' @return Data frame with proportion calculations added
calculate_top_n_proportion <- function(data, question_stem, top_n = 2) {
  
  # Determine available levels for this question
  level_cols <- paste0(question_stem, "_level_", 1:5)
  available_cols <- level_cols[level_cols %in% colnames(data)]
  
  if (length(available_cols) == 0) {
    stop(paste("No level columns found for question:", question_stem))
  }
  
  # Determine the top N levels (assuming 5-point scale, top N are the highest)
  max_level <- length(available_cols)
  top_levels <- (max_level - top_n + 1):max_level
  top_n_cols <- paste0(question_stem, "_level_", top_levels)
  top_n_cols <- top_n_cols[top_n_cols %in% available_cols]
  
  # Create the new column names
  proportion_col_name <- paste0("proportion_top_n_", substr(question_stem, 1, 2))
  percent_col_name <- paste0("percent_top_n_", substr(question_stem, 1, 2))
  
  results <- data |>
    rowwise() |>
    mutate(
      # Calculate response counts
      total_responses = sum(c_across(all_of(available_cols)), na.rm = TRUE),
      top_n_count = sum(c_across(all_of(top_n_cols)), na.rm = TRUE),
      
      # Calculate proportions
      !!proportion_col_name := ifelse(total_responses > 0, top_n_count / total_responses, NA_real_),
      !!percent_col_name := .data[[proportion_col_name]] * 100
    ) |>
    ungroup()
  
  return(results)
}

#' Identify all ordinal questions in the dataset
#' @param data Data frame to examine
#' @return Vector of question stems found in the data
identify_ordinal_questions <- function(data) {
  level_cols <- colnames(data)[grepl("_level_\\d+$", colnames(data))]
  question_stems <- unique(sub("_level_\\d+$", "", level_cols))
  return(question_stems)
}

