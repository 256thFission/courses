# Load required libraries
library(dplyr)
library(stringr)

# =============================================================================
# MODULE 1: DATA AGGREGATOR
# =============================================================================

#' Generic aggregation by any single grouping variable
#' @param data Data frame containing evaluation data
#' @param group_var String name of the grouping variable (e.g., "instructor", "subject")
#' @return Aggregated data frame with standardized structure
aggregate_evaluations <- function(data, group_var) {
  
  # Validate inputs
  if (!group_var %in% colnames(data)) {
    stop(paste("Grouping variable", group_var, "not found in data"))
  }
  
  # Handle special case for subject extraction
  if (group_var == "subject" && !"subject" %in% colnames(data)) {
    data <- data |>
      mutate(subject = str_extract(course_title, "^[A-Z]+"))
  }
  
  # Generic aggregation
  aggregated <- data |>
    group_by(.data[[group_var]]) |>
    summarise(
      # Sum all level counts across classes
      across(contains("_level_"), \(x) sum(x, na.rm = TRUE)),

      # Class and student counts
      numClasses = n(),
      totalStudents = sum(ClassSize, na.rm = TRUE),
      avg_class_size = totalStudents / numClasses,
      
      # Average the means and rates (these are already averaged per class)
      across(ends_with("_mean") | ends_with("Rate"), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
      
    )
  
  return(aggregated)
}

