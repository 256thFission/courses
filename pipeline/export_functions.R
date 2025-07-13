# =============================================================================
# EVALUATION PIPELINE EXPORT FUNCTIONS
# =============================================================================

library(dplyr)
library(tidyr)

#' Export pipeline results to CSV
#' @param results Results object from analyze_evaluations or run_evaluation_pipeline
#' @param file_path Path to save the CSV file
#' @param include_summary Whether to include the tier summary (default FALSE)
#' @return Invisibly returns the exported data frame
export_results_to_csv <- function(results, file_path, include_summary = FALSE) {
  
  # Check that results is the expected type
  if (!is.list(results) || is.null(results$data)) {
    stop("Results must be an analysis result object from analyze_evaluations or run_evaluation_pipeline")
  }
  
  # Extract the main data (this is the complete data with all columns)
  export_data <- results$data
  
  if (include_summary) {
    # If summary requested, add a section with the tier summary data
    # Create a separator row
    separator <- data.frame(matrix(NA, nrow = 2, ncol = ncol(export_data)))
    names(separator) <- names(export_data)
    separator[1, 1] <- "====== SUMMARY BY CONFIDENCE TIER ======"
    
    # Convert summary to compatible data frame
    summary_data <- results$summary
    # Need to pad with NA columns to match main data structure
    missing_cols <- setdiff(names(export_data), names(summary_data))
    for (col in missing_cols) {
      summary_data[[col]] <- NA
    }
    
    # Combine with proper column ordering
    summary_data <- summary_data[, names(export_data)]
    
    # Combine all parts
    export_data <- rbind(export_data, separator, summary_data)
  }
  
  # Ensure column names are database-friendly
  # Replace spaces and special characters with underscores
  names(export_data) <- gsub("[\\s\\-\\.]+", "_", names(export_data))
  
  # Write to CSV with safe options
  write.csv(export_data, file_path, row.names = FALSE, na = "", quote = TRUE)
  
  cat("Results exported to", file_path, "\n")
  return(invisible(export_data))
}

#' Find the most common subject for each instructor
#' @param data Raw evaluation data containing instructor and subject columns
#' @return A data frame with instructor and their most common subject
get_instructor_subjects <- function(data) {
  # Check if required columns exist
  if (!all(c("instructor", "subject") %in% colnames(data))) {
    # If subject column doesn't exist but course_title does, extract subject from course_title
    if ("instructor" %in% colnames(data) && "course_title" %in% colnames(data)) {
      data <- data |>
        mutate(subject = stringr::str_extract(course_title, "^[A-Z]+"))
    } else {
      stop("Data must contain 'instructor' and either 'subject' or 'course_title' columns")
    }
  }
  
  # Group by instructor and find most common subject
  instructor_subjects <- data |>
    group_by(instructor, subject) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(instructor) |>
    arrange(desc(count), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    select(instructor, primary_subject = subject)
  
  return(instructor_subjects)
}

#' Export instructor results to CSV with added subject information
#' @param results Results from analyze_instructors or run_instructor_analysis
#' @param raw_data Original raw data for finding subject information
#' @param file_path Optional file path, defaults to "instructor_results.csv"
#' @param include_summary Whether to include the tier summary
#' @param include_subject Whether to include the most common subject for each instructor
#' @return Invisibly returns the exported data frame
export_instructor_results <- function(results, 
                                     raw_data = NULL,
                                     file_path = "instructor_results.csv",
                                     include_summary = FALSE,
                                     include_subject = TRUE) {
  # Extract data from results
  export_data <- results$data
  
  # Fix column names for Q6 metrics if they don't exist
  # Check if we're working with Q6 data but missing the specific column
  if (results$question == "Q6_overall_instructor_rating" && 
      !"percent_top_n_Q6" %in% colnames(export_data) && 
      "percent_top_n" %in% colnames(export_data)) {
    export_data$percent_top_n_Q6 <- export_data$percent_top_n
  }
  
  # Ensure Q6 mean column exists
  q6_mean_col <- "Q6_overall_instructor_rating_mean"
  if (!q6_mean_col %in% colnames(export_data) && 
      grepl("Q6", results$question)) {
    # Try to find similar columns that might have the data
    possible_cols <- grep("Q6.*mean|mean.*Q6", colnames(export_data), value = TRUE)
    if (length(possible_cols) > 0) {
      export_data[[q6_mean_col]] <- export_data[[possible_cols[1]]]
    }
  }
  
  # Add subject information if requested and raw_data is provided
  if (include_subject && !is.null(raw_data)) {
    # Get instructor subjects
    instructor_subjects <- get_instructor_subjects(raw_data)
    
    # Join with results data
    export_data <- export_data |>
      left_join(instructor_subjects, by = "instructor")
    
    # Move the primary_subject column to be right after instructor
    cols <- colnames(export_data)
    instructor_idx <- which(cols == "instructor")
    new_order <- c(
      cols[1:instructor_idx],
      "primary_subject",
      cols[(instructor_idx+1):length(cols)][cols[(instructor_idx+1):length(cols)] != "primary_subject"]
    )
    export_data <- export_data[, new_order]
    
    # Replace results$data with the enhanced data
    results$data <- export_data
  } else if (include_subject && is.null(raw_data)) {
    warning("Cannot add subject information: raw_data is NULL")
  }
  
  # Use the standard export function
  export_results_to_csv(results, file_path, include_summary)
}

#' Export subject results to CSV (convenience function)
#' @param results Results from analyze_subjects or run_subject_analysis
#' @param file_path Optional file path, defaults to "subject_results.csv"
#' @param include_summary Whether to include the tier summary
#' @return Invisibly returns the exported data frame
export_subject_results <- function(results, 
                                  file_path = "subject_results.csv",
                                  include_summary = FALSE) {
  export_results_to_csv(results, file_path, include_summary)
}

