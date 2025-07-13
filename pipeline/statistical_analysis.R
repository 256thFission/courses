# Load required libraries
library(dplyr)
library(binom)
library(polycor)

# =============================================================================
# MODULE 3: WILSON CI CALCULATOR
# =============================================================================

#' Add Wilson confidence intervals to proportion data
#' @param data Data frame with success and total counts
#' @param success_col String name of success count column
#' @param total_col String name of total count column
#' @param conf_level Confidence level (default 0.95)
#' @return Data frame with confidence intervals added
add_wilson_ci <- function(data, success_col, total_col, conf_level = 0.95) {
  
  # Validate columns exist
  if (!success_col %in% colnames(data)) {
    stop(paste("Success column", success_col, "not found in data"))
  }
  if (!total_col %in% colnames(data)) {
    stop(paste("Total column", total_col, "not found in data"))
  }
  
  result <- data |>
    mutate(
      # Calculate Wilson confidence intervals using binom package
      binom_result = map2(.data[[success_col]], .data[[total_col]], 
                          ~if(.y == 0 || is.na(.y) || is.na(.x)) {
                            data.frame(lower = NA, upper = NA)
                          } else {
                            binom.confint(.x, .y, conf.level = conf_level, methods = "wilson")
                          }),
      
      # Extract confidence interval bounds
      ci_lower = map_dbl(binom_result, ~.x$lower),
      ci_upper = map_dbl(binom_result, ~.x$upper),
      ci_width = ci_upper - ci_lower,
      wilson_center = (ci_lower + ci_upper) / 2,
      
      # Convert to percentages
      ci_lower_pct = ci_lower * 100,
      ci_upper_pct = ci_upper * 100,
      ci_width_pct = ci_width * 100,
      wilson_center_pct = wilson_center * 100,
      
      # Precision weight (inverse of variance, using CI width as proxy)
      precision_weight = ifelse(ci_width > 0, 1 / (ci_width^2), 0)
    ) |>
    select(-binom_result)
  
  return(result)
}

# =============================================================================
# MODULE 5: POLYCHORIC CORRELATION ANALYSIS
# =============================================================================

#' Calculate polychoric correlations between ordinal questions
#' @param data Data frame with aggregated level counts or raw data
#' @param question_stems Vector of question stems to correlate (NULL for all)
#' @param group_var String name of grouping variable (NULL for overall analysis)
#' @param conf_level Confidence level for correlation CIs (default 0.95)
#' @param min_responses Minimum total responses to include a question (default 30)
#' @return List containing correlation matrix, significance tests, and metadata
calculate_polychoric_correlations <- function(data, 
                                             question_stems = NULL,
                                             group_var = NULL,
                                             conf_level = 0.95,
                                             min_responses = 30) {
  
  # If group_var specified, aggregate first
  if (!is.null(group_var)) {
    cat("Aggregating data by", group_var, "...\n")
    data <- aggregate_evaluations(data, group_var)
  }
  
  # Identify available ordinal questions
  all_questions <- identify_ordinal_questions(data)
  cat("Found", length(all_questions), "ordinal questions in data\n")
  
  # Use specified questions or all available
  if (is.null(question_stems)) {
    question_stems <- all_questions
  } else {
    # Validate specified questions exist
    missing_questions <- setdiff(question_stems, all_questions)
    if (length(missing_questions) > 0) {
      warning(paste("Questions not found in data:", paste(missing_questions, collapse = ", ")))
      question_stems <- intersect(question_stems, all_questions)
    }
  }
  
  if (length(question_stems) < 2) {
    stop("Need at least 2 ordinal questions for correlation analysis")
  }
  
  cat("Attempting correlation analysis with", length(question_stems), "questions\n")
  
  # Prepare data for polychoric correlation
  tryCatch({
    correlation_data <- prepare_correlation_data(data, question_stems, min_responses)
  }, error = function(e) {
    stop(paste("Error preparing correlation data:", e$message))
  })
  
  if (length(correlation_data$valid_questions) < 2) {
    stop(paste("Insufficient valid questions after filtering. Found:", 
               length(correlation_data$valid_questions), 
               "Try reducing min_responses."))
  }
  
  cat("Data prepared for", length(correlation_data$valid_questions), "questions\n")
  cat("Questions:", paste(correlation_data$valid_questions, collapse = ", "), "\n")
  
  # Calculate polychoric correlations
  cat("Calculating polychoric correlations...\n")
  
  tryCatch({
    # Use polychor for pairwise correlations with error handling
    poly_result <- calculate_polychoric_matrix(correlation_data$prepared_data, conf_level)
    
    # Create significance matrix
    sig_matrix <- create_significance_matrix(poly_result$correlations, poly_result$std_errors, conf_level)
    
    # Return comprehensive results
    return(list(
      correlations = poly_result$correlations,
      std_errors = poly_result$std_errors,
      confidence_intervals = poly_result$confidence_intervals,
      significance = sig_matrix,
      p_values = sig_matrix$p_values,
      question_stems = correlation_data$valid_questions,
      sample_sizes = correlation_data$sample_sizes,
      group_var = group_var,
      conf_level = conf_level,
      min_responses = min_responses,
      n_observations = nrow(correlation_data$prepared_data)
    ))
  }, error = function(e) {
    cat("Detailed error information:\n")
    cat("- Data dimensions:", dim(correlation_data$prepared_data), "\n")
    cat("- Column names:", paste(colnames(correlation_data$prepared_data), collapse = ", "), "\n")
    cat("- Valid questions:", paste(correlation_data$valid_questions, collapse = ", "), "\n")
    stop(paste("Error calculating polychoric correlations:", e$message))
  })
}

#' Prepare data for polychoric correlation analysis
#' @param data Aggregated data frame
#' @param question_stems Vector of question stems
#' @param min_responses Minimum responses threshold
#' @return List with prepared data and metadata
prepare_correlation_data <- function(data, question_stems, min_responses) {
  
  valid_questions <- c()
  sample_sizes <- c()
  all_responses_list <- list()
  
  # First pass: collect all valid questions and their responses
  for (question in question_stems) {
    # Get level columns for this question
    level_cols <- paste0(question, "_level_", 1:5)
    available_cols <- level_cols[level_cols %in% colnames(data)]
    
    if (length(available_cols) == 0) next
    
    # Calculate total responses for this question
    total_responses <- rowSums(data[available_cols], na.rm = TRUE)
    
    # Check if question meets minimum response threshold
    if (sum(total_responses, na.rm = TRUE) < min_responses) {
      cat(paste("Skipping", question, "- insufficient responses\n"))
      next
    }
    
    # Convert aggregated counts to individual responses
    question_responses <- expand_aggregated_responses(data, available_cols)
    
    if (length(question_responses) < min_responses) next
    
    # Store results
    valid_questions <- c(valid_questions, question)
    sample_sizes <- c(sample_sizes, length(question_responses))
    all_responses_list[[question]] <- question_responses
  }
  
  # Second pass: create properly aligned data frame
  if (length(valid_questions) == 0) {
    return(list(
      prepared_data = data.frame(),
      valid_questions = character(0),
      sample_sizes = numeric(0)
    ))
  }
  
  # Find maximum length
  max_length <- max(sapply(all_responses_list, length))
  
  # Create data frame with proper alignment
  prepared_data <- data.frame(row.names = 1:max_length)
  
  for (question in valid_questions) {
    responses <- all_responses_list[[question]]
    # Pad with NA if needed
    if (length(responses) < max_length) {
      responses <- c(responses, rep(NA, max_length - length(responses)))
    }
    prepared_data[[question]] <- responses
  }
  
  # Remove rows with all NA
  prepared_data <- prepared_data[rowSums(!is.na(prepared_data)) > 0, , drop = FALSE]
  
  names(sample_sizes) <- valid_questions
  
  return(list(
    prepared_data = prepared_data,
    valid_questions = valid_questions,
    sample_sizes = sample_sizes
  ))
}

#' Expand aggregated level counts to individual responses
#' @param data Data frame with level counts
#' @param level_cols Vector of level column names
#' @return Vector of individual responses
expand_aggregated_responses <- function(data, level_cols) {
  all_responses <- c()
  
  # Ensure level_cols exist in data
  available_cols <- level_cols[level_cols %in% colnames(data)]
  
  if (length(available_cols) == 0) {
    return(numeric(0))
  }
  
  for (i in 1:nrow(data)) {
    for (col in available_cols) {
      # Extract level number from column name
      level_num <- as.numeric(sub(".*_level_(\\d+)$", "\\1", col))
      count <- data[i, col]
      
      if (!is.na(count) && !is.na(level_num) && count > 0) {
        all_responses <- c(all_responses, rep(level_num, count))
      }
    }
  }
  
  return(all_responses)
}

#' Calculate polychoric correlation matrix with error handling
#' @param data Data frame with ordinal variables
#' @param conf_level Confidence level
#' @return List with correlations, standard errors, and CIs
calculate_polychoric_matrix <- function(data, conf_level) {
  n_vars <- ncol(data)
  var_names <- colnames(data)
  
  # Initialize matrices
  corr_matrix <- matrix(1, n_vars, n_vars)
  se_matrix <- matrix(0, n_vars, n_vars)
  ci_lower <- matrix(NA, n_vars, n_vars)
  ci_upper <- matrix(NA, n_vars, n_vars)
  
  rownames(corr_matrix) <- colnames(corr_matrix) <- var_names
  rownames(se_matrix) <- colnames(se_matrix) <- var_names
  rownames(ci_lower) <- colnames(ci_lower) <- var_names
  rownames(ci_upper) <- colnames(ci_upper) <- var_names
  
  # Calculate pairwise correlations
  for (i in 1:(n_vars-1)) {
    for (j in (i+1):n_vars) {
      
      # Get complete cases for this pair
      pair_data <- data[complete.cases(data[, c(i, j)]), c(i, j)]
      
      if (nrow(pair_data) < 10) {  # Minimum for polychoric
        corr_matrix[i, j] <- corr_matrix[j, i] <- NA
        se_matrix[i, j] <- se_matrix[j, i] <- NA
        next
      }
      
      tryCatch({
        # Calculate polychoric correlation
        poly_cor <- polychor(pair_data[, 1], pair_data[, 2], std.err = TRUE)
        
        corr_matrix[i, j] <- corr_matrix[j, i] <- poly_cor$rho
        se_matrix[i, j] <- se_matrix[j, i] <- poly_cor$var^0.5
        
        # Calculate confidence intervals
        z_score <- qnorm(1 - (1 - conf_level) / 2)
        ci_lower[i, j] <- ci_lower[j, i] <- poly_cor$rho - z_score * se_matrix[i, j]
        ci_upper[i, j] <- ci_upper[j, i] <- poly_cor$rho + z_score * se_matrix[i, j]
        
      }, error = function(e) {
        cat(paste("Warning: Could not calculate correlation between", 
                  var_names[i], "and", var_names[j], "-", e$message, "\n"))
        corr_matrix[i, j] <- corr_matrix[j, i] <- NA
        se_matrix[i, j] <- se_matrix[j, i] <- NA
      })
    }
  }
  
  return(list(
    correlations = corr_matrix,
    std_errors = se_matrix,
    confidence_intervals = list(lower = ci_lower, upper = ci_upper)
  ))
}

#' Create significance matrix for correlations
#' @param corr_matrix Correlation matrix
#' @param se_matrix Standard error matrix  
#' @param conf_level Confidence level
#' @return List with significance indicators and p-values
create_significance_matrix <- function(corr_matrix, se_matrix, conf_level) {
  
  # Calculate z-scores and p-values
  z_matrix <- corr_matrix / se_matrix
  p_matrix <- 2 * (1 - pnorm(abs(z_matrix)))
  
  # Create significance indicators
  alpha <- 1 - conf_level
  sig_matrix <- case_when(
    p_matrix < 0.001 ~ "***",
    p_matrix < 0.01 ~ "**", 
    p_matrix < 0.05 ~ "*",
    p_matrix < 0.1 ~ ".",
    TRUE ~ ""
  )
  
  # Set diagonal and make symmetric
  diag(sig_matrix) <- ""
  diag(p_matrix) <- 0
  
  return(list(
    significance = sig_matrix,
    p_values = p_matrix
  ))
}

