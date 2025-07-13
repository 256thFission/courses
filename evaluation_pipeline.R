# Modular Evaluation Analysis Pipeline
# Load required libraries
library(dplyr)
library(binom)
library(stringr)
library(polycor)
library(corrplot)
library(psych)

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
    data <- data %>%
      mutate(subject = str_extract(course_title, "^[A-Z]+"))
  }
  
  # Generic aggregation
  aggregated <- data %>%
    group_by(.data[[group_var]]) %>%
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
  
  results <- data %>%
    rowwise() %>%
    mutate(
      # Calculate response counts
      total_responses = sum(c_across(all_of(available_cols)), na.rm = TRUE),
      top_n_count = sum(c_across(all_of(top_n_cols)), na.rm = TRUE),
      
      # Calculate proportions
      proportion_top_n = ifelse(total_responses > 0, top_n_count / total_responses, NA_real_),
      percent_top_n = proportion_top_n * 100
    ) %>%
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
  
  # Store all the original column names to ensure we keep them
  original_cols <- colnames(data)
  
  result <- data %>%
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
    ) %>%
    # Only remove the temporary binom_result column but keep all other columns
    select(-binom_result)
  
  return(result)
}

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
        # Ensure data is properly formatted as ordered factors
        var1 <- factor(pair_data[, 1], levels = 1:5, ordered = TRUE)
        var2 <- factor(pair_data[, 2], levels = 1:5, ordered = TRUE)
        
        # Check if variables have sufficient variation
        if (length(unique(var1)) < 2 || length(unique(var2)) < 2) {
          stop("Insufficient variation in variables")
        }
        
        # Calculate polychoric correlation
        poly_cor <- polychor(var1, var2, std.err = TRUE)
        
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
  sig_matrix <- matrix("", nrow = nrow(p_matrix), ncol = ncol(p_matrix))
  rownames(sig_matrix) <- rownames(p_matrix)
  colnames(sig_matrix) <- colnames(p_matrix)
  
  # Apply significance levels element-wise
  sig_matrix[p_matrix < 0.001] <- "***"
  sig_matrix[p_matrix < 0.01 & p_matrix >= 0.001] <- "**"
  sig_matrix[p_matrix < 0.05 & p_matrix >= 0.01] <- "*"
  sig_matrix[p_matrix < 0.1 & p_matrix >= 0.05] <- "."
  sig_matrix[is.na(p_matrix)] <- ""
  
  # Set diagonal and make symmetric
  diag(sig_matrix) <- ""
  diag(p_matrix) <- 0
  
  return(list(
    significance = sig_matrix,
    p_values = p_matrix
  ))
}

# =============================================================================
# MODULE 6: CORRELATION VISUALIZATION
# =============================================================================

#' Create correlation plot with significance indicators
#' @param correlation_result Result from calculate_polychoric_correlations
#' @param method Visualization method ("circle", "square", "ellipse", "number")
#' @param title Plot title
#' @return Displays correlation plot
plot_polychoric_correlations <- function(correlation_result, 
                                        method = "circle",
                                        title = "Polychoric Correlations") {
  
  if (is.null(correlation_result$correlations)) {
    stop("No correlation matrix found in results")
  }
  
  # Create significance matrix for plotting
  sig_matrix <- correlation_result$significance$significance
  
  # Plot using corrplot
  corrplot(correlation_result$correlations,
           method = method,
           type = "upper",
           order = "hclust",
           title = title,
           mar = c(0, 0, 1, 0),
           tl.cex = 0.8,
           tl.col = "black",
           tl.srt = 45,
           # Add significance stars
           p.mat = correlation_result$p_values,
           sig.level = 0.05,
           insig = "label_sig",
           pch.cex = 0.8,
           pch.col = "black")
  
  # Add legend for significance levels
  legend("bottomright", 
         legend = c("p < 0.001 (***)", "p < 0.01 (**)", "p < 0.05 (*)", "p < 0.1 (.)"),
         bty = "n", cex = 0.7)
}

#' Create summary of significant correlations
#' @param correlation_result Result from calculate_polychoric_correlations
#' @param alpha Significance level threshold (default 0.05)
#' @return Data frame of significant correlations
summarize_significant_correlations <- function(correlation_result, alpha = 0.05) {
  
  corr_matrix <- correlation_result$correlations
  p_matrix <- correlation_result$p_values
  se_matrix <- correlation_result$std_errors
  ci_lower <- correlation_result$confidence_intervals$lower
  ci_upper <- correlation_result$confidence_intervals$upper
  
  # Get upper triangle indices
  upper_tri <- upper.tri(corr_matrix)
  
  # Create summary data frame
  summary_df <- data.frame(
    Question1 = rownames(corr_matrix)[row(corr_matrix)[upper_tri]],
    Question2 = colnames(corr_matrix)[col(corr_matrix)[upper_tri]],
    Correlation = corr_matrix[upper_tri],
    Std_Error = se_matrix[upper_tri],
    CI_Lower = ci_lower[upper_tri],
    CI_Upper = ci_upper[upper_tri],
    P_Value = p_matrix[upper_tri],
    Significant = p_matrix[upper_tri] < alpha,
    stringsAsFactors = FALSE
  )
  
  # Remove NA correlations and sort by significance
  summary_df <- summary_df[!is.na(summary_df$Correlation), ]
  summary_df <- summary_df[order(summary_df$P_Value), ]
  
  return(summary_df)
}

#' Print polychoric correlation summary
#' @param correlation_result Result from calculate_polychoric_correlations
print_correlation_summary <- function(correlation_result) {
  cat("=== POLYCHORIC CORRELATION ANALYSIS SUMMARY ===\n\n")
  
  cat("Questions analyzed:", length(correlation_result$question_stems), "\n")
  cat("Group variable:", ifelse(is.null(correlation_result$group_var), "None (overall)", correlation_result$group_var), "\n")
  cat("Confidence level:", correlation_result$conf_level, "\n")
  cat("Minimum responses threshold:", correlation_result$min_responses, "\n")
  cat("Total observations:", correlation_result$n_observations, "\n\n")
  
  cat("Sample sizes by question:\n")
  for (i in 1:length(correlation_result$sample_sizes)) {
    cat(sprintf("  %s: %d responses\n", 
                names(correlation_result$sample_sizes)[i], 
                correlation_result$sample_sizes[i]))
  }
  cat("\n")
  
  # Show significant correlations
  sig_corr <- summarize_significant_correlations(correlation_result)
  
  if (nrow(sig_corr) > 0) {
    cat("Significant correlations (p < 0.05):\n")
    print(sig_corr[sig_corr$Significant, c("Question1", "Question2", "Correlation", "P_Value")])
  } else {
    cat("No significant correlations found at p < 0.05 level.\n")
  }
  cat("\n")
  
  # Show strongest correlations regardless of significance
  cat("Strongest correlations (regardless of significance):\n")
  strongest <- sig_corr[order(abs(sig_corr$Correlation), decreasing = TRUE), ][1:min(5, nrow(sig_corr)), ]
  print(strongest[c("Question1", "Question2", "Correlation", "P_Value")])
}

# =============================================================================
# MODULE 7: MAIN ANALYSIS PIPELINE
# =============================================================================

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
  tier_summary <- summarize_by_tier(with_tiers)
  
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

#' Summarize results by confidence tier
#' @param data Data frame with tier assignments
#' @return Summary data frame
summarize_by_tier <- function(data) {
  data %>%
    group_by(confidence_tier) %>%
    summarise(
      n_groups = n(),
      mean_sample_size = mean(totalStudents, na.rm = TRUE),
      median_sample_size = median(totalStudents, na.rm = TRUE),
      mean_percent_top_n = mean(percent_top_n, na.rm = TRUE),
      median_percent_top_n = median(percent_top_n, na.rm = TRUE),
      mean_ci_width = mean(ci_width_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n_groups))
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
  top_performers <- analysis_result$data %>%
    filter(confidence_tier == "High Confidence") %>%
    arrange(desc(percent_top_n)) %>%
    slice(1:min(10, nrow(.))) %>%
    select(1, confidence_tier, percent_top_n, ci_lower_pct, ci_upper_pct, totalStudents)
  
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

#' Quick polychoric correlation analysis
#' @param data Raw evaluation data
#' @param question_stems Vector of question stems (NULL for all)
#' @param group_var Grouping variable (NULL for overall)
#' @param ... Additional arguments passed to calculate_polychoric_correlations
#' @return Correlation analysis results
analyze_correlations <- function(data, question_stems = NULL, group_var = NULL, ...) {
  calculate_polychoric_correlations(data, question_stems, group_var, ...)
}

#' Quick instructor correlation analysis
#' @param data Raw evaluation data
#' @param question_stems Vector of question stems (NULL for all)
#' @param ... Additional arguments passed to calculate_polychoric_correlations
#' @return Correlation analysis results for instructors
analyze_instructor_correlations <- function(data, question_stems = NULL, ...) {
  calculate_polychoric_correlations(data, question_stems, "instructor", ...)
}

#' Quick subject correlation analysis
#' @param data Raw evaluation data
#' @param question_stems Vector of question stems (NULL for all)
#' @param ... Additional arguments passed to calculate_polychoric_correlations
#' @return Correlation analysis results for subjects
analyze_subject_correlations <- function(data, question_stems = NULL, ...) {
  calculate_polychoric_correlations(data, question_stems, "subject", ...)
}