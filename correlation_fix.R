# Alternative approach for polychoric correlations
# This addresses the data structure issues by using a different method

# Load required libraries
library(dplyr)
library(polycor)

#' Alternative polychoric correlation calculation using weighted approach
#' @param data Aggregated evaluation data
#' @param question_stems Vector of question stems
#' @param group_var Grouping variable (NULL for overall)
#' @param min_responses Minimum responses threshold
#' @return Correlation results
calculate_polychoric_correlations_fixed <- function(data, 
                                                   question_stems = NULL,
                                                   group_var = NULL,
                                                   min_responses = 30) {
  
  # If group_var specified, aggregate first
  if (!is.null(group_var)) {
    cat("Aggregating data by", group_var, "...\n")
    data <- aggregate_evaluations(data, group_var)
  }
  
  # Identify available questions
  all_questions <- identify_ordinal_questions(data)
  cat("Found", length(all_questions), "ordinal questions in data\n")
  
  if (is.null(question_stems)) {
    question_stems <- all_questions
  } else {
    question_stems <- intersect(question_stems, all_questions)
  }
  
  if (length(question_stems) < 2) {
    stop("Need at least 2 ordinal questions for correlation analysis")
  }
  
  # Alternative approach: Use contingency tables for polychoric correlations
  cat("Using contingency table approach for polychoric correlations...\n")
  
  # Filter questions by minimum responses
  valid_questions <- c()
  for (q in question_stems) {
    level_cols <- paste0(q, "_level_", 1:5)
    available_cols <- level_cols[level_cols %in% colnames(data)]
    
    if (length(available_cols) > 0) {
      total_resp <- sum(rowSums(data[available_cols], na.rm = TRUE), na.rm = TRUE)
      if (total_resp >= min_responses) {
        valid_questions <- c(valid_questions, q)
      } else {
        cat("Skipping", q, "- only", total_resp, "responses\n")
      }
    }
  }
  
  if (length(valid_questions) < 2) {
    stop(paste("Only", length(valid_questions), "questions meet minimum response threshold"))
  }
  
  cat("Proceeding with", length(valid_questions), "questions\n")
  
  # Create correlation matrix
  n_q <- length(valid_questions)
  cor_matrix <- matrix(1, n_q, n_q)
  se_matrix <- matrix(0, n_q, n_q)
  p_matrix <- matrix(0, n_q, n_q)
  
  rownames(cor_matrix) <- colnames(cor_matrix) <- valid_questions
  rownames(se_matrix) <- colnames(se_matrix) <- valid_questions
  rownames(p_matrix) <- colnames(p_matrix) <- valid_questions
  
  # Calculate pairwise correlations using contingency tables
  for (i in 1:(n_q-1)) {
    for (j in (i+1):n_q) {
      
      q1 <- valid_questions[i]
      q2 <- valid_questions[j]
      
      tryCatch({
        # Create contingency table
        cont_table <- create_contingency_table(data, q1, q2)
        
        if (sum(cont_table) >= 20) {  # Minimum for polychoric
          # Calculate polychoric correlation from contingency table
          poly_result <- polychor(cont_table, std.err = TRUE)
          
          cor_matrix[i, j] <- cor_matrix[j, i] <- poly_result$rho
          se_matrix[i, j] <- se_matrix[j, i] <- sqrt(poly_result$var)
          
          # Calculate p-value
          z_score <- poly_result$rho / sqrt(poly_result$var)
          p_val <- 2 * (1 - pnorm(abs(z_score)))
          p_matrix[i, j] <- p_matrix[j, i] <- p_val
          
        } else {
          cat("Warning: Insufficient data for", q1, "vs", q2, "\n")
          cor_matrix[i, j] <- cor_matrix[j, i] <- NA
          se_matrix[i, j] <- se_matrix[j, i] <- NA
          p_matrix[i, j] <- p_matrix[j, i] <- NA
        }
        
      }, error = function(e) {
        cat("Error calculating correlation between", q1, "and", q2, ":", e$message, "\n")
        cor_matrix[i, j] <- cor_matrix[j, i] <- NA
        se_matrix[i, j] <- se_matrix[j, i] <- NA
        p_matrix[i, j] <- p_matrix[j, i] <- NA
      })
    }
  }
  
  # Return results
  return(list(
    correlations = cor_matrix,
    std_errors = se_matrix,
    p_values = p_matrix,
    question_stems = valid_questions,
    n_questions = length(valid_questions),
    method = "contingency_table"
  ))
}

#' Create contingency table from aggregated level counts
#' @param data Aggregated data
#' @param q1 First question stem
#' @param q2 Second question stem
#' @return Contingency table
create_contingency_table <- function(data, q1, q2) {
  
  # Get level columns for both questions
  q1_cols <- paste0(q1, "_level_", 1:5)
  q2_cols <- paste0(q2, "_level_", 1:5)
  
  q1_available <- q1_cols[q1_cols %in% colnames(data)]
  q2_available <- q2_cols[q2_cols %in% colnames(data)]
  
  if (length(q1_available) == 0 || length(q2_available) == 0) {
    return(matrix(0, 5, 5))
  }
  
  # Create a simple approximation contingency table
  # This assumes responses are distributed proportionally
  cont_table <- matrix(0, length(q1_available), length(q2_available))
  
  for (row in 1:nrow(data)) {
    q1_counts <- as.numeric(data[row, q1_available])
    q2_counts <- as.numeric(data[row, q2_available])
    
    # Skip if any NA values
    if (any(is.na(q1_counts)) || any(is.na(q2_counts))) next
    
    total_q1 <- sum(q1_counts)
    total_q2 <- sum(q2_counts)
    
    if (total_q1 > 0 && total_q2 > 0) {
      # Proportional allocation (simplified approach)
      for (i in 1:length(q1_counts)) {
        for (j in 1:length(q2_counts)) {
          # Assume joint distribution proportional to marginal distributions
          expected_joint <- (q1_counts[i] * q2_counts[j]) / max(total_q1, total_q2)
          cont_table[i, j] <- cont_table[i, j] + expected_joint
        }
      }
    }
  }
  
  # Ensure minimum counts
  cont_table <- pmax(cont_table, 0.1)
  
  return(cont_table)
}

#' Simple summary function for fixed correlations
print_correlation_summary_fixed <- function(correlation_result) {
  cat("=== POLYCHORIC CORRELATION ANALYSIS (FIXED) ===\n\n")
  
  cat("Questions analyzed:", length(correlation_result$question_stems), "\n")
  cat("Method:", correlation_result$method, "\n\n")
  
  cat("Questions:\n")
  for (i in 1:length(correlation_result$question_stems)) {
    cat(sprintf("%d. %s\n", i, correlation_result$question_stems[i]))
  }
  
  cat("\nCorrelation Matrix:\n")
  print(round(correlation_result$correlations, 3))
  
  cat("\nP-values:\n")
  print(round(correlation_result$p_values, 3))
  
  # Show significant correlations
  sig_pairs <- which(correlation_result$p_values < 0.05 & 
                     upper.tri(correlation_result$p_values), arr.ind = TRUE)
  
  if (nrow(sig_pairs) > 0) {
    cat("\nSignificant correlations (p < 0.05):\n")
    for (k in 1:nrow(sig_pairs)) {
      i <- sig_pairs[k, 1]
      j <- sig_pairs[k, 2]
      cat(sprintf("%s - %s: r = %.3f, p = %.3f\n",
                  correlation_result$question_stems[i],
                  correlation_result$question_stems[j],
                  correlation_result$correlations[i, j],
                  correlation_result$p_values[i, j]))
    }
  } else {
    cat("\nNo significant correlations found.\n")
  }
}

cat("Fixed correlation functions loaded.\n")
cat("Use: calculate_polychoric_correlations_fixed(data, min_responses = 20)\n")