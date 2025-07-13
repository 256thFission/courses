# Load required libraries
library(dplyr)
library(corrplot)

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
# CONVENIENCE CORRELATION FUNCTIONS
# =============================================================================

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