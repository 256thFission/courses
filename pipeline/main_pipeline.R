# =============================================================================
# MAIN EVALUATION ANALYSIS PIPELINE
# =============================================================================
# This file coordinates all pipeline modules and provides the main interface
# for evaluation analysis.

# Load required libraries
library(dplyr)
library(binom)
library(stringr)
library(polycor)
library(corrplot)
library(psych)

# Source all pipeline modules
source("pipeline/data_aggregation.R")
source("pipeline/ordinal_metrics.R") 
source("pipeline/statistical_analysis.R")
source("pipeline/tiering_and_summary.R")
source("pipeline/vizualization.R")
source("pipeline/export_functions.R")

# =============================================================================
# MAIN PIPELINE INTERFACE
# =============================================================================

#' Run complete evaluation analysis pipeline
#' @param data Raw evaluation data
#' @param group_var String name of grouping variable ("instructor", "subject", etc.)
#' @param question_stem String stem of question to analyze
#' @param top_n Integer number of top levels to include (default 2)
#' @param conf_level Confidence level for Wilson CI (default 0.95)
#' @param tier_method String method for sample size tiers ("quartile" or "fixed")
#' @param include_correlations Boolean whether to calculate polychoric correlations (default FALSE)
#' @param correlation_questions Vector of questions for correlation analysis (NULL for all)
#' @param min_responses Minimum responses for correlation analysis (default 30)
#' @param print_summary Boolean whether to print summary (default TRUE)
#' @param plot_correlations Boolean whether to plot correlations if calculated (default TRUE)
#' @return List containing complete analysis results
run_evaluation_pipeline <- function(data,
                                   group_var,
                                   question_stem,
                                   top_n = 2,
                                   conf_level = 0.95,
                                   tier_method = "quartile",
                                   include_correlations = FALSE,
                                   correlation_questions = NULL,
                                   min_responses = 30,
                                   print_summary = TRUE,
                                   plot_correlations = TRUE) {
  
  cat("=== STARTING EVALUATION ANALYSIS PIPELINE ===\n\n")
  
  # Run main analysis
  results <- analyze_evaluations(
    data = data,
    group_var = group_var,
    question_stem = question_stem,
    top_n = top_n,
    conf_level = conf_level,
    tier_method = tier_method,
    include_correlations = include_correlations,
    correlation_questions = correlation_questions,
    min_responses = min_responses
  )
  
  # Print summary if requested
  if (print_summary) {
    print_evaluation_summary(results)
    
    if (!is.null(results$correlations)) {
      cat("\n")
      print_correlation_summary(results$correlations)
    }
  }
  
  # Plot correlations if requested and available
  if (plot_correlations && !is.null(results$correlations)) {
    cat("\nGenerating correlation plot...\n")
    plot_polychoric_correlations(results$correlations)
  }
  
  cat("\n=== PIPELINE COMPLETE ===\n")
  
  return(results)
}

#' Quick instructor analysis
#' @param data Raw evaluation data
#' @param question_stem Question to analyze (default: overall instructor rating)
#' @param ... Additional arguments passed to run_evaluation_pipeline
run_instructor_analysis <- function(data, question_stem = "Q6_overall_instructor_rating", ...) {
  run_evaluation_pipeline(data, "instructor", question_stem, ...)
}

#' Quick subject analysis
#' @param data Raw evaluation data  
#' @param question_stem Question to analyze (default: overall instructor rating)
#' @param ... Additional arguments passed to run_evaluation_pipeline
run_subject_analysis <- function(data, question_stem = "Q6_overall_instructor_rating", ...) {
  run_evaluation_pipeline(data, "subject", question_stem, ...)
}

#' Show pipeline help
show_pipeline_help <- function() {
  cat("=== EVALUATION ANALYSIS PIPELINE HELP ===\n\n")
  
  cat("MAIN FUNCTIONS:\n")
  cat("• run_evaluation_pipeline() - Complete analysis pipeline\n")
  cat("• run_instructor_analysis() - Quick instructor analysis\n")
  cat("• run_subject_analysis()    - Quick subject analysis\n\n")
  
  cat("INDIVIDUAL MODULE FUNCTIONS:\n")
  cat("Data Aggregation:\n")
  cat("• aggregate_evaluations()   - Aggregate by grouping variable\n\n")
  
  cat("Ordinal Metrics:\n")
  cat("• calculate_top_n_proportion() - Calculate top-N proportions\n")
  cat("• identify_ordinal_questions() - Find ordinal questions in data\n\n")
  
  cat("Statistical Analysis:\n")
  cat("• add_wilson_ci()              - Add Wilson confidence intervals\n")
  cat("• calculate_polychoric_correlations() - Polychoric correlations\n\n")
  
  cat("Tiering and Summary:\n")
  cat("• create_sample_tiers()        - Create confidence tiers\n")
  cat("• analyze_evaluations()        - Main analysis function\n")
  cat("• print_evaluation_summary()   - Print analysis summary\n\n")
  
  cat("Visualization:\n")
  cat("• plot_polychoric_correlations() - Plot correlation matrix\n")
  cat("• analyze_correlations()         - Quick correlation analysis\n\n")
  
  cat("UTILITY FUNCTIONS:\n")
  cat("• get_available_questions()    - List available questions\n")
  cat("• show_pipeline_help()         - Show this help\n\n")
  
  cat("EXAMPLE USAGE:\n")
  cat("# Basic instructor analysis\n")
  cat("results <- run_instructor_analysis(my_data)\n\n")
  
  cat("# Analysis with correlations\n")
  cat("results <- run_evaluation_pipeline(\n")
  cat("  data = my_data,\n")
  cat("  group_var = 'instructor',\n")
  cat("  question_stem = 'Q6_overall_instructor_rating',\n")
  cat("  include_correlations = TRUE\n")
  cat(")\n\n")
}

# Print helpful message when pipeline is loaded
cat("Evaluation Analysis Pipeline loaded successfully!\n")
cat("Use show_pipeline_help() to see available functions.\n\n")