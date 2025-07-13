#!/usr/bin/env Rscript

# Test script to debug correlation analysis
source("evaluation_pipeline.R")

# Load your data (adjust path as needed)
# If you have a specific data file, load it here
# data <- read.csv("your_data_file.csv")

# Or if you're using sta_data from your environment
# Make sure sta_data is available

cat("Starting correlation analysis debug test...\n")

# Test with your 4 questions
questions <- c("Q3_intellectually_stimulating", "Q5_overall_course_quality", 
               "Q6_overall_instructor_rating", "Q8_course_difficulty")

# Run the analysis that was failing
tryCatch({
  result <- calculate_polychoric_correlations(sta_data, questions, min_responses = 30)
  cat("SUCCESS: Analysis completed!\n")
  print(result$correlations)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("Full error details:\n")
  print(e)
})