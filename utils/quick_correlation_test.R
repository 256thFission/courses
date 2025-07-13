# Quick test and fix for correlation analysis
# Source the main pipeline first
source("evaluation_pipeline.R")

# Quick diagnostic function
quick_correlation_test <- function(data, min_responses = 20) {
  
  cat("=== QUICK CORRELATION DIAGNOSTIC ===\n\n")
  
  # Check data structure
  cat("Data dimensions:", dim(data), "\n")
  cat("Sample column names:", paste(head(colnames(data), 10), collapse = ", "), "\n")
  
  # Find ordinal questions
  questions <- identify_ordinal_questions(data)
  cat("\nFound", length(questions), "ordinal questions\n")
  
  if (length(questions) >= 2) {
    cat("First few questions:", paste(head(questions, 3), collapse = ", "), "\n")
    
    # Try with minimal approach - just first 2 questions
    test_questions <- questions[1:2]
    
    cat("\nTesting with:", paste(test_questions, collapse = ", "), "\n")
    
    # Try the correlation analysis with very low threshold
    tryCatch({
      result <- analyze_correlations(data, 
                                   question_stems = test_questions,
                                   min_responses = min_responses)
      
      cat("SUCCESS! Correlation analysis completed.\n")
      cat("Correlation between first two questions:\n")
      print(result$correlations[1:2, 1:2])
      
      return(result)
      
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
      
      # Try the fixed version
      cat("\nTrying alternative approach...\n")
      source("correlation_fix.R")
      
      tryCatch({
        result_fixed <- calculate_polychoric_correlations_fixed(
          data, 
          question_stems = test_questions,
          min_responses = min_responses
        )
        
        cat("Alternative approach SUCCESS!\n")
        print_correlation_summary_fixed(result_fixed)
        return(result_fixed)
        
      }, error = function(e2) {
        cat("Alternative approach also failed:", e2$message, "\n")
        return(NULL)
      })
    })
  } else {
    cat("Insufficient questions for correlation analysis\n")
    return(NULL)
  }
}

# Run the test
cat("Quick correlation test function loaded.\n")
cat("Run: quick_correlation_test(your_data)\n")
cat("Try with different min_responses if needed: quick_correlation_test(your_data, min_responses = 10)\n")