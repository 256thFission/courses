# Test Script for Modular Evaluation Pipeline
# This script demonstrates how to use the new modular pipeline

# Load the pipeline
source("evaluation_pipeline.R")

# Test with sample data (you'll need to load your actual data)
# For demonstration, let's show how it would work:

cat("=== MODULAR EVALUATION PIPELINE TEST ===\n\n")

# Example usage patterns:

cat("1. BASIC USAGE EXAMPLES:\n")
cat("   # Load your data\n")
cat("   eval_df <- read.csv('data/all.csv')\n\n")

cat("   # Analyze by instructor (default question)\n") 
cat("   instructor_analysis <- analyze_instructors(eval_df)\n")
cat("   print_evaluation_summary(instructor_analysis)\n\n")

cat("   # Analyze by subject\n")
cat("   subject_analysis <- analyze_subjects(eval_df)\n")
cat("   print_evaluation_summary(subject_analysis)\n\n")

cat("   # Custom analysis - semester grouping, different question\n")
cat("   semester_analysis <- analyze_evaluations(\n")
cat("     eval_df,\n") 
cat("     group_var = 'semester',\n")
cat("     question_stem = 'Q7_course_quality'\n")
cat("   )\n\n")

cat("2. ADVANCED USAGE EXAMPLES:\n")
cat("   # Different confidence level and top N\n")
cat("   dept_analysis <- analyze_evaluations(\n")
cat("     eval_df,\n")
cat("     group_var = 'department',\n") 
cat("     question_stem = 'Q8_workload_appropriate',\n")
cat("     top_n = 3,  # Top 3 levels instead of top 2\n")
cat("     conf_level = 0.99,  # 99% confidence intervals\n")
cat("     tier_method = 'fixed'  # Fixed thresholds instead of quartiles\n")
cat("   )\n\n")

cat("3. EXPLORING AVAILABLE QUESTIONS:\n")
cat("   # See what questions are available for analysis\n")
cat("   available_questions <- get_available_questions(eval_df)\n")
cat("   print(available_questions)\n\n")

cat("4. ACCESSING RESULTS:\n")
cat("   # The analysis object contains:\n")
cat("   # - analysis$data: Full results with all calculations\n")
cat("   # - analysis$summary: Summary by confidence tier\n") 
cat("   # - analysis$thresholds: Sample size thresholds used\n")
cat("   # - analysis$group_var: Grouping variable used\n")
cat("   # - analysis$question: Question analyzed\n\n")

cat("   # Example: Get high confidence instructors\n")
cat("   high_conf <- instructor_analysis$data %>%\n")
cat("     filter(confidence_tier == 'High Confidence') %>%\n")
cat("     arrange(desc(percent_top_n))\n\n")

cat("5. MODULAR USAGE (using individual functions):\n")
cat("   # Step-by-step analysis for custom workflows\n")
cat("   \n")
cat("   # 1. Aggregate by custom grouping\n")
cat("   aggregated <- aggregate_evaluations(eval_df, 'instructor')\n")
cat("   \n")
cat("   # 2. Calculate proportions for specific question\n") 
cat("   with_props <- calculate_top_n_proportion(\n")
cat("     aggregated, 'Q6_overall_instructor_rating', top_n = 2\n")
cat("   )\n")
cat("   \n")
cat("   # 3. Add confidence intervals\n")
cat("   with_ci <- add_wilson_ci(\n")
cat("     with_props, 'top_n_count', 'total_responses'\n")
cat("   )\n")
cat("   \n") 
cat("   # 4. Create tiers\n")
cat("   final_data <- create_sample_tiers(\n")
cat("     with_ci, 'total_responses', method = 'quartile'\n")
cat("   )\n\n")

cat("6. COMPARISON OF DIFFERENT GROUPINGS:\n")
cat("   # Easily compare the same question across different groupings\n")
cat("   question <- 'Q6_overall_instructor_rating'\n")
cat("   \n")
cat("   instructor_results <- analyze_evaluations(eval_df, 'instructor', question)\n")
cat("   subject_results <- analyze_evaluations(eval_df, 'subject', question)\n")
cat("   dept_results <- analyze_evaluations(eval_df, 'department', question)\n")
cat("   \n")
cat("   # Compare tier distributions\n")
cat("   print('Instructor Analysis:')\n")
cat("   print_evaluation_summary(instructor_results)\n")
cat("   \n")
cat("   print('Subject Analysis:')\n")
cat("   print_evaluation_summary(subject_results)\n\n")

cat("=== PIPELINE READY FOR USE ===\n")
cat("Load your data and run the examples above!\n")