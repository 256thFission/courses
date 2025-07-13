# Master Data Preparation Script
# This script runs all data preparation steps in the correct order
# to go from raw data to the final clean dataset

# Load required libraries
library(tidyverse)
library(ggplot2)
library(DT)

# Source the pipeline functions
source("pipeline/main_pipeline.R")

cat("=== Starting Data Preparation Pipeline ===\n")

# Step 1: Combine individual department CSV files into all.csv
cat("Step 1: Combining raw department evaluation CSV files...\n")

# Get file paths for all department evaluation files
file_list <- list.files(path = "data", pattern = "_evaluations\\.csv$", full.names = TRUE)
cat(paste("Found", length(file_list), "department files to combine\n"))

# Initialize list to store data frames
results_list <- list()

# Process each file
for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Extract subject code from filename
  period_identifier <- gsub("_evaluations", "", file_name)
  period_identifier <- gsub(".csv", "", period_identifier)
  
  # Read the data
  df <- read.csv(file_path)
  
  # Add subject column
  df$subject <- period_identifier
  
  # Add to results list
  results_list[[file_path]] <- df
}

# Combine all data frames
final_df <- do.call(rbind, results_list)

# Write combined data
write.csv(final_df, "./data/all.csv", row.names = FALSE)
cat("Combined data written to data/all.csv\n")

# Step 2: Process course codes and create enriched dataset
cat("Step 2: Processing course codes and creating enriched dataset...\n")

# Read the combined data and course codes
df <- read.csv("data/all.csv")
codes <- read.csv("data/all_codes.csv")

# Extract course codes from course titles
main <- extract_course_code(df, course_title)

# Get unique course codes
unique_codes <- codes %>%
  distinct(course_code, .keep_all = TRUE)

# Remove redundant subject column before joining
unique_codes_slimmer <- select(unique_codes, -subject)

# Join with course codes
combine <- left_join(main, unique_codes_slimmer, by = "course_code")

# Write enriched dataset
write.csv(combine, "data/all_combine.csv", row.names = FALSE)
cat("Enriched data written to data/all_combine.csv\n")

# Step 3: Aggregate by course codes and prepare final dataset
cat("Step 3: Aggregating by course codes and preparing final dataset...\n")

# Aggregate evaluations by course code
combine <- combine %>%
  aggregate_evaluations("course_code")

# Prepare the final table
prepped <- table_prep(combine)

# Write final prepared dataset
write.csv(prepped, "data/treqs.csv", row.names = FALSE)
cat("Final prepared data written to data/treqs.csv\n")

cat("=== Data Preparation Complete ===\n")
cat("Final datasets available:\n")
cat("  - data/all.csv: Combined raw evaluation data\n")
cat("  - data/all_combine.csv: Data enriched with course codes\n")
cat("  - data/treqs.csv: Final aggregated and prepared dataset\n")