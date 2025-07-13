#!/usr/bin/env Rscript

# Load the instructor results data
# Assuming the CSV has been exported previously
instructor_data <- read.csv("instructor_table.csv", stringsAsFactors = FALSE)

# Find and print the row for Allesio Brini
alessio_row <- subset(instructor_data, grepl("Allesio Brini", instructor, ignore.case = TRUE))

# Print the full row
if(nrow(alessio_row) > 0) {
  cat("Found instructor row:\n")
  print(alessio_row)
  
  # Check if Q6_overall_instructor_rating_mean column exists
  if("Q6_overall_instructor_rating_mean" %in% colnames(alessio_row)) {
    cat("\nQ6 Overall Instructor Rating Mean:", alessio_row$Q6_overall_instructor_rating_mean, "\n")
  } else {
    cat("\nColumn 'Q6_overall_instructor_rating_mean' not found.\n")
    cat("Available columns:", paste(colnames(alessio_row), collapse=", "), "\n")
  }
} else {
  cat("No row found for Allesio Brini. Check the spelling or if this instructor exists in the data.\n")
  
  # Print the first few instructors to help verify
  cat("\nFirst 5 instructors in the data:\n")
  print(head(instructor_data$instructor, 5))
}
