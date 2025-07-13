library(stringr)
extract_course_code <- function(df, col_name) {
  df |>
    mutate(
      # Apply str_extract to the whole column passed in 'col_name'
      course_code = str_extract({{col_name}}, pattern = "^[^-]*-[^-]*")
    )
}




table_prep <- function(df){
  available_questions <- get_available_questions(df)
  temp<-df
  for (s in available_questions) {
    temp <- calculate_top_n_proportion(temp,s,2)
  } 
  temp<- select(temp,course_code,course_title,subject,ends_with("mean"),starts_with("percent"),avg_class_size,ALP,CZ,NS,QS,SS, CCI, EI, STS, FL, R,W)
}


name_courses <- function(df) {
  
  # --- Step 1: Drop the 'x' column if it exists ---
  # This checks for a column named "x" and removes it.
  if ("x" %in% names(df)) {
    df <- select(df, -x)
  }

  rename_map <- c(
    "code" = "course_code",
    "title" = "course_title",
    "Stimulating Avg" = "Q3_intellectually_stimulating_mean",
    "OverallQuality Avg" = "Q5_overall_course_quality_mean",
    "Instructor Avg" = "Q6_overall_instructor_rating_mean",
    "Difficulty Avg" = "Q8_course_difficulty_mean",
    "Hrs Avg" = "Q9_hours_outside_mean",
    "% Rated Interesting" = "percent_top_n_Q3",
    "% Rated High Quality" = "percent_top_n_Q5",
    "% Rated Instructor Good" = "percent_top_n_Q6",
    "% Rated Hard" = "percent_top_n_Q8",
    "class_size" = "avg_class_size"
  )
  
  

  df <- df |>
    rename(any_of(rename_map))
  
  # Return the cleaned dataframe
  return(df)
}
