# Evaluation Analysis Pipeline

A modular R pipeline for analyzing course evaluation data with statistical rigor and confidence-based tiering.

## Overview

This pipeline transforms raw course evaluation data into actionable insights by:
- Aggregating responses by instructor, subject, or other grouping variables
- Calculating confidence intervals using Wilson's method
- Creating sample-size based confidence tiers
- Computing polychoric correlations between ordinal questions
- Providing comprehensive visualizations and summaries

## Pipeline Structure

The pipeline is organized into focused modules:

```
pipeline/
├── main_pipeline.R          # Main interface and coordination
├── data_aggregation.R       # Data aggregation functions
├── ordinal_metrics.R        # Ordinal response calculations
├── statistical_analysis.R   # Statistical methods (Wilson CI, correlations)
├── tiering_and_summary.R    # Confidence tiers and summary functions
└── vizualization.R          # Plotting and visualization functions
```

## Quick Start

### Basic Usage

```r
# Load the pipeline
source("pipeline/main_pipeline.R")

# Run instructor analysis
results <- run_instructor_analysis(my_data)

# Run subject analysis  
results <- run_subject_analysis(my_data)

# Custom analysis
results <- run_evaluation_pipeline(
  data = my_data,
  group_var = "instructor",
  question_stem = "Q6_overall_instructor_rating",
  include_correlations = TRUE
)
```

### Getting Help

```r
# Show available functions and usage examples
show_pipeline_help()

# List available questions in your dataset
available_questions <- get_available_questions(my_data)
```

## Module Details

### 1. Data Aggregation (`data_aggregation.R`)

**Key Function:** `aggregate_evaluations(data, group_var)`

Aggregates individual responses by grouping variable (instructor, subject, etc.):
- Sums level counts across classes
- Calculates class and student counts
- Averages means and rates
- Handles subject extraction from course titles

### 2. Ordinal Metrics (`ordinal_metrics.R`)

**Key Functions:**
- `calculate_top_n_proportion(data, question_stem, top_n = 2)` - Calculates proportions of top N responses
- `identify_ordinal_questions(data)` - Finds all ordinal questions in dataset

Handles 5-point Likert scale analysis with flexible top-N calculations.

### 3. Statistical Analysis (`statistical_analysis.R`)

**Key Functions:**
- `add_wilson_ci(data, success_col, total_col, conf_level = 0.95)` - Adds Wilson confidence intervals
- `calculate_polychoric_correlations(...)` - Computes polychoric correlations between ordinal variables

Features:
- Wilson confidence intervals for robust proportion estimates
- Polychoric correlations designed for ordinal data
- Comprehensive error handling and validation
- Significance testing with multiple comparison awareness

### 4. Tiering and Summary (`tiering_and_summary.R`)

**Key Functions:**
- `create_sample_tiers(data, sample_col, method = "quartile")` - Creates confidence tiers
- `analyze_evaluations(...)` - Main analysis orchestrator
- `print_evaluation_summary(analysis_result)` - Comprehensive result summaries

**Confidence Tiers:**
- **High Confidence**: Top quartile (or >100 responses for fixed method)
- **Moderate Confidence**: 50th-75th percentile (or 50-100 responses)
- **Low Confidence**: 25th-50th percentile (or 25-50 responses)  
- **Preliminary Only**: 10+ responses but below low tier
- **Insufficient Data**: <10 responses

### 5. Visualization (`vizualization.R`)

**Key Functions:**
- `plot_polychoric_correlations(correlation_result)` - Correlation matrix plots
- `summarize_significant_correlations(correlation_result)` - Tabular correlation summaries
- `print_correlation_summary(correlation_result)` - Comprehensive correlation reporting

Features hierarchical clustering, significance indicators, and publication-ready plots.

## Analysis Workflow

### Standard Analysis Steps

1. **Data Aggregation**: Raw responses → grouped summaries
2. **Proportion Calculation**: Level counts → top-N proportions  
3. **Confidence Intervals**: Wilson method for robust estimates
4. **Confidence Tiering**: Sample-size based reliability categories
5. **Summary Generation**: Tier-based performance summaries
6. **Correlation Analysis** (optional): Inter-question relationships
7. **Visualization** (optional): Correlation matrices and plots

### Example Complete Analysis

```r
# Load your data
my_data <- read.csv("evaluation_data.csv")

# Run comprehensive instructor analysis
results <- run_evaluation_pipeline(
  data = my_data,
  group_var = "instructor",
  question_stem = "Q6_overall_instructor_rating",
  top_n = 2,                    # Top 2 rating levels (4-5 on 5-point scale)
  conf_level = 0.95,            # 95% confidence intervals
  tier_method = "quartile",     # Quartile-based tiers
  include_correlations = TRUE,  # Calculate correlations
  min_responses = 30            # Minimum responses for correlation
)

# Access results
results$data              # Full dataset with tiers and CIs
results$summary           # Summary by confidence tier
results$correlations      # Correlation analysis (if requested)
```

## Key Features

### Statistical Rigor
- **Wilson Confidence Intervals**: More accurate than normal approximation, especially for small samples
- **Polychoric Correlations**: Appropriate for ordinal data, accounts for underlying continuous variables
- **Sample Size Awareness**: Explicit confidence tiers based on response counts

### Flexibility
- **Multiple Grouping Variables**: Instructor, subject, course, department, etc.
- **Configurable Questions**: Any ordinal question in your dataset
- **Adaptive Tiering**: Quartile-based (data-driven) or fixed thresholds
- **Selective Analysis**: Include only the analyses you need

### Robust Error Handling
- Input validation at every stage
- Graceful handling of missing data
- Informative error messages and warnings
- Fallback options for edge cases

## Data Requirements

### Input Data Format
Your data should contain:
- **Grouping variables**: instructor, subject, course_title, etc.
- **Level counts**: `[question]_level_1`, `[question]_level_2`, etc.
- **Class information**: ClassSize or similar sample size indicators
- **Optional**: Pre-computed means and rates

### Example Data Structure
```
instructor | course_title | ClassSize | Q6_level_1 | Q6_level_2 | Q6_level_3 | Q6_level_4 | Q6_level_5
-----------|--------------|-----------|------------|------------|------------|------------|------------
Smith_J    | MATH 101     | 25        | 1          | 2          | 5          | 10         | 7
Johnson_M  | PHYS 201     | 30        | 0          | 1          | 3          | 12         | 14
```

## Interpretation Guide

### Confidence Tiers
- **High Confidence**: Results are statistically robust, suitable for high-stakes decisions
- **Moderate Confidence**: Results are reliable for most purposes, minor caution advised
- **Low Confidence**: Results provide useful insights but should be interpreted carefully
- **Preliminary Only**: Results are suggestive but require more data for firm conclusions
- **Insufficient Data**: Results are unreliable and should not be used for decisions

### Wilson Confidence Intervals
- Narrower intervals indicate more precise estimates
- Intervals that don't overlap suggest statistically meaningful differences
- Use interval width as a measure of estimate reliability

### Polychoric Correlations
- Range from -1 to +1, like Pearson correlations
- Account for the ordinal nature of rating scales
- Significance testing helps identify meaningful relationships
- Hierarchical clustering in plots groups related questions

## Dependencies

Required R packages:
- `dplyr` - Data manipulation
- `binom` - Wilson confidence intervals  
- `stringr` - String processing
- `polycor` - Polychoric correlations
- `corrplot` - Correlation visualization
- `psych` - Psychological statistics

Install with: `install.packages(c("dplyr", "binom", "stringr", "polycor", "corrplot", "psych"))`


### Performance Optimization
- Use `include_correlations = FALSE` for faster analysis when correlations aren't needed


## Citation

- Wilson, E.B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association.
- Olsson, U. (1979). Maximum likelihood estimation of the polychoric correlation coefficient. Psychometrika.