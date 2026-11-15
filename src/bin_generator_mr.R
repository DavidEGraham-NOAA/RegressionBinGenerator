# David Graham 11/10/26
# R version 4.5.1
# Tidyverse 2.0.0 (dplyr 1.2.1, purrr 1.2.2)
library(tidyverse)

#Data and variable setup function
PrepareData <- function(baseframe, measure, measure_multiplier) {
  as_tibble(baseframe) |>
    mutate(
      act_measure = floor(measure_multiplier * .data[[measure]]),
      min_measure = 0,
      max_measure = 0,
      mid_measure = 0
    )
}

# Get all rows with the species code and create a new summarised df on which to operate
PrimaryGroupExtractor <- function(ana_frame, primary_group, primary_group_value) {
  group_frame <- ana_frame |>
    filter(.data[[primary_group]] == .env$primary_group_value)

  if (nrow(group_frame) == 0L) {
    return(
      group_frame |>
        select(all_of(primary_group), act_measure) |>
        mutate(n = integer(), obs = double(), pres_true = integer(), pres_false = integer())
    )
  }

  group_frame |>
    summarise(
      n = n(),
      obs = max(pres),
      pres_true = sum(pres == 1),
      pres_false = sum(pres == 0),
      .by = c(all_of(primary_group), act_measure)
    ) |>
    FillSparseValues(primary_group)
}

FillSparseValues <- function(operant_frame, primary_group) {
  if (nrow(operant_frame) == 0L) {
    return(operant_frame)
  }

  operant_frame |>
    complete(
      act_measure = seq_len(max(act_measure)),
      fill = list(n = 0L, obs = 0, pres_true = 0L, pres_false = 0L),
      explicit = FALSE
    ) |>
    fill(all_of(primary_group), .direction = "downup") |>
    arrange(act_measure)
}

ProcessBins <- function(processing_frame) {
  processing_frame |>
    mutate(min_measure = NA_real_, max_measure = NA_real_)
}

#Main entry point
RegressionBinGenerator <- function(baseframe, measure, bin_size, logit = FALSE, measure_increment = 1, primary_group = "species_cd", merge_final = FALSE, results_filename = NULL){
  if(!is.numeric(measure_increment)){
    stop(paste0("Input 'measure_increment' must be a numeric value. You entered '", measure_increment, "'."))
  }
  if (measure_increment <= 0) {
    stop(paste0("Input 'measure_increment' must be a non-zero, non-negative number. You entered ", measure_increment, "."))
  }
  
  measure_increment_dec <- GetDecimalPlaces(measure_increment)

  if(measure_increment_dec > 0 && measure_increment > 1){
    stop(paste0("Input 'measure_increment' must be between 0 and 1 if it contains a decimal value. You entered ", measure_increment, "."))
  }
  measure_multiplier <- CalculateIncrementMultiplier(measure_increment, measure_increment_dec)

  ana_frame <- PrepareData(baseframe, measure, measure_multiplier)
  group_names <- unique(ana_frame[[primary_group]])

  if (length(group_names) == 0L) {
    stop("No data to process due to null primary_group. Check to ensure that your primary_group variable contains values and is properly named.")
  }

  group_names |>
    map(function(primary_group_value) {
      PrimaryGroupExtractor(ana_frame, primary_group, primary_group_value) |>
        ProcessBins()
    }) |>
    list_rbind() |>
    as_tibble() |>
    mutate(
      min_measure = min_measure / measure_multiplier,
      max_measure = max_measure / measure_multiplier,
      mid_measure = (min_measure + max_measure) / 2
    ) |>
    relocate(mid_measure, .after = max_measure)
}


# Setup
baseframe <- read_csv("data/orig_test_data.csv", show_col_types = FALSE)

huh <- RegressionBinGenerator(baseframe, "DEPTH", 1, primary_group="species_cd")