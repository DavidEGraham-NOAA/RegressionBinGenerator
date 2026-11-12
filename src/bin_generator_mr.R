# David Graham 11/10/26
# R version 4.5.1
# Tidyverse 2.0.0 (dplyr 1.2.1, purrr 1.2.2)
# Go to line 194 for instructions on execution
library(tidyverse)

#Data and variable setup function
PrepareData <- function() {
  ana_frame <-
    baseframe %>%
    mutate("act_measure" = floor(measure_multiplier * baseframe[[measure]]), 
           "min_measure" = 0, 
           "max_measure" = 0, 
           "mid_measure" = 0)
  
  group_names <- unique(ana_frame[[primary_group]])
  
  if(length(group_names) < 1){
    #free objects and memory
    #rm(rbg_env)
    #gc()
    #Message user on having no data to process
    stop("No data to process due to null primary_group. Check to ensure that your primary_group variable contains values and is properly named.")
  }
}

# Get all rows with the species code and create a new summarised df on which to operate
PrimaryGroupExtractor <- function(primary_group_value) {
  intermed_frame <- filter(ana_frame, !!sym(primary_group) == primary_group_value) %>%
    group_by(!!sym(primary_group), act_measure) %>%
    summarise(
      n = n(),
      obs = max(pres),
      pres_true = sum(ifelse(pres == 1, 1, 0)),
      pres_false = sum(ifelse(pres == 0, 1, 0))
    ) |>
    arrange(act_measure)
  
  operant_frame <- as.data.frame(intermed_frame) 
  operant_frame <- FillSparseValues(operant_frame)
  return(operant_frame)
  
}

FillSparseValues <- function(operant_frame) {
  # Find missing values for the series defined by measure
  # Must make sure this is an integer series from 1 to n
  # PrimaryGroupExtractor (pge) has already grouped and ordered the rows
  # Now generate a series from 0 to max(pge$act_measure), find the disjoint set, then add those rows 
  # to pge data frame
  max_val <- max(operant_frame$act_measure)
  all_vals <- seq(from = 1, to = max_val, by = 1)
  sdiff <- setdiff(all_vals, operant_frame$act_measure)
  
  # Add an empty row to the data frame for each missing value defined in sdiff
  for (i in 1:length(sdiff)){
    operant_frame <- operant_frame %>% add_row(group_name = operant_frame$group_name[1], 
                                               act_measure = sdiff[i], 
                                               n = 0, 
                                               obs = 0, 
                                               pres_true = 0, 
                                               pres_false = 0)
    
    #print(sdiff[i])
  }
  
  operant_frame <- operant_frame %>% arrange(act_measure)
  return(operant_frame)
  
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
  result <- data.frame()
  #final_result <- data.frame()

  if(measure_increment_dec > 0 && measure_increment > 1){
    stop(paste0("Input 'measure_increment' must be between 0 and 1 if it contains a decimal value. You entered ", measure_increment, "."))
  }
  #print(primary_group)
  measure_multiplier <- CalculateIncrementMultiplier(measure_increment, measure_increment_dec)
  
  p <- PrepareData()
  
  
  if (length(group_names) > 0) {
    final_result <- data.frame(
      group_name = character(0),
      observations = double(0),
      min_measure = double(0),
      max_measure = double(0),
      pos_observations = double(0),
      neg_observations = double(0)
    )
    
    
    # Loop through the group names and calculate results for each one
    for (i in group_names) {
      # i == group_names, normally species code
      the_frame <- PrimaryGroupExtractor(i)
      final_result <- union(final_result, ProcessBins(the_frame))
      
      # Empty the_frame so we have a clean slate for subsquent loops
      the_frame <- the_frame[0, ]
    }

    # Add a final midpoint column that is the mid between min_measure and max_measure
    # and then convert back to the original base 
    # The midpoint calculation happens after the min and max columns have been adjusted, hence no need to divide by the multiplier as it has already been done
    final_result <- final_result %>%
      mutate(min_measure = min_measure/measure_multiplier,
             max_measure = max_measure/measure_multiplier,
             mid_measure = (min_measure + max_measure) / 2)
    
    # Reorder the columns for clarity
    final_result <- final_result %>%
      relocate(mid_measure, .after = max_measure)
    
    
    return(final_result)
  }
}


# Setup
baseframe <- read.csv("data/test_data_basic.csv", header = TRUE)
baseframe <- read.csv("data/orig_test_data.csv", header = TRUE)

primary_group <- "species_cd"
measure <- "DEPTH"
measure_increment <- 1
measure_increment_dec <- 0
measure_multiplier <- 1
group_names <- c()

huh <- RegressionBinGenerator(baseframe, "DEPTH", 1, primary_group="species_cd")