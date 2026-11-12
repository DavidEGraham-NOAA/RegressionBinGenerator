# David Graham 11/10/26
# R version 4.5.1
# Tidyverse 2.0.0 (dplyr 1.2.1, purrr 1.2.2)
# Go to line 194 for instructions on execution
library(tidyverse)


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
      mutate(min_measure = min_measure/rbg_env$measure_multiplier,
             max_measure = max_measure/rbg_env$measure_multiplier,
             mid_measure = (min_measure + max_measure) / 2)
    
    # Reorder the columns for clarity
    final_result <- final_result %>%
      relocate(mid_measure, .after = max_measure)
    
    
    return(final_result)
  }
}
