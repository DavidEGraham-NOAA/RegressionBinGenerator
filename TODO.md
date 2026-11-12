# TODO:

1.  Need test files for edge cases
2.  Convert to package
3.  Merge ProcessDev.R and FinalResultGenerator.R logic to add new columns to original data and return that.
4.  Add column name in method call to id the column over which to sum.
5.  Make rugosity value a variable so it can be changed. This is tricky because of decimal places. Build a function to compute the number of decimal places in the function and then take 10 to the power of that number and it will give the variable/multiplier.
6.  Restructure this so that depth category and rugosity category can live peacefully together.
7.  Genericize min, max, & mid columns for use with depth & rugosity
8.  Add error handling/bounds checking in helper functions

# Questions

-Should package prompt for result save info or just return data.frame?

# Dev Project

1.  Add measure variable. - DONE
2. Add measure_increment.

# 11/11/26
1. Import new data to local database
2. Build queries for basic data understanding and stats - unique spp, presence/absence per spp, depth categories, etc
3. Test data prep steps
4. Non-logistic function
5. Logistic function
6. Dataframe merge
7. Final merge

# Needs
Good known test data with edge cases fully documented.