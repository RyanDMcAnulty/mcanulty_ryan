
### Function 1: Standard Error of the Mean
## The first argument accepts a vector of numeric values
## The second argument tells whether to remove NA. Defaults to False.
std_err <- function(vector, na.rm = FALSE) {
  ifelse (na.rm,                      # Test: TRUE (yes), FALSE (no)
      vector <- na.omit(vector),      # If test is true, remove NAs from vector 
      vector)                         # If test is false keep NAs in
  (sd(vector) / sqrt(length(vector))) # This does the calculation for the standard error of the mean
}
