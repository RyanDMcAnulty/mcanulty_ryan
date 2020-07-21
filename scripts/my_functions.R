#### Function 1: Standard Error function
## The first argument accepts a vector of numeric values
## The second argument tells whether to remove NA. Defaults to False.
std_err <- function(vector, na.rm = FALSE) { 
  ifelse (na.rm,                      # Test: TRUE (yes), FALSE (no)
      vector <- na.omit(vector),      # If test is true, remove NAs from vector 
      vector)                         # If test is false keep NAs in
  (sd(vector) / sqrt(length(vector))) # This does the calculation for the standard error of the mean: 
                                      # (Standard Deviation)/(Square root of n, sample size)
}

#### Function 2: Scaled Mass Index function
## The first argument, mass, is mass of each individual bird
## The second argument, tarsus, is the length of the tarsus 
##    (part of the leg) of each individual bird
## The third argument, slope, is the slope estimate of the regression
##    line on the log-transformed mass and log-transformed tarsus length.
## All three arguments are defaulted to 0.
scaled_mass <- function(mass = FALSE, tarsus = FALSE, slope = FALSE) {
  ifelse (mass >= 0,   # Test: TRUE or FALSE
      mass <- mass,    # If test is true, keep mass as is
      mass <-0)       # If test is false, set mass to 0
  ifelse (tarsus >= 0, # Same test on tarsus length  
      tarsus <- tarsus,
      tarsus <-0)
  ifelse (slope >= 0,  # Same test on slope value  
      slope <- slope, 
      slope <-0)
  mass * (((mean(tarsus)) / tarsus) ^ slope) # This does the calculations for the scaled_mass
}