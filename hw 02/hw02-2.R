# Ryan McAnulty
# HW 02 Part 2: Vectors
## 2.1 Create a vector

## 2.2 Vector Types
# Create two vectors, one of alphabet and one of integers
alphabet <- c("A", "B", "C", "D", "E", "F")

integers <- c(1, 2, 3, 4, 5, 6)

#Logical vectors
logical_vector <- c(FALSE, FALSE, TRUE)

## 2.3 Biological vectors

# Cultures without pplo contaminant
cultures_without_pplo <- c(4.6, 4.8, 5.1, 5.5, 5.8)

# Cultures with pplo contaminant
cultures_with_pplo <- c(4.6, 4.7, 4.8, 4.9, 4.8)

## 2.4 Name Your Vectors
# Create a 'days_sampled' vector.
days_sampled <- c("Day 0", "Day 2", "Day 4", "Day 6", "Day 8")

# Name your two data vectors with the 'day_sampled' vector

names(cultures_without_pplo) <- days_sampled
names(cultures_with_pplo) <- days_sampled

# Check that your two data vectors were properly named.
cultures_without_pplo
cultures_with_pplo

## 2.5 Calculations with Vectors

#Calculate what is the max of cultures_without_pplo and cultures_with_pplo
max(cultures_without_pplo)
max(cultures_with_pplo)

# Store results in max_without_pplo and max_with_pplo
max_without_pplo <- max(cultures_without_pplo)
max_with_pplo <- max(cultures_with_pplo)

#Calculate what is the min of cultures_without_pplo and cultures_with_pplo
min(cultures_without_pplo)
min(cultures_with_pplo)

# Store results in min_without_pplo and min_with_pplo
min_without_pplo <- min(cultures_without_pplo)
min_with_pplo <- min(cultures_with_pplo)

# Use 10^ to calculate the actual number of cells for each culture.
10^(cultures_without_pplo)
10^(cultures_with_pplo)

# Store in cell_counts_without_pplo and cell_counts_with_pplo
cell_counts_without_pplo <-10^(cultures_without_pplo)
cell_counts_with_pplo <- 10^(cultures_with_pplo)

# Calculate the average number of cell counts for each vector
mean(cell_counts_without_pplo)
mean(cell_counts_with_pplo)

## 2.6 Extract Individual Cell Elements From a Vector
# Select the third element from cultures_without_pplo by position number
cultures_without_pplo[3]


# Select the odd numbered elements of cell_counts_with_pplo using a vector of position numbers.
cell_counts_with_pplo[c(1,3,5)]


# Select the elements for "Day 2" and "Day 4" by name from cultures_with_pplo
cultures_with_pplo[c("Day 2", "Day 4")]

## 2.7 Extraction by Logical Comparison
# Use `cell_counts_without_pplo` to create a logical vector for cell counts greater than 100000.
without_greater_than_or_equal_to_100000 <- cell_counts_without_pplo>=100000

# Use that vector to show the days and log values from `cultures_without_pplo`.
cell_counts_without_pplo[without_greater_than_or_equal_to_100000]

# Use `cell_counts_with_pplo` and `&` to create a logical vector for cells counts greater than 50,000 and less than 75,000.
with_between_50000_and_75000 <- cell_counts_with_pplo > 50000 & cell_counts_with_pplo < 75000

# Use that logical vector to show the days and log values from `cultures_with_pplo`.
cell_counts_with_pplo[with_between_50000_and_75000]

###