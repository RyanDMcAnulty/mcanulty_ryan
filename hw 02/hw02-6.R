## Ryan McAnulty
## HW 02 Part 6: Lists

## 6.1 Introduction to Lists
## 6.2 Create a List
# Numeric vector with numerics from 1 to 6
numeric_vector <- 1:6

# Logical vector with TRUE and FALSE
# The`rep`function replicates objects you pass to it.
# In this case, you're replicating TRUE FALSE four times.
logical_vector <-rep(c(TRUE, FALSE), 4)

# letter Matrix with the first nine letters of the English alphabet
letter_matrix <-matrix(LETTERS[1:9], ncol = 3)

# First 10 elements of the chickwts data frame
chicks_df <- chickwts[1:10,]

# Use the`list()`function to create`the_list`with the above objects. Use the names of the objects to name the objects.
the_list <- list(numbers = numeric_vector, boolean = logical_vector, letters = letter_matrix, chicks = chicks_df)

# Display the contents of`the_list`
the_list

# Display the structure of 'the_list'
str(the_list)

## 6.3 Extract Elements from a List
# Add the line to create the horsebean vector
hb_chicks <- chickwts$weight[1:10]

# Add the line to create the linseed vector
ls_chicks <- chickwts$weight[11:20]

# Create a list object with the results of the t-test.
chicks_t <-t.test(hb_chicks, ls_chicks, var.equal = TRUE)

# Run the`str()`function on`chicks_t`
str(chicks_t)
# Display the numeric value of the`statistic`object from`chicks_t`
chicks_t$statistic

# Display the numeric value of the`p.value`object from`chicks_t`
chicks_t$p.value
# Display the lower confidence limit
chicks_t$conf.int[1]

# Display the upper confidence limit
chicks_t$conf.int[2]
# Create a text string called`Whew`with the value "I did it!"#
Whew <- "I did it!"
Whew