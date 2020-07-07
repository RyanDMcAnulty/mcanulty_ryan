## Ryan McAnulty
## HW 02 Part 5: Data Frames

## 5.1 Data Frames
## 5.2 Viewing Data Frames
# Use`data()`to load the`iris`data frame.
data("iris")

# Enter`iris`on a line by itself to display the full data frame.
iris

# Display the first 10 rows of the data frame.
head(iris, n = 10)

# Display the last rows of the data frame.
tail(iris)

# Display the dimensions using the least amount of code (9 characters).
dim(iris)

# Display the structure of the data frame.#
str(iris)

## 5.3 Extracting Elements from a Data Frame
# Display the 101st row of the`Petal.Length`column, using column numbers.
iris[101, 3]

# Display the first six rows of all columns (mimic head ())
iris[1:6,]

# Display rows 48-52 of the fourth column, using the column header name in square brackets.
iris[48:52,"Petal.Width"]

# Display the contents of the`Sepal.Width`column using the`$`
iris$Sepal.Width

## 5.4 Extracting Elements with Boolean Vectors
# Extract rows where sepal length less than or equal to 5.5. Save the result.
sepal_short <- iris$Sepal.Length <=5.5
sepal_short_rows <- iris[sepal_short,]
sepal_short_rows
# Apply the`min()`and`max()`functions to your result from above.
min(sepal_short_rows$Sepal.Length)
max(sepal_short_rows$Sepal.Length)

# Display rows where sepal width is less than 3.2 AND species is setosa.
iris[iris$Sepal.Width <3.2 & iris$Species == "setosa",]

# Display rows where sepal width is less than 2.5 OR petal width is greater than 2.0.
iris[iris$Sepal.Width <2.5 | iris$Petal.Width >2.0,]

## 5.5 Use Subset to Extract Data From a Data Frame
# Display rows for petal length between and including 4.0 and 5.0.
subset(iris, Petal.Length >= 4.0 & Petal.Length<= 5.0)

# Display rows for sepal length < 5.2 and species is versicolor.
subset(iris, Sepal.Length < 5.2 & Species == "versicolor")

## 5.6 Sort
# Order the data frame from shortest to longest sepal length.
ordered_sepal_length <- order(iris$Sepal.Length)
iris[ordered_sepal_length,]

# Display the species and petal width columns in decreasing order of petal width.#
decreasing_petal_width <- order(iris$Petal.Width)
decreasing_petal_width_species <- iris[decreasing_petal_width,] 
decreasing_petal_width_species[,c("Species", "Petal.Width")]