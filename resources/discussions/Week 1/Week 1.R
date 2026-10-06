# -----------------------------------------------------------------------------
# Part 0. Getting around -------------------------------------------------------
# -----------------------------------------------------------------------------
# RStudio panes: Source (scripts), Console (runs code), Environment (objects),
# Files / Plots / Help.
# Anything after "#" is a comment and is not run.

?mean              # open the help page for a function
example(mean)      # run the examples from the help page
getwd()            # which folder R is currently working in


# -----------------------------------------------------------------------------
# Part 1. Value and variable --------------------------------------------------
# -----------------------------------------------------------------------------
# Arithmetic
2 + 3
10 / 4
2 ^ 10
sqrt(81)
log(100, base = 10)    # functions take named arguments

# Variable
x <- 5                 # "<-" assigns a value to a name (read it as "gets")
y <- x * 2
y

# Basic data types
class(3.14)            # "numeric"
class("hello")         # "character"
class(TRUE)            # "logical"
class(2L)              # "integer" (L makes it a whole number)

# Question: What does 5 == 5.0 return? What about "5" == 5?


# -----------------------------------------------------------------------------
# Part 2. Vectors and logic ---------------------------------------------------
# -----------------------------------------------------------------------------
scores <- c(72, 85, 90, 64, 78, 95)      # c() combines values into a vector
length(scores)
mean(scores); median(scores); sd(scores)

# Vectorisation
scores + 5
scores / 100

# Indexing
scores[1]                   # the first element (R counts from 1)
scores[2:4]                 # a slice
scores[-1]                  # everything except the first element

# Logical filtering
scores > 80                 # a TRUE/FALSE vector
scores[scores > 80]         # keep only the values that are TRUE
sum(scores > 80)            # TRUE counts as 1, so this counts the values
mean(scores > 80)           # proportion above 80

# Handy sequences
1:10
seq(0, 1, by = 0.25)
rep("A", 3)

# Question: How would you get the scores between 70 and 90 (inclusive)?


# -----------------------------------------------------------------------------
# Part 3. Functions and simple control flow -----------------------------------
# -----------------------------------------------------------------------------
to_z <- function(v) {
  (v - mean(v)) / sd(v)          # the last line is returned automatically
}
round(to_z(scores), 2)

grade <- function(s) {
  if (s >= 90) {
    "A"
  } else if (s >= 80) {
    "B"
  } else {
    "C or below"
  }
}
grade(85)

# For loop
for (s in scores) {
  cat(s, "->", grade(s), "\n")
}

# A vectorised equivalence
sapply(scores, grade)            # apply grade() to every element


# -----------------------------------------------------------------------------
# Part 4. Data frames ---------------------------------------------------------
# -----------------------------------------------------------------------------
# mtcars: 32 cars from a 1974 magazine (mpg = miles per gallon,
# cyl = number of cylinders, wt = weight in 1000 lb, am = 0 automatic / 1 manual)
head(mtcars)           # first six rows
str(mtcars)            # structure: size, column names, types
summary(mtcars)        # quick summary of every column
dim(mtcars); nrow(mtcars); names(mtcars)

# Picking columns and rows
mtcars$mpg                           # one column, as a vector
mtcars[1:3, c("mpg", "cyl", "wt")]   # data[rows, columns]
mtcars[mtcars$mpg > 25, ]            # rows that meet a condition
subset(mtcars, cyl == 4 & am == 1, select = c(mpg, wt))   # easier to read

# Adding a new column
mtcars2 <- mtcars                              # work on a copy
mtcars2$kpl <- mtcars2$mpg * 0.425             # kilometres per litre
mtcars2$transmission <- ifelse(mtcars2$am == 1, "manual", "automatic")
head(mtcars2[, c("mpg", "kpl", "transmission")])

# Sorting
head(mtcars2[order(mtcars2$mpg, decreasing = TRUE), c("mpg", "cyl")])

# Making your own data frame
students <- data.frame(
  name  = c("Ana", "Ben", "Cai", "Dee"),
  year  = c(3, 4, 3, 4),
  score = c(88, 92, 79, 85)
)
students


# -----------------------------------------------------------------------------
# Part 5. Missing values (N/A) ------------------------------------------------
# -----------------------------------------------------------------------------
# airquality: daily air readings in New York, May to September 1973
summary(airquality)                   # note the NA's under Ozone and Solar.R
mean(airquality$Ozone)                # NA, because one missing value spreads
mean(airquality$Ozone, na.rm = TRUE)  # drop the NAs first
sum(is.na(airquality$Ozone))          # how many are missing?
colSums(is.na(airquality))            # missing values in each column
complete <- na.omit(airquality)       # keep rows with no missing values
nrow(airquality); nrow(complete)

# ASK: Is dropping rows always safe for statistics?


# -----------------------------------------------------------------------------
# Part 6. Group summaries -----------------------------------------------------
# -----------------------------------------------------------------------------
table(mtcars$cyl)                               # counts
table(mtcars$cyl, mtcars$am)                    # two-way table
tapply(mtcars$mpg, mtcars$cyl, mean)            # mean mpg for each cyl group
aggregate(mpg ~ cyl + am, data = mtcars, FUN = mean)   # formula interface


# -----------------------------------------------------------------------------
# Part 7. Base R plots --------------------------------------------------------
# -----------------------------------------------------------------------------
hist(mtcars$mpg, main = "Fuel efficiency", xlab = "Miles per gallon",
     col = "lightblue", breaks = 8)

boxplot(mpg ~ cyl, data = mtcars, main = "MPG by number of cylinders",
        xlab = "Cylinders", ylab = "MPG", col = c("#9ecae1", "#fdae6b", "#a1d99b"))

plot(mpg ~ wt, data = mtcars, pch = 19, col = "steelblue",
     main = "Heavier cars use more fuel",
     xlab = "Weight (1000 lb)", ylab = "MPG")
abline(lm(mpg ~ wt, data = mtcars), col = "red", lwd = 2)   # fitted line


# -----------------------------------------------------------------------------
# Part 8. A first look at statistics ------------------------------------------
# -----------------------------------------------------------------------------
cor(mtcars$mpg, mtcars$wt)                     # correlation

# Do manual cars get better mileage than automatic ones? (Welch two-sample t-test)
t.test(mpg ~ am, data = mtcars)

# Linear regression
fit <- lm(mpg ~ wt + hp, data = mtcars)
summary(fit)                                   # coefficients, R-squared, p-values
coef(fit)
predict(fit, newdata = data.frame(wt = 3, hp = 150))   # a prediction for a new car
