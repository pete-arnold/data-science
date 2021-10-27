# ------------------------------------------------------------------------------
# Statistics - BMI
# ------------------------------------------------------------------------------
# October 2021
# Pete Arnold
# ------------------------------------------------------------------------------
library(crayon)     # Used for coloured console text.
library(tidyverse)  # Used for the starwars dataset.

cat(yellow("\014\012PMIM102 Exercise - Statistics - working file\n"))
cat(yellow("---------------------------------------------------------------\n"))

# ------------------------------------------------------------------------------

# Step 1. Calculate BMI.
weight_kg <- 100
height_cm <- 170
weight_kg / ((height_cm / 100) ^ 2)

# ------------------------------------------------------------------------------

# Step 2. Make a function to do this.
bmi <- function(w_kg, h_cm){
    value <- w_kg / ((h_cm / 100) ^ 2)
    return(value)
}

saved_bmi <- bmi(70, 170)
bmi(weight_kg, height_cm)
bmi(650, 20)    # 16250
# ------------------------------------------------------------------------------

# Step 3. Sanity and error checks.


# Step 3a. Testing code.
# Create a list of the tests you think you need to do to check the function.
# These will include boundaries and random values.
# You will need to supply expected values of the function responses and to
# make sure the actual response matches these.

# ------------------------------------------------------------------------------

# Step 4. Work out the BMI values for the Star Wars characters.

# ------------------------------------------------------------------------------

# Step 5. Create a boxplot of BMI. Create one separating by gender.

# Find and remove the outlier (and repeat the above lines).


# Step 5b. Create the cumulative distribution plot for the BMI values.
# What extra information have we got here?


# Step 5a. Outliers.
# Find out how to define outliers. Are there functions that will detect outliers
# (and remove them)? When should you do this?

# ------------------------------------------------------------------------------
# Intermission - standard deviation.

x <- seq(-4, 4, 0.1)
n <- 10000
sample <- rnorm(n, mean=0, sd=1)

plot(sample, pch=20, col='darkmagenta', cex=0.1)

hist(sample, breaks=50)

# Overlay an appropriately scaled normal distribution.
pd <- (n/4.8)*dnorm(x, mean(sample), sd(sample))
lines(x, pd, col="darkmagenta", lwd=2)

xpoly <- x[x >= 0 & x <= 1.00]
ypoly <- pd[x >= 0 & x <= 1.00]
xpoly <- c(xpoly, 1.00, 0.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="slategray1")

xpoly <- x[x <= 0 & x >= -1.00]
ypoly <- pd[x <= 0 & x >= -1.00]
xpoly <- c(xpoly, 0.00, -1.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="steelblue1")

xpoly <- x[x >= 1.00 & x <= 2.00]
ypoly <- pd[x >= 1.00 & x <= 2.00]
xpoly <- c(xpoly, 2.00, 1.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="slategray2")

xpoly <- x[x <= -1.00 & x >= -2.00]
ypoly <- pd[x <= -1.00 & x >= -2.00]
xpoly <- c(xpoly, -1.00, -2.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="steelblue2")

xpoly <- x[x >= 2.00 & x <= 3.00]
ypoly <- pd[x >= 2.00 & x <= 3.00]
xpoly <- c(xpoly, 3.00, 2.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="thistle1")

xpoly <- x[x <= -2.00 & x >= -3.00]
ypoly <- pd[x <= -2.00 & x >= -3.00]
xpoly <- c(xpoly, -2.00, -3.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="plum2")

xpoly <- x[x >= 3.00 & x <= 4.00]
ypoly <- pd[x >= 3.00 & x <= 4.00]
xpoly <- c(xpoly, 4.00, 3.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="red")

xpoly <- x[x <= -3.00 & x >= -4.00]
ypoly <- pd[x <= -3.00 & x >= -4.00]
xpoly <- c(xpoly, -3.00, -4.00)
ypoly <- c(ypoly, 0, 0)
polygon(xpoly, ypoly, col="red")

# ------------------------------------------------------------------------------

# Step 6. Create a demonstration dataset.
# Step 6.1 Create the BMI data for women.


# Step 6.2 Create the BMI data for men in the same way.


# Step 6.3 Join the tables.


# Step 6.4 Calculate the BMI.


# What was that warning message?

# Other ways to do this.

# Check the dataset again.


# Step 6.5 Check the BMI data.


# ------------------------------------------------------------------------------

# Step 7. Perform t-tests on the variables.

# Step 7.1 Compare the heights, weights and BMI for genders.


# Step 7.2 Compare the heights, weights and BMI for genders for a small sample.


# ------------------------------------------------------------------------------

# Step 8. Wilcox Mann Whitney non-parametric test.


# ------------------------------------------------------------------------------

# Step 9. Correlation.


# ------------------------------------------------------------------------------

# Step 10. Look at the normal distibution Q-Q plots.


# ------------------------------------------------------------------------------

# Step 11. Kolmogorov-Smirnov test.


# ------------------------------------------------------------------------------

# Step 12. Categorical variables.
# Create a cateory for the BMI using the standard boundaries.


# Step 12.1. Contingency table.
# Create the contingency table for gender vs. BMI Group.


# ------------------------------------------------------------------------------

# Step 13. Get the test dataset.
birthweight <- read_csv(file="data/Birthweight.csv")
head(birthweight)

# Low birthweight is < 2.70kg.
# Select suitable parental age bands.

attach(birthweight)
# Step 13.1 Explore and describe the dataset.
# How many people?
cat("There are", length(levels(as.factor(birthweight$ID))), "babies.\n")

# Step 13.2 T-test questions.
# Which factors might lead to lighter babies? Smokers? What else?

# Step 13.3 Correlation questions.
# Which variables have a strong relationship? Maternal height and baby length?

# Step 13.4 Chi-squared test questions.
# Is there a relationship between baby weight and smoking? Age and baby weight?

# Step 13.5 Find or create some groups to enable comparisons. Look at continuous
# variables - plots, t-test, wilcox, correlation. Look at categorical variables
# - plots, chi-squared tests.
detach(birthweight)

cat(yellow("Done.\n"))
cat(yellow("---------------------------------------------------------------\n"))

# ------------------------------------------------------------------------------





