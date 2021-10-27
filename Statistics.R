# ------------------------------------------------------------------------------
# Statistics - BMI
# ------------------------------------------------------------------------------
# October 2021
# Pete Arnold
# ------------------------------------------------------------------------------
library(crayon)     # Used for coloured console text.
library(tidyverse)  # Used for the starwars dataset.

cat(yellow("\014\012PMIM102 Exercise - Statistics\n"))
cat(yellow("---------------------------------------------------------------\n"))

# ------------------------------------------------------------------------------

# Step 1. Calculate BMI.
# bmi <- weight_kg / ((height_cm / 100) ^ 2)
# Try it out:
weight_kg <- 70
height_cm <- 170
print(weight_kg / ((height_cm / 100) ^ 2))

# ------------------------------------------------------------------------------

# Step 2. Make a function to do this.
# bmi - calculate the body mass index from weight and height.
# @param w_kg numeric The person's weight in kg.
# @param h_cm numeric The person's height in cm.
# @return     numeric The person's BMI.
bmi <- function(w_kg, h_cm) {
    b <- w_kg / ((h_cm / 100)^2)
    return (b)
}
print(bmi(weight_kg, height_cm))

# ------------------------------------------------------------------------------

# Step 3. Sanity and error checks.
# bmi - Calculate body-mass index
# Uses two global variables:
#   error       contains any error message created by the function.
#   show_errors indicates whether the function should display an error if one
#               occurs.
# @param w_kg double Weight in kilograms
# @param h_cm double Height in centimetres
# @return     double BMI
bmi <- function(w_kg, h_cm) {
   error <<- NA
   if ((w_kg < 2) | (w_kg > 650) | (h_cm < 50) | (h_cm > 280)) {
       error <<- "bmi: inputs out of reasonable range."
       b <- NA
   } else {
       b <- w_kg / ((h_cm / 100)^2)
       if ((b < 15) | (b > 40)) {
           error <<- "bmi: output out of expected range."
           b <- NA
       }
   }
   if (!is.na(error) && show_errors) print(error)
   return (b)
}

show_errors <- FALSE
bmi(650, 100)
print(error)

# Step 3a. Testing code.
# Create a list of the tests you think you need to do to check the function.
# These will include boundaries and random values.
# You will need to supply expected values of the function responses and to
# make sure the actual response matches these.

test_bmi <- function(w, h, expect){
    show_errors <- TRUE
    error <- ''
    if ((is.na(expect) & !is.na(bmi(w, h))) |
        (!is.na(expect) & (bmi(w, h) != expect))){
        print(error)
    }
}
test_bmi(100, 200, 25)
test_bmi(100, 100, NA)

# ------------------------------------------------------------------------------

# Step 4. Work out the BMI values for the Star Wars characters.
head(starwars)
sw <- starwars %>% 
    select(name, height, mass, gender) %>%
    mutate(BMI=bmi(mass, height))
head(sw)

# ------------------------------------------------------------------------------

# Step 5. Create a boxplot of BMI. Create one separating by gender.
boxplot(sw$BMI)
female <- sw %>% filter(gender=="masculine")
male <- sw[sw$gender=="masculine"]
boxplot(sw$BMI[sw$gender=="feminine"], sw$BMI[sw$gender=="masculine"])
boxplot(sw$BMI ~ sw$gender)

# Find and remove the outlier (and repeat the above lines).
head(sw %>% filter(BMI>=100))
sw <- sw %>% filter(BMI<100)

# Step 5b. Create the cumulative distribution plot for the BMI values.
# What extra information have we got here?
plot(ecdf(sw$BMI[sw$gender=="feminine"]),
    main="CDF for BMI by gender",
    xlim=range(sw$BMI, na.rm=TRUE), 
    col="red")
plot(ecdf(sw$BMI[sw$gender=="masculine"]), 
    col="blue",
    add=TRUE)
# Comparing the CDF and boxplot.
par(mfrow=c(2,1))
plot(ecdf(sw$BMI[sw$gender=="feminine"]),
    main="CDF for BMI by gender",
    xlim=range(sw$BMI, na.rm=TRUE), 
    col="red")
plot(ecdf(sw$BMI[sw$gender=="masculine"]), 
    col="blue",
    add=TRUE)
boxplot(sw$BMI ~ sw$gender, col=c("red", "blue"),horizontal=TRUE)
par(mfrow=c(1,1))

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
lines(x, pd, col='darkmagenta', lwd=2)
abline(v=0, lwd=2, col='orange')

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
N <- 100
bmi_data_female <- data.frame(gender=character(N), 
                              weight_kg=numeric(N), 
                              height_cm=numeric(N))
bmi_data_female$gender <- 'Female'
mean_female_weight <- 69
sd_female_weight <- 10
mean_female_height <- 162
sd_female_height <- 6
bmi_data_female$weight_kg <- rnorm(N, mean=mean_female_weight, sd=sd_female_weight)
bmi_data_female$height_cm <- rnorm(N, mean=mean_female_height, sd=sd_female_height)
# Check it looks OK.
hist(bmi_data_female$weight_kg)
hist(bmi_data_female$height_cm)
# What about correlation?
plot(bmi_data_female$weight_kg, bmi_data_female$height_cm)
cor.test(bmi_data_female$weight_kg, bmi_data_female$height_cm)

# Does it matter that these are so different?
boxplot(bmi_data_female$weight_kg, bmi_data_female$height_cm)

# Step 6.2 Create the BMI data for men in the same way.
bmi_data_male <- data.frame(gender=character(N), 
                            weight_kg=numeric(N), 
                            height_cm=numeric(N))
bmi_data_male$gender <- 'Male'
mean_male_weight <- 84
sd_male_weight <- 12
mean_male_height <- 177
sd_male_height <- 6
bmi_data_male$weight_kg <- rnorm(N, mean=mean_male_weight, sd=sd_male_weight)
bmi_data_male$height_cm <- rnorm(N, mean=mean_male_height, sd=sd_male_height)
# Check it looks OK.
hist(bmi_data_male$weight_kg)
hist(bmi_data_male$height_cm)
plot(bmi_data_male$weight_kg, bmi_data_male$height_cm)
cor.test(bmi_data_male$weight_kg, bmi_data_male$height_cm)
# Does it matter that these are so different?
boxplot(bmi_data_male$weight_kg, bmi_data_male$height_cm)

# Step 6.3 Join the tables.
bmi_data <- rbind(bmi_data_female, bmi_data_male)

# Step 6.4 Calculate the BMI.
bmi_data$BMI <- bmi(bmi_data$weight_kg, bmi_data$height_cm)

# What was that warning message?
bmi <- function(w_kg, h_cm) {
   error <<- NA
   if ((w_kg < 2) || (w_kg > 650) || (h_cm < 50) || (h_cm > 280)) {
       error <<- "bmi: inputs out of reasonable range."
       b <- NA
   } else {
       b <- w_kg / ((h_cm / 100)^2)
       if ((b < 15) || (b > 40)) {
           error <<- "bmi: output out of expected range."
           b <- NA
       }
   }
   if (!is.na(error) && show_errors) print(error)
   return (b)
}
bmi_data$BMI <- bmi(bmi_data$weight_kg, bmi_data$height_cm)
# Other ways to do this.
for(i in 1:nrow(bmi_data)){
    bmi_data[i, 'BMIF'] <- bmi(bmi_data[i, 'weight_kg'], bmi_data[i, 'height_cm'])
}
bmi_data$BMIM <- mapply(bmi, bmi_data$weight_kg, bmi_data$height_cm)
bmi_data <- bmi_data %>%
    mutate(BMIT=bmi(weight_kg, height_cm))

# Check the dataset again.
with(bmi_data, plot(weight_kg, height_cm))

df <- bmi_data %>% filter(gender=='Female')
plot(df$weight_kg, df$height_cm)
with(bmi_data %>% filter(gender=='Male'), plot(weight_kg, height_cm))

# Step 6.5 Check the BMI data.
hist(bmi_data$weight_kg)
hist(bmi_data[which(bmi_data$gender=='Female'),]$weight_kg,
    col=rgb(1, 0, 0, 0.5), breaks=20)
hist(bmi_data[which(bmi_data$gender=='Male'),]$weight_kg,
    col=rgb(0, 0, 1, 0.5), breaks=20, add=TRUE)
with(bmi_data, boxplot(weight_kg ~ gender))

hist(bmi_data$height_cm)
hist(bmi_data[which(bmi_data$gender=='Female'),]$height_cm,
    col=rgb(1, 0, 0, 0.5), breaks=20)
hist(bmi_data[which(bmi_data$gender=='Male'),]$height_cm,
    col=rgb(0, 0, 1, 0.5), breaks=20, add=TRUE)
with(bmi_data, boxplot(height_cm ~ gender))

hist(bmi_data$BMI, breaks=30)
hist(bmi_data[which(bmi_data$gender=='Female'),]$BMI,
    col=rgb(1, 0, 0, 0.5), breaks=30)
hist(bmi_data[which(bmi_data$gender=='Male'),]$BMI,
    col=rgb(0, 0, 1, 0.5), breaks=30, add=TRUE)

with(bmi_data, boxplot(BMI ~ gender, col=c('darkseagreen1', 'tan1')))

# ------------------------------------------------------------------------------

# Step 7. Perform t-tests on the variables.

# Step 7.1 Compare the heights, weights and BMI for genders.
t.test(bmi_data_female$height_cm)

t.test(bmi_data_female$height_cm, bmi_data_male$height_cm)
t.test(bmi_data_female$weight_kg, bmi_data_male$weight_kg)
t.test(bmi_data[which(bmi_data$gender=='Female'),]$BMI,
    bmi_data[which(bmi_data$gender=='Male'),]$BMI)


# Step 7.2 Compare the heights, weights and BMI for genders for a small sample.
Ns <- 500
n_breaks <- 20
# Selected by gender.
sample_female <- sample_n(bmi_data[which(bmi_data$gender=='Female'),], Ns)
sample_male <- sample_n(bmi_data[which(bmi_data$gender=='Male'),], Ns)
# Selected and then separated.
sample <- sample_n(bmi_data, Ns)
sample_female <- sample[which(sample$gender=='Female'),]
sample_male <- sample[which(sample$gender=='Male'),]
cat("We have", nrow(sample_female), "women and", nrow(sample_male), "men.\n")

hist(sample_female$BMI, col=rgb(1, 0, 0, 0.5), breaks=n_breaks)
hist(sample_male$BMI, col=rgb(0, 0, 1, 0.5), breaks=n_breaks, add=TRUE)

with(sample, boxplot(weight_kg ~ gender))
with(sample, boxplot(height_cm ~ gender))
with(sample, boxplot(BMI ~ gender))

t.test(sample_female$height_cm, sample_male$height_cm)
t.test(sample_female$weight_kg, sample_male$weight_kg)
t.test(sample_female$BMI, sample_male$BMI)

# ------------------------------------------------------------------------------

# Step 8. Wilcox Mann Whitney non-parametric test.
wilcox.test(bmi_data_female$height_cm)
wilcox.test(sample_female$height_cm)

wilcox.test(bmi_data_female$height_cm, bmi_data_male$height_cm)
wilcox.test(bmi_data_female$weight_kg, bmi_data_male$weight_kg)
wilcox.test(bmi_data[which(bmi_data$gender=='Female'),]$BMI,
    bmi_data[which(bmi_data$gender=='Male'),]$BMI)

wilcox.test(sample_female$height_cm, sample_male$height_cm)
wilcox.test(sample_female$weight_kg, sample_male$weight_kg)
wilcox.test(sample_female$BMI, sample_male$BMI)

# ------------------------------------------------------------------------------

# Step 9. Correlation.
rm(weight_kg, height_cm)

attach(bmi_data)
cor.test(weight_kg, height_cm)
plot(weight_kg, height_cm)
cor.test(weight_kg, BMI)
plot(weight_kg, BMI)
random <- rnorm(length(weight_kg))
cor.test(weight_kg, random)
plot(weight_kg, random)
detach(bmi_data)

# ------------------------------------------------------------------------------

# Step 10. Look at the normal distibution Q-Q plots.
qqnorm(bmi_data_female$weight_kg, main='Female, weight', col='red')
qqline(bmi_data_female$weight_kg, col='black')
qqnorm(bmi_data_male$height_cm, main='Male, height', col='blue')
qqline(bmi_data_male$height_cm, col='black')
qqnorm(sample_female$weight_kg, main='Female sample, weight', col='red')
qqline(sample_female$weight_kg, col='black')
qqnorm(sample_male$height_cm, main='Male sample, weight', col='blue')
qqline(sample_male$height_cm, col='black')

qqnorm(bmi_data$weight_kg, main='Weight (m+f)')
qqline(bmi_data$weight_kg, col='red')
qqnorm(bmi_data$height_cm, main='Height (m+f)')
qqline(bmi_data$height_cm, col='red')
qqnorm(sample$weight_kg, main='Sample weight (m+f)')
qqline(sample$weight_kg, col='red')
qqnorm(sample$height_cm, main='Sample height (m+f)')
qqline(sample$height_cm, col='red')

# ------------------------------------------------------------------------------

# Step 11. Kolmogorov-Smirnov test.
ks.test(scale(bmi_data_female$height_cm), "pnorm")
ks.test(scale(bmi_data_female$height_cm), scale(bmi_data_female$height_cm))
ks.test(scale(bmi_data_female$weight_kg), "pnorm")
ks.test(scale(bmi_data_female$weight_kg), scale(bmi_data_male$weight_kg))
ks.test(scale(bmi_data[which(bmi_data$gender=='Female'),]$BMI), "pnorm")
ks.test(scale(bmi_data[which(bmi_data$gender=='Female'),]$BMI),
        scale(bmi_data[which(bmi_data$gender=='Male'),]$BMI))


# ------------------------------------------------------------------------------

# Step 12. Categorical variables.
# Create a cateory for the BMI using the standard boundaries.
bmi_data$Height_Group <- cut(bmi_data$height_cm, c(0, 150, 170, 190, Inf),
    right=FALSE, 
    labels=c("Short", "Medium", "Long", "Very long"))
table(bmi_data$gender, bmi_data$Height_Group)
barplot(table(bmi_data$gender, bmi_data$Height_Group), beside=TRUE)
chisq.test(bmi_data$gender, bmi_data$Height_Group)

# Create a cateory for the BMI using the standard boundaries.
bmi_data$BMI_Group <- cut(bmi_data$BMI, c(0, 18.5, 25, 30, 35, Inf),
    right=FALSE, 
    labels=c("Underweight", "Healthy", "Overweight", "Obese", "Very Obese"))

head(sample_n(bmi_data, 10))
levels(as.factor(bmi_data$BMI_Group))
with(bmi_data, boxplot(BMI ~ BMI_Group))

# Step 12.1. Contingency table.
# Create the contingency table for gender vs. BMI Group.
table(bmi_data$gender, bmi_data$BMI_Group)
barplot(table(bmi_data$gender, bmi_data$BMI_Group), beside=TRUE)
chisq.test(bmi_data$gender, bmi_data$BMI_Group)

sample$BMI_Group <- cut(sample$BMI, c(0, 18.5, 25, 30, 35, Inf),
    right=FALSE, 
    labels=c("Underweight", "Healthy", "Overweight", "Obese", "Very Obese"))
table(sample$gender, sample$BMI_Group)
legend <- rownames(table(sample$gender, sample$BMI_Group))
barplot(table(sample$gender, sample$BMI_Group), legend.text=legend, args.legend = list(x = "topleft"), beside=TRUE)
chisq.test(sample$gender, sample$BMI_Group)

sw$BMI_Group <- cut(sw$BMI, c(0, 18.5, 25, 30, 35, Inf),
    right=FALSE, 
    labels=c("Underweight", "Healthy", "Overweight", "Obese", "Very Obese"))
barplot(table(sw$gender, sw$BMI_Group), legend.text=legend, args.legend = list(x = "topleft"), beside=TRUE)
chisq.test(sw$gender, sw$BMI_Group, correct=FALSE)
chisq.test(sw$gender, sw$BMI_Group, correct=TRUE)

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





