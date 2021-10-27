# ------------------------------------------------------------------------------
# Guessing Game Exercise
# ------------------------------------------------------------------------------
# October 2021
# Pete Arnold
# ------------------------------------------------------------------------------

# Step 0. Display something so we know we are working.
print("Hello, world!")

# Step 1. Think of a number.
# Generate a random number between 1 and 100 (inclusive). There are other
# solutions, for example, n <- as.integer(runif(1) * 100).
# This is a function call that gets 1 number in the range 1 to 100 from a
# uniform distribution (we'll cover that later).
number <- runif(1, 1, 100)
# But we should only allow whole numbers as decimals would be hard to guess
# exactly, so we round 'number' to zero decimal place and save the value back to
# 'number'.
number <- round(number, 0)

# We can, in fact, do this is one step.
number <- round(runif(1, 1, 100), 0)

# ------------------------------------------------------------------------------

# How shall we check that this is correct?
# Make n large and see the statistics of the values are saying.
# It is good practice to put numbers that might vary in named variables.

# Define a large number.
count <- 10000000
# Get that many numbers - this will be a long list called numbers.
numbers <- round(runif(count, 1, 100), 0)
# We can plot a histogram of those numbers to see if it looks right.
hist(numbers)
# We can do the basic descriptive statistics and see if they look right.
summary(numbers)

# ------------------------------------------------------------------------------

# And, as they are numbers, we can write some code to check them for us.
# Here we have textual data, Min. and Max., which we indicate is text by placing
# quotes around them (they are referred to as strings (of characters)).
# The square brackets []allow us to access a paricular data entry in a list.
# The 'if' statement reads like it is written and will run the first section of
# code if the check on the 'if' line is TRUE and the second if it is FALSE.
check <- summary(numbers)
if ((check['Min.'] == 1) & (check['Max.'] == 100)) {
    print("Number range is OK.")
} else {
    print("Number range is not as intended!")
}

# ------------------------------------------------------------------------------

# 2. Get the user to make a guess.

guess <- readline(prompt="Enter a number:")

# ------------------------------------------------------------------------------

# 3. How do we check that we have a number?
# Convert the entered string to a number - as.numeric() will do this and, if the
# string is not a number it will return NA.
guess <- as.numeric(guess)

# Now all we need to do is check if the guess is NA. If not, it must be a
# number. If it is NA, we'll stop and print out a message to tell the user.
# This kind of checking is done with an 'if' statement - if the contents of the
# brackets after the if are TRUE, the next section is run. If FALSE, it is
# skipped.
if (is.na(guess)){
    stop("That guess was not a number!\n")
}

# ------------------------------------------------------------------------------

# 4. How do we check if the guess matches the number?
# 4.1
if (guess == number){
    print("You guessed correctly!")
}
# 4.2
if (guess == number){
    print("You guessed correctly!")
} else {
    print("You guessed wrong!")
}
# 4.3
have_guessed <- guess == number
if (have_guessed == TRUE){
    print("You guessed correctly!")
} else {
    print("You guessed wrong!")
}

# ------------------------------------------------------------------------------

# 5. Do it again - use a loop - one with for, one with while and one with
# repeat.

# This is all the one-guess code.
number <- runif(1, 1, 100)
number <- round(number, 0)
guess <- readline(prompt="Enter a number:")
guess <- as.numeric(guess)
if (is.na(guess)){
    stop("That guess was not a number!\n")
}
have_guessed <- guess == number
if (have_guessed == TRUE){
    print("You guessed correctly!")
} else {
    print("You guessed wrong!")
}

# This is the for-loop code.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
steps <- 10
for (step in 1: steps){
    guess <- readline(prompt="Enter a number:")
    guess <- as.numeric(guess)
    if (is.na(guess)){
        stop("That guess was not a number!\n")
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        print("You guessed correctly!")
        break
    } else {
        print("You guessed wrong!")
    }
}

# This is the while-loop code.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
have_guessed <- FALSE
while(have_guessed == FALSE){
    guess <- readline(prompt="Enter a number:")
    guess <- as.numeric(guess)
    if (is.na(guess)){
        stop("That guess was not a number!\n")
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        print("You guessed correctly!")
    } else {
        print("You guessed wrong!")
    }
}

# This is the repeat-loop code.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
repeat{
    guess <- readline(prompt="Enter a number:")
    guess <- as.numeric(guess)
    if (is.na(guess)){
        stop("That guess was not a number!\n")
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        print("You guessed correctly!")
        break
    } else {
        print("You guessed wrong!")
    }
}

# ------------------------------------------------------------------------------

# 7. Messing with the code.
# 7.1 Improving the messages.
# This is the repeat-loop code.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
repeat{
    guess_string <- readline(prompt="Enter a number:")
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        stop(paste("That guess (", guess_string, ") was not a number!\n"))
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        cat("You guessed wrong, it wasn't", guess, "!\n")
    }
}

# 7.2 Improve the error handling.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
repeat{
    guess_string <- readline(prompt="Enter a number:")
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        cat("That guess (", guess_string, ") was not a number!\n")
        next
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        cat("You guessed wrong, it wasn't", guess, "!\n")
    }
}

# 7.3 An explicit way out.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
repeat{
    guess_string <- readline(prompt="Enter a number:")
    if (guess_string == 'q'){
        break
    }
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        cat("That guess (", guess_string, ") was not a number!\n")
        next
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        cat("You guessed wrong, it wasn't", guess, "!\n")
    }
}

# 7.4 With clues.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
repeat{
    guess_string <- readline(prompt="Enter a number:")
    if (guess_string == 'q'){
        break
    }
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        cat("That guess (", guess_string, ") was not a number!\n")
        next
    }
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        if (guess > number) {
            cat("You guessed too high, it wasn't", guess, "!\n")
        } else {
            cat("You guessed too low, it wasn't", guess, "!\n")
        }
    }
}

# 7.5 Save the guesses.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
guesses <- vector()
repeat{
    guess_string <- readline(prompt="Enter a number:")
    if (guess_string == "q"){
        break
    }
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        cat("That guess (", guess_string, ") was not a number!\n")
        next
    }
    guesses <- c(guesses, guess)
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        if (guess > number) {
            cat("You guessed too high, it wasn't", guess, "!\n")
        } else {
            cat("You guessed too low, it wasn't", guess, "!\n")
        }
    }
}
print("You guessed the following values:")
print(guesses)
plot(guesses, type='l')
abline(h=number, col='red')

# 7.6 Move the number.
number <- runif(1, 1, 100)
number <- round(number, 0)
attempts <- 0
change <- 3
guesses <- vector()
numbers <- c(number)
repeat{
    guess_string <- readline(prompt="Enter a number:")
    if (guess_string == "q"){
        break
    }
    guess <- as.numeric(guess_string)
    if (is.na(guess)){
        cat("That guess (", guess_string, ") was not a number!\n")
        next
    }
    guesses <- c(guesses, guess)
    have_guessed <- guess == number
    attempts <- attempts + 1
    if (have_guessed == TRUE){
        cat("You guessed correctly, it was", number, "!\n")
        cat("You took", attempts, "goes to guess.\n")
        break
    } else {
        if (guess > number) {
            cat("You guessed too high, it wasn't", guess, "!\n")
        } else {
            cat("You guessed too low, it wasn't", guess, "!\n")
        }
        number <- number + round(runif(1, -change, change), 0)
        numbers <- c(numbers, number)
    }
}
print("You guessed the following values:")
print(guesses)
plot(guesses, type='l', col='blue')
lines(numbers, col='red')

# ------------------------------------------------------------------------------





