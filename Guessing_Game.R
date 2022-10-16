# ------------------------------------------------------------------------------
# Guessing Game Exercise
# ------------------------------------------------------------------------------
# January 2020
# Pete Arnold
# ------------------------------------------------------------------------------

# 1. Generate a random number between 1 and 100 (inclusive).
n <- round(runif(1) * 100, 0)

# How shall we check that this is correct?

# 2. Get the user to make a guess.
guess <- readline(prompt="Enter a number:")

# 3. How do we check that we have a number?
is_number <- FALSE
# Change to using as.numeric.
is_number <- !is.na(as.numeric(guess))
#if (grepl("^[0-9]+$", guess)) is_number <- TRUE

if (!is_number) {
    message <- paste("The guess '", guess, "' was not a number!\n")
    stop(message)
}

# 4. How do we check if the guess matches the number?
is_guessed <- FALSE
if (as.integer(guess) == n) is_guessed <- TRUE

# 5. How to display a message that the guess was (in)correct.
if (is_guessed) {
    cat("The guess '", guess, "' was correct!\n")
} else {
    cat("The guess '", guess, "' was incorrect!\n")
    cat("The number was", n, ".\n")
}

# 6. How to let the player guess 6 times with a for-loop.
n <- round(runif(1) * 100, 0)
n_guesses <- 6
for (i in 1:n_guesses){
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
        break
    }
    is_guessed <- FALSE
    if (as.integer(guess) == n) is_guessed <- TRUE
    if (is_guessed) {
        cat("The guess '", guess, "' was correct!\n")
        break
    } else {
        cat("The guess '", guess, "' was incorrect!\n")
    }
}
if (!is_guessed) cat("The number was", n, ".\n")

# 7. Add a too high/too low hint.
n <- round(runif(1) * 100, 0)
n_guesses <- 6
for (i in 1:n_guesses){
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
        break
    }
    is_guessed <- FALSE
    if (as.integer(guess) == n) is_guessed <- TRUE
    if (is_guessed) {
        cat("The guess '", guess, "' was correct!\n")
        break
    } else if (as.integer(guess) > n) {
        cat("The guess '", guess, "' was too high!\n")
    } else {
        cat("The guess '", guess, "' was too low!\n")
    }
}
if (!is_guessed) cat("The number was", n, ".\n")

# 8a. Use a repeat loop so that we continue until the user guesses or inputs a
#    non number.
n <- round(runif(1) * 100, 0)
is_number <- TRUE
is_guessed <- FALSE
repeat{
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
        break
    }
    is_guessed <- FALSE
    if (as.integer(guess) == n) is_guessed <- TRUE
    if (is_guessed) {
        cat("The guess '", guess, "' was correct!\n")
        break
    } else if (as.integer(guess) > n) {
        cat("The guess '", guess, "' was too high!\n")
    } else {
        cat("The guess '", guess, "' was too low!\n")
    }
}
if (!is_guessed) cat("The number was", n, ".\n")

# 8b. Use a while loop for the same result.
n <- round(runif(1) * 100, 0)
is_number <- TRUE
is_guessed <- FALSE
while(is_number & !is_guessed){
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
    } else {
        is_guessed <- FALSE
    if (as.integer(guess) == n) is_guessed <- TRUE
        if (is_guessed) {
            cat("The guess '", guess, "' was correct!\n")
        } else if (as.integer(guess) > n) {
            cat("The guess '", guess, "' was too high!\n")
        } else {
            cat("The guess '", guess, "' was too low!\n")
        }
    }
}
if (!is_guessed) cat("The number was", n, ".\n")

# 9. Remember the guesses, display how many guesses there were and the guesses.
n <- round(runif(1) * 100, 0)
is_number <- TRUE
is_guessed <- FALSE
guesses <- vector()
count <- 1
repeat{
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
        break
    }
    guess <- as.integer(guess)
    guesses[count] <- guess
    count <- count + 1
    is_guessed <- FALSE
    if (guess == n) is_guessed <- TRUE
    if (is_guessed) {
        cat("The guess '", guess, "' was correct!\n")
        break
    } else if (guess > n) {
        cat("The guess '", guess, "' was too high!\n")
    } else {
        cat("The guess '", guess, "' was too low!\n")
    }
}
if (!is_guessed) cat("The number was", n, ".\n")
cat("There were", length(guesses), "guesses, which were:\n")
print(guesses)

# 10. Now move the number a little between every guess and save that too.
n <- round(runif(1) * 100, 0)   # The original guess.
d <- 1                          # Max move between guesses.
is_number <- TRUE
is_guessed <- FALSE
guesses <- vector()
numbers <- vector()
count <- 1
repeat{
    guess <- readline(prompt="Enter a number:")
    is_number <- FALSE
    if (grepl("^[0-9]+$", guess)) is_number <- TRUE
    if (!is_number) {
        cat("The guess '", guess, "' was not a number!\n")
        break
    }
    guess <- as.integer(guess)
    guesses[count] <- guess
    numbers[count] <- n
    count <- count + 1
    is_guessed <- FALSE
    if (guess == n) is_guessed <- TRUE
    if (is_guessed) {
        cat("The guess '", guess, "' was correct!\n")
        break
    } else if (guess > n) {
        cat("The guess '", guess, "' was too high!\n")
    } else {
        cat("The guess '", guess, "' was too low!\n")
    }
    if (runif(1) < 0.5)
    n <- n + as.integer((runif(1) - 0.5) * 2 * (d + 1))
}
if (!is_guessed) cat("The number was", n, ".\n")
cat("There were", length(guesses), "guesses, which were:\n")
print(guesses)
cat("The numbers were:\n")
print(numbers)

# 11. Rewrite as a function, using functions.

# 12. Calculate the error at each step and plot it.
#     Put the results in a data frame first.
results <- data.frame(numbers=numbers, guesses=guesses)
errors <- guesses - numbers
results <- cbind(results, errors)

x <- 1:length(guesses)
plot(x, results$errors, type='o', col="darkgreen")
hist(results$errors, col="steelblue")



