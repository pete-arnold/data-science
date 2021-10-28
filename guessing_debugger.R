# ------------------------------------------------------------------------------
# Guessing game as a for loop to demonstrate debugger use.
# ------------------------------------------------------------------------------
# Pete Arnold
# 26 October 2021
# ------------------------------------------------------------------------------

# A function definition contains:
# -------------------------------
# The word 'function()' to indicate that it is a function.
# The brackets contain a list of parameter names.
# These names can then be referred to in the function code.
# When the function is used in your code, you supply values for these parameters.
# In your definition, you can provide a default value (using '=').
# The function code is contained in the curly brackets, { <code> }.
# When the function is used, it does it's work which may produce a value to be
#    returned. This value is returned using return(value). The part of your
#    code that uses the function effectively becomes this value. For example,
#    x <- guessing_game(5)
#    will run the game below with a parameter, 5, which will be loaded into
#    max_attempts (just for this particular use). When the function finishes,
#    the return(list(values)) line means that the original code,
#    x <- guessing_game(5) is now, effectively
#    x <- list(values)
#    and the list of values from the guessing_game() function are stored in a
#    variable, x.
# You could use the function as:
#    x <- guessing_game(max_attempts=8)  # if you want to name the parameter
#                                        # to make the code clearer
#    n <- 16
#    x <- guessing_game(n)               # if you want to vary the number
#    x <- guessing_game()                # to use the default number specified in
#                                        # the definition (i.e. 10).
#    x <- guessing_game(runif(1, 10, 20))# to set a random number for the max
#                                        # attempts.

# The guessing game as a function.
# See the guessing game exercises from day 1.
# @param max_attempts integer The maximum number of attempts you have to guess.
# @return A list containing the numbers and guesses.
guessing_game <- function(max_attempts=10){
    # Include the debugger:
    browser()
    number <- runif(n=1, min=1, max=100)
    number <- round(number, 0)
    attempts <- 0
    change <- 3
    guesses <- vector()
    numbers <- c(number)
    for(i in 1:max_attempts){
        guess_string <- readline(prompt="Enter a number: ")
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
    plot(guesses, type='l', col='red')
    lines(numbers, col='blue')

    return(list(nums=numbers, attempts=guesses))
}

x <- guessing_game(20)

guessing_game()
