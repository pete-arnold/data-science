# ------------------------------------------------------------------------------
# Guessing game as a for loop to demonstrate debugger use.
# ------------------------------------------------------------------------------
# Pete Arnold
# 26 October 2021
# ------------------------------------------------------------------------------

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
