# ------------------------------------------------------------------------------
# Asking the user for some information.
# ------------------------------------------------------------------------------
# 22 September 2019
# Pete Arnold
# ------------------------------------------------------------------------------

# Function: Get an integer from the user.
# Ask the user to enter a number, check it. If not an integer, report this and
# try again.
# @return The integer
get_number <- function() { 
    n <- readline(prompt="Enter an integer: ")
    if(!grepl("^[0-9]+$", n)) {
        cat(n, "is not an integer.\n", sep=" ")
        return(get_number())
    }
    return(as.integer(n))
}

# Main program: Choose a random number and get the user to guess until they get
#               it right.
set.seed(42)
number <- round(runif(1) * 100, digits = 0)
guess <- (-1)

cat("Guess a number between 0 and 100.\n")

while(guess != number)
{ 
    guess <- get_number()
    if (guess == number) {
        cat("Da iawn!,", number, "is the number.\n", sep=" ")
    }
    else if (guess < number) {
        cat("That was a little low.\n")
    }
    else if(guess > number) {
        cat("Too big!\n")
    }
}
