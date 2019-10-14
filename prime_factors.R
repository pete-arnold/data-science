# -----------------------------------------------------------------------------
# Determine the largest prime factor of a number
# -----------------------------------------------------------------------------
# Use a sieve of eratosthenes to generate the primes up to the square root of
# n then check each to see if it is a factor of n. Report the largest found.
# -----------------------------------------------------------------------------
# Peter Prevos 2017-01-03
# https://rstudio-pubs-static.s3.amazonaws.com/236332_7a7d3b552016415d9fda1f0fafc31ff9.html
# Euler Project — The First Twentyfive Problems in base R
#
# Pete Arnold
# 2 October 2019
# Modified with additional comments and adjustments
# -----------------------------------------------------------------------------
# Examples:
# prime_factors(1) : [1] 1
# prime_factors(99) : [1]  3  3 11
# prime_factors(13195) : [1]  5  7 13 29
# prime_factors(600851475143) : [1]   71  839 1471 6857
#
# max_prime_factor(13195) : [1] 29
# max_prime_factor(600851475143) : [1] 6857
# -----------------------------------------------------------------------------

# Sieve of Eratosthenes for generating primes 1:n.
# @param  n integer          The maximum value for the prime.
# @return   vector(integer)  A vector of the prime numbers in the range 1:n.
esieve <- function(n) {
    if (n==1) return(NULL)
    if (n==2) return(n)
    # Create a list of consecutive integers {2,3,…,N}.
    # Exclude even numbers to save computing time.
    the_primes <- c(2, seq(from=3, to=n, by=2))
    i <- 1
    next_prime <- 2
    while (next_prime ^ 2 <= n) {
        # Remove all multiples of primes from the the_primes.
        the_primes <- the_primes[(the_primes == next_prime) |
                                 ((the_primes %% next_prime) !=0 )]
        # Repeat until next_prime^2 > n, all the remaining numbers in the list
        # are primes.
        i <- i + 1
        next_prime <- the_primes[i]
   }
   return(the_primes)
}

# Calculate the prime factors of a number.
# @param n integer          The number whose primes factors we want.
# @param   vector(integer)  A vector of the factors.
prime_factors <- function (n) {
    the_factors <- c()
    # Get a list of all the primes up to the square root of n - why?
    primes <- esieve(floor(sqrt(n)))
    # Get a list of the indices to all the primes which are factors of n.
    candidate_index <- which((n %% primes) == 0)
    if (length(candidate_index) == 0) return(n)
    # Test each candidate prime (from low to high) as a factor of n.
    for (candidate in primes[candidate_index]) {
        # Divide n by the candidate as many times as possible, adding to the
        # list each time.
        while (n %% candidate == 0) {
            the_factors <- c(the_factors, candidate)
            n <- n / candidate
        }
    }
    # If there is anything left here, it will be the remaining prime
    # factor > sqrt(n).
    if (n > 1) the_factors <- c(the_factors, n)
    return(the_factors)
}

# Calculate the maximum prime factor of a number.
# @param n integer  The number whose primes factors we want.
# @param   integer  A the maximum prime factor.
max_prime_factor <- function (n) {
    max(prime_factors(n))
}

# Tests:
print(prime_factors(13195))
print(max_prime_factor(13195))
