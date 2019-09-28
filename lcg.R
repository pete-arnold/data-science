# ------------------------------------------------------------------------------
# Linear Congruential Generator (Random Number Generator).
# ------------------------------------------------------------------------------
# 22 September 2019
# Pete Arnold
# ------------------------------------------------------------------------------

# Next value/seed generator.
lcg <- function(modulus, a, c, seed) {
    return((a * (seed + c)) %% modulus)
}

# Vector generator.
lcg_vector <- function(modulus, a, c, seed, n) {
    l <- vector()
    for (i in 1:n) {
        s <- lcg(modulus, a, c, s)
        l <- rbind(l, s)
    }
    return(l)
}

# Create two arrays, one with typically bad parameters and one with good ones.
# Parameters from Nicole Radziwill,
# https://qualityandinnovation.com/2015/03/03/a-linear-congruential-generator-lcg-in-r/

# Number of elements in the vectors.
n <- 10000

# Bad parameters.
m <- 23
a <- 6
s <- runif(1) * 100
c <- 7
bad <- lcg_vector(m, a, c, s, n)
# Good parameters.
m <- 86436
a <- 1093
s <- runif(1) * 100
c <- 18257
good <- lcg_vector(m, a, c, s, n)

# Plot the results.
par(mfrow=c(2,2))
hist(bad, breaks=100)
plot(bad[1:length(bad)-1], bad[2:length(bad)], pch='.', cex=1, col='red')
hist(good, breaks=100)
plot(good[1:length(good)-1], good[2:length(good)], pch='.', cex=1, col='green')
