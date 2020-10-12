# A solution to the less than five vector checking problem.
# ------------------------------------------------------------------------------

# If you want to include code from another file, you just have to 'source()'
# that file. R will run the file, so we'll get the plots again.
source("lcg.R")

# For a change, we'll set fixed values for the function parameters.
v <- lcg_vector(86436, 1093, 18257, 23, 100)

# I asked for less than 5 but all the values are huge. So lets adjust the
# requirement slightly and divide by a tenth of the mean of the values in the
# vector.
m <- mean(v)
v <- as.integer(v * 10 / m)

# So now we run through the vector, one entry at a time checking for <5 and
# setting any that are <5 to NA and reporting the event to the user.
for (i in 1:length(v)) {
    if (v[i] < 5) {
        v[i] <- NA
        cat('Item', i, 'in the vector was less than 5.\n', sep=' ')
    }
}
