# ------------------------------------------------------------------------------
# PMIM102J Scientific Computing and Healthcare
# Possible answers to the exercise in Introduction to R.
# ------------------------------------------------------------------------------
# Pete Arnold
# 18 January 2021
# ------------------------------------------------------------------------------

# 1. Display the 12 times table.
for (i in 1:12) {
    cat(i * 12, " ")
}
# or:
seq(12, 144, 12)

# 1aj. displaying 13 times table
for (i in 1:13){
  cat (i*13, " ")
}

## or:
seq(13, 169, 13)

# 1 (bonus) Display all the times tables from 1 to 12.
for (i in 1:12) {
    for (j in 1:12) {
        cat(i * j, " ")
    }
    cat("\n")
}

# And if you want to line things up.
for (i in 1:12) {
    for (j in 1:12) {
        v <- paste("    ", i * j)
        cat(substring(v, nchar(v) - 3, nchar(v)), " ")
    }
    cat("\n")
}

# 2. Random numbers until first one is <0.1 or you have 10.
for (i in 1:10) {
    n <- runif(1)
    if (n < 0.1) break
    cat(n, " ")
}
cat("\n")

count <- 1
repeat {
    if (count > 10) break
    n <- runif(1)
    if (n < 0.1) break
    cat(n, " ")
    count <- count + 1
}
cat("\n")

count <- 0
n <- runif(1)
while ((n >= 0.1) & (count < 10)){
    cat(n, " ")
    count <- count + 1
    n <- runif(1)
}

# 3. Display dice rolls until you get a pair that adds to 7.
repeat {
    d1 <- as.integer(runif(1) * 6 + 1)
    d2 <- as.integer(runif(1) * 6 + 1)
    cat("Roll:", d1, d2, "=", d1+d2, "\n")
    if (d1+d2 == 7) break
}

# Or with a function.
roll_dice <- function() {
    as.integer(runif(1) * 6 + 1)
}
repeat {
    d1 <- roll_dice()
    d2 <- roll_dice()
    cat("Roll:", d1, d2, "=", d1+d2, "\n")
    if (d1+d2 == 7) break
}

# Or with a function with more interesting dice.
roll_dice <- function(sides) {
    as.integer(runif(1) * sides + 1)
}
repeat {
    d1 <- roll_dice(4)
    d2 <- roll_dice(4)
    cat("Roll:", d1, d2, "=", d1+d2, "\n")
    if (d1+d2 == 7) break
}





