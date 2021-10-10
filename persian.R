# ------------------------------------------------------------------------------
# Matrices and contours
# Persian Rug Art
# ------------------------------------------------------------------------------
# outer is also possible as %o% compared to inner product %*%.
# also have kronecker
# ------------------------------------------------------------------------------
# Create vectors to provide the values for x and y calculations.
x <- y <- seq(-4*pi, 4*pi, len = 27)
# Calculate a function using the outer product of x and y and the function '+'.
r <- sqrt(outer(x^2, y^2, "+"))
# Visualise the values as a filled contour plot.
filled.contour(r, #axes = FALSE,
               color.palette = terrain.colors,
               frame.plot = FALSE, plot.axes = {})

# Run through a series of ordinary contour plots for different transformations.
par(mfrow = c(2, 2), mar = rep(0, 4))
for(f in pi^(0:3))
    contour(g <- cos(r^2)*exp(-r/f),
            drawlabels = FALSE, axes = FALSE, frame = TRUE)

# Try a random variable and solve it to see what we get.
par(mfrow=c(1, 1), mar=rep(1, 4))
s <- matrix(rbinom(27 * 27, size=10, prob=0.5), nrow=27, ncol=27)
t <- solve(s)
filled.contour(t, #axes = FALSE,
               color.palette = terrain.colors,
               frame.plot = FALSE, plot.axes = {})
