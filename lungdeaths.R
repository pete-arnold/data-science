# ------------------------------------------------------------------------------
# UK Lung Deaths
# ------------------------------------------------------------------------------

data(UKLungDeaths)

# Join the vectors into a data frame.
df <- data.frame(All=as.vector(ldeaths), Male=as.vector(mdeaths), Female=as.vector(fdeaths))

# Add the dates.
df <- cbind(Date=as.Date(0, origin='1974-01-01'), df)
for(i in 2:nrow(df)){
    df[i, 1] <- df[i-1, 1] + 30
}

df <- cbind(Date=as.POSIXlt('1974-01-01'), df)
for(i in 2:nrow(df)){
    #browser()
    d <- as.POSIXlt(df[i-1, 1])
    #print(class(d))
    d$mon <- d$mon + 1
    print(d)
    df[i, 1] <- as.POSIXct(d)
}

df <- cbind(Date=seq(from = as.Date("1974-01-01"), to = as.Date("1979-12-31"), by = 'month'), df)

# Consider proportions?
plot(df)
boxplot()
