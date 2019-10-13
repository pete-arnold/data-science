# ------------------------------------------------------------------------------
# Prostate Cancer
# ------------------------------------------------------------------------------
# Format
# This data frame contains the following columns:
# lcavol - log(cancer volume) 
# lweight - log(prostate weight) 
# age - age 
# lbph - log(benign prostatic hyperplasia amount) 
# svi - seminal vesicle invasion 
# lcp - log(capsular penetration) 
# gleason - Gleason score 
# pgg45 - percentage Gleason scores 4 or 5 
# lpsa - log(prostate specific antigen)
#
# install.packages('faraway')

library(faraway)
attach(prostate)

print(head(prostate))

summary(prostate)

options(repr.plot.width = 8, repr.plot.height = 8)
plot(prostate)

# Plot of PSA levels with gleason.
options(repr.plot.width = 4, repr.plot.height = 4)
boxplot(lpsa ~ gleason)

# Tables svi vs gleason
table(svi, gleason)

# Create a factor for age-group.
par(mfrow=c(1,2))
options(repr.plot.width = 8, repr.plot.height = 4)
prostate$agegp <- as.factor(ifelse(age<50, '<50', ifelse(age<60, '50-60', ifelse(age<70, '60-70', '>70'))))
with(prostate, boxplot(lpsa ~ agegp))
prostate$agegp <- factor(prostate$agegp, levels=c('<50', '50-60', '60-70', '>70'))
with(prostate, boxplot(lpsa ~ agegp))

# Get psa range (< 10, 10-20, >20).
par(mfrow=c(1,2))
prostate$psagp <- as.factor(ifelse(lpsa<log(10), '<10', ifelse(lpsa<log(20), '10-20', '>20')))
with(prostate, boxplot(lpsa ~ psagp))
prostate$psagp <- factor(prostate$psagp, levels=c('<10', '10-20', '>20'))
with(prostate, boxplot(lpsa ~ psagp))

par(mfrow=c(1,2))
plot(exp(lpsa), exp(lcavol))
plot(exp(lpsa), lcavol)

par(mfrow=c(1,2))
qqnorm(exp(lpsa))
qqline(exp(lpsa))

qqnorm(lpsa)
qqline(lpsa)

shapiro.test(lpsa)

# Simple linear regression.
par(mfrow=c(1,2))
m<-lm(lcavol~lpsa, data=prostate)
plot(lpsa,lcavol)
abline(m, col='red')

m<-lm(lcavol~exp(lpsa), data=prostate)
plot(exp(lpsa),lcavol)
abline(m, col='green')

