
# Lets obtain a vector with weights of male of age between 20 and 74 years.
weights <- read.csv("./data/weights.csv", header = T)
weights <- as.vector(weights$x)

# Formula for Calculating a Confidence Interval for Mean
confint <- function(x, alpha = .05) {
conflevel <- (1 - alpha)*100
stderr <- sd(x)/sqrt(length(x))
tcrit <- qt(1-alpha/2, length(x)-1)
margin <- stderr * tcrit
lower <- mean(x) - margin
upper <- mean(x) + margin
cat(conflevel,"Percent Confidence Interval","\n")
cat("Mean:", mean(x), "Std. Error:", stderr,"\n")
cat("Lower Limit:", lower, "\n")
cat("Upper Limit:", upper, "\n")
}

confint(weights, alpha = 0.10)

t.test(weights, mu = 191, conf.level = .90)

#Creating a new function to have flow control to determine whether the user is conducting a one-tailed
#test or a two-tailed test.

confint.1 <- function(x, alpha = .05, two.tailed = TRUE) {
cat("Mean:",mean(x),"\n")
df <- length(x) - 1
stderr <- sd(x)/sqrt(length(x))
cat("Standard error of the mean:",stderr,"\n")
conflevel <- 1 - alpha/2
if (two.tailed == FALSE) {
conflevel <- 1 - alpha
}
tcrit <- qt(conflevel, df)
margin <- stderr * tcrit
LL <- mean(x) - margin
UL <- mean(x) + margin
if (two.tailed == FALSE) {
cat("You are doing a one-tailed test.","\n")
cat("If your test is left-tailed, the lower bound","\n")
cat("is negative infinity. If your test is right-tailed","\n")
cat("the upper bound is positive infinity.","\n")
cat("Either add the margin",margin,"to or subtract it from","\n")
cat("the sample mean as appropriate.","\n")
cat("For a left-tailed test, the upper bound is",LL,"\n")
cat("For a right-tailed test, the lower bound is",UL,"\n")
}
if (two.tailed == TRUE) {
cat((1-alpha)*100,"percent confidence interval","\n")
cat("lower bound:",LL,"\n")
cat("upper bound:",UL,"\n")
}
}

# Now try:
confint.1(weights, alpha = 0.01, two.tailed = TRUE)
confint.1(weights, alpha = 0.01, two.tailed = FALSE)
