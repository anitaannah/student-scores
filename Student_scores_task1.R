
setwd("C:/Users/HP/Documents")
getwd()



data=read.table(file.choose(),header=T,sep=",")
data

X <- as.numeric(as.vector(data$Scores))
Y <- as.numeric(as.vector(data$Hours))
X
Y
n <- length(X)

#------Calculating Slope and Intercept------#

b.1 <- sum((Y - mean(Y))*(X - mean(X)))/sum((X - mean(X))^2)
b.0 <- mean(Y)- (b.1 * mean(X))

#------Calculating y hat and Error term------#

y.hat <- b.0 + b.1*X
err   <- Y - y.hat


#------Sum of squares------#

SSerr <- sum(err ^ 2)
SSr   <- sum((y.hat - mean(Y))^2)
SStot <- sum((Y - mean(Y))^2)

#------Coeficient of Determination------#

R.Square     <- SSr/SStot
adj.R.Square <- 1-(((1-R.Square)*(n-1))/(n-1-1))

#------F statistic and Std. Residual Error------#

F.Statistic      <- (SSr/1)/(SSerr/(n-2))
Residual.std.err <- sqrt(SSerr/(n-2))
P.value          <- 1-pf(F.Statistic,1,576)

#------Std.Error of Beta0 and Beta1------#

sigma      <- sqrt(SSerr/(n-2)) 
std.err.b0 <- sigma*sqrt((1/n)+(mean(X)^2)/sum((X-mean(X))^2))
std.err.b1 <- sigma/sqrt(sum((X-mean(X))^2))

#------test statistic for Beta0 & Beta1------#

t1 <- b.1/std.err.b1
t0 <- b.0/std.err.b0

#------Prob.value for test statistic of t0 and t1------#

p0 <- 2*pt(-abs(t0),df=n-2)
p1 <- 2*pt(-abs(t1),df=n-2)


#------Estimated values of simple regression model------#

Intercept <- c(round(b.0,5), round(std.err.b0,5), round(t0,5), p0)
coeff.X   <- c(round(b.1,5), round(std.err.b1,5), round(t1,5), p1)
coefficients <- rbind(Intercept, coeff.X)
colnames(coefficients) <- c("Estimates","Std.Error", "t-value","Pr(>|t|)") 

result <- list("Summary of Simple Regression Model" = "Weight ~ Day since Birth",
               "Coefficients" = coefficients,
               "Residual standard error" = round(Residual.std.err,5),
               "R-squared" = round(R.Square,5),
               "Adjusted R-squared" = round(adj.R.Square,5),
               "F-statistic" = round(F.Statistic,5),
               "Degrees of freedom" = n-2,
               "p-value" = P.value)
print(result)
