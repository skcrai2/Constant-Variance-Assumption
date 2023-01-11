#let's load the library and the data and take a look at it

library(faraway)
require(faraway)
data(sat)
head(sat)

#now we have to make some changes to the data to check our assumptions
by.math <- sat[order(sat$math),]
by.verbal <- sat[order(sat$verbal),]
by.salary <- sat[order(sat$salary),]
attach(sat)
out <- lm(total ~ expend + ratio + salary + takers)
summary(out)

#It appears from the linear model including all of the other variables as predictors of the total SAT score, 
#only takers was significant.

#let's look at the plot

plot(residuals(out) ~ fitted(out), xlab="Fits", ylab="Residuals")
abline(h=0)

#The residuals seem scattered rather randomly against the fits, there may be a slight U shaped pattern, however.
#Now, let's look at a plot of the absolute residuals vs the fits

plot(sqrt(abs(residuals(out))) ~ fitted(out), xlab="Fits", ylab=expression(sqrt(hat(epsilon))))
summary(lm(sqrt(abs(residuals(out)))~ fitted(out)))

#It looks like the absolute residuals are not predicted very well by the response variable, 
#so, we can conclude that there does not seem to be a problem with the constant variance assumption

#For a proper evaluation of residual plots, we can generate some artificial plots for situations where true relationships are known:

windows()
par(mfrow=c(2,2))
plot(residuals(out) ~ sat$expend, xlab="expend", ylab="Residuals")
plot(residuals(out) ~ sat$salary, xlab="salary", ylab="Residuals")
plot(residuals(out) ~ sat$ratio, xlab="ratio", ylab="Residuals")
plot(residuals(out) ~ sat$takers, xlab="takers", ylab="Residuals")
var.test(residuals(out)[sat$expend<7], residuals(out)[sat$expend>7])
var.test(residuals(out)[sat$salary<40], residuals(out)[sat$salary>40])
var.test(residuals(out)[sat$ratio<18], residuals(out)[sat$ratio>18])
var.test(residuals(out)[sat$takers<40], residuals(out)[sat$takers>40])

#The constant variance assumption is met


