# Linear Regression, plot fit line and label regression equation
rm(list=ls(all=TRUE))
setwd('C:/Sereda/Job/portfolio/R/Regression/')
#library(readxl)
df <- read.csv('fatality.csv')
head(df)

# 1. Plot a scatter of the beer tax (x axis) against the fatality rate (y axis), with a best fit
# line and reported coefficients. Is there a significant relationship between beer taxes
# and traffic fatalities?
x = df$beertax # Tax on Case of Beer
y = df$mrall # Vehicle Fatality Rate (VFR)
plot(x,y, xlab = "Beer tax", ylab="Vehicle Fatality Rate") # scatter plot

#fit a linear regression model
m1 <- lm(y ~ x, data = df)

#add the fitted regression line to the scatterplot
abline(m1)

# add text inside plot
reg_coefs1 = m1$coefficients
print(reg_coefs1)
txt = sprintf("y = %g + %g*x", summary(m1)$coefficients[1,1], summary(m1)$coefficients[2,1])
text(1, 0.0004, txt) # x, y, text

# 2. Run a fixed effects regression of fatality rate (y) on beer tax (x), with state and year
# level fixed effects.
m2 <- lm(y ~ x + df$state + 0 + df$year, data = df)
reg_coefs2 = m2$coefficients
# a)
txt = sprintf("FatalityRate = 0 + %f*BeerTax + %f*stateAL + ... + %f*stateWY + %.12f*year", summary(m2)$coefficients[1,1], summary(m2)$coefficients[2,1], summary(m2)$coefficients[49,1], summary(m2)$coefficients[50,1])
print(txt)

# (b) Describe one particular omitted variable that is now controlled for by including
# Answer: The year

# (c) Display the OLS and FE regressions in a table of regression output. Compared
#to the simple OLS regression, how did the coefficient on BeerTax change? Did it
#change in the direction you expected given the omitted variable you identified above?
print(reg_coefs2)
# Answer: the regression coefficient was 3.646e-5, and became -6.867e-5. It has changed not only the value but also the sign!
# Yes, BeerTax regression coefficient has changed from unexpected negative value to positive. It makes sense that the fatality rate decreases when beer tax increases, which promotes less alcohol consumption, especially among yound people.
