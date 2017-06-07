# Analysis of Airline Ticket Pricing
# NAME: Aditya Sharma
# EMAIL: adityathc@gmail.com
# COLLEGE / COMPANY: NIT Jaipur


# Setting up working directory
setwd("C:/Users/aditya/Downloads/Internship 2017/R directory")

# Reading CSV file into R and creating airline data frame
airline <- read.csv("SixAirlines.csv")

# Renaming AIRLINE's name into numeric values and changing class ( for easy correlation matrix)
library(car)
airline$AIRLINE <- recode(airline$AIRLINE, "'AirFrance'=1;'British'=2;'Delta'=3;'Jet'=4;'Singapore'=5;'Virgin'=6")
airline$AIRLINE <- as.numeric(as.character(airline$AIRLINE))

# Adding new column "diffPRICE" which is difference between Premium Economy Price and Economy Price ticket
airline$diffPRICE <- with(PRICE_RELATIVE*PRICE_ECONOMY, data = airline)

# Adding new column "diffWIDTH" which is difference between Premium Economy Width and Economy Width
airline$diffWIDTH <- with(WIDTH_PREMIUM-WIDTH_ECONOMY, data = airline)

# Renaming the columns "QUALITY" to "diffPITCH" because quality is diff between width of both class
names(airline)[17] <- "diffPITCH"

# Visualizing the structure of data frame
str(airline)

# Summarizing the data to understand the mean, median, standard deviation
summary(airline)

# Drawing various boxplots to visualize varibales individually 
boxplot(airline$AIRLINE)
boxplot(airline$AIRCRAFT)
boxplot(airline$FLIGHT_DURATION)
boxplot(airline$MONTH)
boxplot(airline$INTERNATIONAL)
boxplot(airline$SEATS_ECONOMY)
boxplot(airline$SEATS_PREMIUM)
boxplot(airline$PITCH_ECONOMY)
boxplot(airline$PITCH_PREMIUM)
boxplot(airline$WIDTH_ECONOMY)
boxplot(airline$WIDTH_PREMIUM)
boxplot(airline$PRICE_ECONOMY)
boxplot(airline$PRICE_PREMIUM)
Boxplot(airline$PRICE_RELATIVE)
boxplot(airline$N)
boxplot(airline$LAMBDA)
boxplot(airline$QUALITY)

# Drawing scatterplot and boxplots between two variables to check pair-wise correlation
boxplot(airline$SEATS_ECONOMY ~ airline$SEATS_PREMIUM)
boxplot(airline$PITCH_ECONOMY ~ airline$PRICE_ECONOMY)
boxplot(airline$diffPRICE ~ airline$diffPITCH)
plot(x=airline$SEATS_ECONOMY, y=airline$SEATS_PREMIUM)
plot(x=airline$PRICE_PREMIUM, y=airline$PRICE_ECONOMY)
plot(x=airline$PRICE_PREMIUM, y=airline$WIDTH_PREMIUM)
plot(x=airline$diffPRICE, y=airline$diffWIDTH)
plot(x=airline$PRICE_PREMIUM, y=airline$LAMBDA)

# Drawring a Corrgram between each variable of airline dataset (shade-pie and scatterplot-ellipse)
library(corrgram)
corrgram(airline, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main="Corrgram of airline intercorrelation")
corrgram(airline, order = TRUE, lower.panel = panel.ellipse, upper.panel = panel.pts, text.panel = panel.txt, diag.panel = panel.minmax, main="Corrgram using scatterplots of airline intercorrelation")

# Variance, Covariance, Correlation matrix respectively of airline data frame
var(airline)
cov(airline)
cor(airline)

# Hypotheis testing between correlated variables using cor.test and t.test
cor.test(airline$INTERNATIONAL ,airline$PRICE_RELATIVE)
cor.test(airline$FLIGHT_DURATION ,airline$PRICE_PREMIUM)
cor.test(airline$PITCH_PREMIUM ,airline$WIDTH_PREMIUM)
cor.test(airline$PRICE_RELATIVE ,airline$WIDTH_PREMIUM)
cor.test(airline$INTERNATIONAL ,airline$PRICE_RELATIVE)
cor.test(airline$INTERNATIONAL ,airline$PRICE_RELATIVE)
cor.test(airline$SEATS_PREMIUM ,airline$N)


t.test(airline$diffPITCH, airline$diffWIDTH)
t.test(airline$diffPRICE, airline$diffWIDTH)
t.test(airline$diffPITCH, airline$diffPRICE)
t.test(airline$FLIGHT_DURATION, airline$INTERNATIONAL)
t.test(airline$diffPRICE, airline$LAMBDA)
t.test(airline$diffPITCH, airline$LAMBDA)
t.test(airline$FLIGHT_DURATION, airline$LAMBDA)
t.test(airline$diffPITCH, airline$diffWIDTH)

# Regression model 1 ( for only understanding price premium economy)
model1 <- lm(PRICE_PREMIUM ~ PRICE_ECONOMY + FLIGHT_DURATION + INTERNATIONAL + diffWIDTH + diffPITCH + LAMBDA, data = airline)
summary(model1)

# Model 1 - removing explanatory variables whose beta-coefficients are not statistically significant (p > 0.05)
model1A <- lm(PRICE_PREMIUM ~ PRICE_ECONOMY + FLIGHT_DURATION + diffWIDTH  + LAMBDA, data = airline)
summary(model1A)

# Final linear regression model - describing diff in Premuim economy and economy price
model2 <- lm(diffPRICE ~ FLIGHT_DURATION + INTERNATIONAL + diffWIDTH + diffPITCH + LAMBDA  ,data = airline)
summary(model2)

# Model 2 - removing explanatory variables whose beta-coefficients are not statistically significant (p > 0.05)
model2A <- lm(diffPRICE ~ FLIGHT_DURATION + diffWIDTH + LAMBDA  ,data = airline)
summary(model2A)

# Comparing model 2 and 2A
anova(model2, model2A)

# Plotting the residual vs regression fit
plot(model2)
plot(model2A)

# Analyzing Fitted values and resudials matrix
fitted(model2)

residuals(model2)
residuals(model2A)

# 95 % confidence intervals of fitted variables
confint(model2)
confint(model2A)

