#################################################
##################### SLR 1 #####################
#################################################

cost = read.table('Cost_of_Living_2013.txt', sep = '\t', header = TRUE)

# 1. a scatterplot between the Cost of Living Index and EACH index variables.
par(mfrow = c(2,2))

plot(cost$Cost.of.Living.Index, cost$Rent.Index, xlab = "Cost of Living Index", 
     ylab = "Rent Index", main = "Cost of Living & Rent Indices Scatterplot (1)")

model1 = lm(Cost.of.Living.Index~Rent.Index, data = cost)
abline(model1, col="red")

plot(cost$Cost.of.Living.Index, cost$Groceries.Index, 
     xlab = "Cost of Living Index", ylab = "Groceries Index", 
     main = "Cost of Living & Groceries Indices Scatterplot (2)")

model2 = lm(Cost.of.Living.Index~Groceries.Index, data = cost)
abline(model2, col="red")

plot(cost$Cost.of.Living.Index, cost$Restaurant.Price.Index, 
     xlab = "Cost of Living Index", ylab = "Restaurant Price Index", 
     main = "Cost of Living & Restaurant Price Indices Scatterplot (3)")
model3 = lm(Cost.of.Living.Index~Restaurant.Price.Index, data = cost)
abline(model3, col="red")

plot(cost$Cost.of.Living.Index, cost$Local.Purchasing.Power.Index, 
     xlab = "Cost of Living Index", ylab = "Local Purchasing Power Index", 
     main = "Cost of Living & Local Purchaing Power Indices Scatterplot (4)")
model4 = lm(Cost.of.Living.Index~Local.Purchasing.Power.Index, data = cost)
abline(model4, col="red")

# 2. Compute correlation coefficients
cor(cost$Cost.of.Living.Index, cost$Rent.Index)
cor(cost$Cost.of.Living.Index, cost$Groceries.Index)
cor(cost$Cost.of.Living.Index, cost$Restaurant.Price.Index)
cor(cost$Cost.of.Living.Index, cost$Local.Purchasing.Power.Index)

# 3. Verify the conditions of each in #2

# 4. Fit a linear regression model between the Cost of Living Index and EACH 
model1 = lm(Cost.of.Living.Index~Rent.Index, data = cost)
summary(model1)
model2 = lm(Cost.of.Living.Index~Groceries.Index, data = cost)
summary(model2)
model3 = lm(Cost.of.Living.Index~Restaurant.Price.Index, data = cost)
summary(model3)
model4 = lm(Cost.of.Living.Index~Local.Purchasing.Power.Index, data = cost)
summary(model4)

# 5. What items would be the best predictors of overall cost

# 6. Find the cost of living as predicted by Groceries Index and its residual 
# for Beijing, China.

ri=which(cost$City=='Beijing, China')
ri
model2$fitted.values[ri]
model2$residuals[ri]

# plot(model1)
# plot(model2)
# plot(model3)
# plot(model4)

#################################################
##################### SLR 2 #####################
#################################################

pizza = read.table('Frozen_Pizza.txt', sep = '\t', header = TRUE)

# 1. 
baltimore = lm(Baltimore.Volume~Baltimore.Price, data = pizza)
summary(baltimore)
dallas = lm(Dallas.Volume~Dallas.Price, data = pizza)
summary(dallas)
chicago = lm(Chicago.Volume~Chicago.Price, data = pizza)
summary(chicago)
denver = lm(Denver.Volume~Denver.Price, data = pizza)
summary(denver)

summary(pizza$Dallas.Volume)
# 2.
par(mfrow = c(1,3))
plot(pizza$Week, baltimore$residuals, xlab = "Time", ylab = 'Residual',
     main = "Baltimore's Residual Plot in the Time Order")
abline(a = 0, b = 0)
plot(baltimore$fitted.values, baltimore$residuals, xlab = 'Fitted Values',
     ylab = 'Residual', main = "Baltimore's Residual Plot Against the Fitted Values")
abline(a = 0, b = 0)
qqnorm(baltimore$residuals, main = "Baltimore's Normal Q-Q Plot")
qqline(baltimore$residuals)

plot(pizza$Week, dallas$residuals, xlab = "Time", ylab = 'Residual',
     main = "Dallas's Residual Plot in the Time Order")
abline(a = 0, b = 0)
plot(dallas$fitted.values, dallas$residuals, xlab = 'Fitted Values',
     ylab = 'Residual', main = "Dallas's Residual Plot Against the Fitted Values")
abline(a = 0, b = 0)
qqnorm(dallas$residuals, main = "Dallas's Normal Q-Q Plot")
qqline(dallas$residuals)

plot(pizza$Week, chicago$residuals, xlab = "Time", ylab = 'Residual',
     main = "Chicago's Residual Plot in the Time Order")
abline(a = 0, b = 0)
plot(chicago$fitted.values, chicago$residuals, xlab = 'Fitted Values',
     ylab = 'Residual', main = "Chicago's Residual Plot Against the Fitted Values")
abline(a = 0, b = 0)
qqnorm(chicago$residuals, main = "Chicago's Normal Q-Q Plot")
qqline(chicago$residuals)

plot(pizza$Week, denver$residuals, xlab = "Time", ylab = 'Residual',
     main = "Denver's Residual Plot in the Time Order")
abline(a = 0, b = 0)
plot(denver$fitted.values, denver$residuals, xlab = 'Fitted Values',
     ylab = 'Residual', main = "Denver's Residual Plot Against the Fitted Values")
abline(a = 0, b = 0)
qqnorm(denver$residuals, main = "Denver's Normal Q-Q Plot")
qqline(denver$residuals)

# 3.
confint(dallas, level = 0.90)
#cor(pizza$Dallas.Price,pizza$Dallas.Volume)

# 5. 
range(pizza$Dallas.Price)
new = data.frame(Dallas.Price = c(2.50, 3.00))
predict(dallas, newdata = new, interval = 'confidence', level = 0.95)

# 6.
predict(dallas, newdata = data.frame(Dallas.Price = c(2.99)), 
        interval = 'prediction', level = 0.95)

plot(pizza$Dallas.Price,pizza$Dallas.Volume)
abline(dallas, col='red')

range(pizza$Dallas.Volume)

#################################################
##################### SLR 3 #####################
#################################################

gdp = read.table('GDP.txt', sep = '\t', header = TRUE)

# 1. 
par(mfrow = c(1,2))
hist(GDP$Personal.Income, xlab = "Personal Income", 
     main = "Histogram of Residuals")
qqnorm(GDP$Personal.Income)
qqline(GDP$Personal.Income)

par(mfrow = c(1,2))
hist(log10(GDP$Personal.Income), xlab = expression(Log[10](Personal.Income)), 
     main = "Histogram of Residuals")
qqnorm(log10(GDP$Personal.Income))
qqline(log10(GDP$Personal.Income))

# 2.
plot(GDP$GDP, log10(GDP$Personal.Income), 
     xlab = "GDP", ylab = expression(Log[10](Personal.Income)),
     main = "Transformed Personal Income & GDP Scatterplot")

par(mfrow = c(1,2))
plot(sqrt(GDP$GDP), log10(GDP$Personal.Income),xlab = expression(sqrt(GDP)), 
     ylab = expression(log[10](Personal.Income)), 
     main = "Transformed Personal Income & Transformed GDP Scatterplot")
plot(log10(gdp$GDP), log10(gdp$Personal.Income),xlab = expression(log10(GDP)), 
     ylab = expression(log[10](Personal.Income)),
     main = "Transformed Personal Income & Transformed GDP Scatterplot")

# 3.
model = lm(log10(Personal.Income) ~ log10(GDP), data = gdp)
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals Against Fitted Values")
abline(a = 0, b = 0)

# 4. 
outlier_1 = which.min(model$residuals); outlier_1
gdp[which.min(model$residuals), ]
outlier_2 = which(model$residuals == sort(model$residuals)[2]); outlier_2

gdp[which.min(model$residuals), ]
gdp[which(model$residuals == sort(model$residuals)[2]), ]

gdp_1 = gdp[-which.min(model$residuals), ]
model_1 = lm(log10(Personal.Income) ~ log10(GDP), data = gdp_1)
summary(model)
summary(model_1)

mean(model$fitted.values)

# gdp_2 = gdp[-outlier_2, ]
# model_2 = lm(log10(Personal.Income) ~ log10(GDP), data = gdp_2)
# summary(model_2)
# 
# gdp_3 = gdp_1[-outlier_2, ]
# model_3 = lm(log10(Personal.Income) ~ log10(GDP), data = gdp_3)
# summary(model_3)

# 6.
summary(model)
#new_obs = data.frame(GDP = 300000)
#new_obs
pred = predict(model, newdata = data.frame(GDP = 300000), 
               interval = 'prediction', level = 0.95) 
pred
10^(pred)
