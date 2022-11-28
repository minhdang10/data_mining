#################################################
##################### SLR 3 #####################
#################################################

motor = read.table('Motorcycles.txt', sep = '\t', header = TRUE)

# 1. check linear by scaterplot
par(mfrow = c(2,2))

plot(motor$Wheelbase,motor$MSRP, 
     xlab = "Wheelbase", ylab = "MSRP",
     main = "Wheelbase & MSRP Scatterplot")
plot(motor$Displacement, motor$MSRP, 
     xlab = "Displacement", ylab = "MSRP",
     main = "Displacement & MSRP Scatterplot")
plot(motor$Bore, motor$MSRP, 
     xlab = "Bore", ylab = "MSRP",
     main = "Bore & MSRP Scatterplot")
plot(motor$Clearance, motor$MSRP, 
     xlab = "Clearance", ylab = "MSRP",
     main = "Clearance & MSRP Scatterplot")
model = lm(motor$MSRP~motor$Wheelbase, data = motor)
summary(model)
abline(model)

model1 = lm(motor$MSRP~motor$Displacement, data = motor)
summary(model1)
abline(model1)

model2 = lm(motor$MSRP~motor$Bore, data = motor)
summary(model2)
abline(model2)

model3 = lm(motor$MSRP~motor$Clearance, data = motor)
summary(model3)
abline(model3)

# 2. fit multiple model + report R^2, adjusted R^2 and coef
model = lm(MSRP~Displacement + Bore, data = motor)
summary(model)

#3. check model's assumption: residual plot, hist, normal QQ
#par(mfrow = c(1,2))
# plot(factor(motor$Displacement), motor$MSRP, xlab = 'Displacement', 
#      ylab = 'MSRP', main = 'Displacement & MSRP Boxplot') 
# plot(factor(motor$Bore), motor$MSRP, xlab = 'Bore', ylab = 'MSRP',
#      main = 'Bore & MSRP Boxplot')

par(mfrow = c(1,3))
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals Against Fitted Values")
abline(0,0)

hist(model$residuals, main = 'Residuals Histogram', xlab = 'Residuals')
qqnorm(model$residuals)
qqline(model$residuals)

# 4. hypo test based on summary

# 5. propose a new multiple regression and compare (r^2, coef, hypo test) 
# and assumption: try fitting with different predictors
model1 = lm(MSRP~Displacement + Bore + Clearance, data = motor)
summary(model1)
par(mfrow = c(1,3))
plot(model1$fitted.values, model1$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals Against Fitted Values")
abline(0,0)

hist(model1$residuals, main = 'Residuals Histogram', xlab = 'Residuals')

qqnorm(model1$residuals)
qqline(model1$residuals)

model2 = lm(MSRP~Displacement + Clearance, data = motor)
summary(model2)

par(mfrow = c(1,1))
qqnorm(model$residuals)
qqline(model$residuals)

qqnorm(model1$residuals)
qqline(model1$residuals)

# plot(motor$Wheelbase, motor$MSRP)
# plot((motor$Wheelbase)^2, motor$MSRP)
# plot(exp(motor$Wheelbase), motor$MSRP)
# plot(log10(motor$Wheelbase), motor$MSRP)
# plot(sqrt(motor$Wheelbase), motor$MSRP)
# plot(1/(motor$Wheelbase), motor$MSRP)
# plot(exp(-(motor$Wheelbase)), motor$MSRP)
# 
# plot(motor$Clearance, motor$MSRP)
# plot((motor$Clearance)^2, motor$MSRP)
# plot(exp(motor$Clearance), motor$MSRP)
# plot(log10(motor$Clearance), motor$MSRP)
# plot(sqrt(motor$Clearance), motor$MSRP)
# plot(1/(motor$Clearance), motor$MSRP)
# plot(exp(-(motor$Clearance)), motor$MSRP)

#################################################
##################### SLR 5 #####################
#################################################

pizza = read.table('Pizza_ratings.txt', sep = '\t', header = TRUE)

# 1. regression Score on Calories, Type and Fat. 
# Write down the fitted model. interpret the coefficient of Type? 
# Is that coefficient statistically significant? Explain.

model = lm(Score~Calories+Type+Fat, data=pizza)
summary(model)
str(pizza)
# pizza$Type =  as.factor(pizza$Type)
# str(pizza)
# pizza$Type
# library(lme4)

# plot its residuals against predicted values. any unusual observations? 
# identify them and find their standardized residuals, 
# leverages, Cook’s distances and DFFITS measures. 
# Are they influential pts? any other influential case you can find? 
# identify the corresponding pizza.
par(mfrow = c(1,3))

std.res = rstandard(model)
lev = hatvalues(model)

plot(model$fitted.values, model$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals Against Fitted Values")
abline(0,0)

hist(std.res, xlab = "Standardized Residuals", main = "Standardized Residuals Plot")
hist(lev, xlab = "Leverages", main = "Leverages Plot")

pizza[which(abs(std.res) >= 3), c(2, 4, 6, 5)] 
## c la column name; 2 la y

lev_cut = 3*length(model$coefficients)/dim(pizza)[1]
# 1 la n
pizza[which(lev > lev_cut), c(2, 4, 6, 5)]

cook_d = cooks.distance(model)
dffit = dffits(model)

par(mfrow = c(1,2))
boxplot(cook_d, main = "Cook's Distance Boxplot")
boxplot(dffit, main = "DFFITS Measures Boxplot")

which(cook_d > (4/dim(pizza)[1]))
#pizza[which.max(cook_d), c(2, 4, 6, 5)]

dffits_cut = 2*sqrt(length(model$coefficients)/
                      (dim(pizza)[1] - length(model$coefficients)))
which(abs(dffit) > dffits_cut)
#pizza[which.max(abs(dffit)), c(2, 4, 6, 5)]
pizza[12, ]
pizza[16, ]
pizza[29, ]

#abs(dffit)
#dffits_cut

#3. remove influential, refit, compare and check assumption
#influentiall = as.numeric(names(cook_d)[(cook_d > (4/dim(pizza)[1]))])

influential = as.numeric(names(dffit)[abs(dffit) > dffits_cut]); influential
pizza1 = pizza[-influential,]

model1 = lm(Score~Calories+Type+Fat, data=pizza1)

summary(model1)
summary(model)

par(mfrow = c(1,2))
plot(model1$fitted.values, model1$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals Against Fitted Values")
abline(0,0)
hist(model1$residuals, xlab = "Residuals", main = "Residuals Histogram")
# qqnorm(model1$residuals)
# qqline(model1$residuals)

# 4. Check for collinearity for Q3
library(car)
vif(model1)
# > 12 is collinearity

# 5. Use full pizza. Interaction between Calories/Type? Add it if neccessary, and interpret.
library(ggplot2)
pizza$Type = as.factor(pizza$Type)
ggplot(pizza, aes(Calories, Score, color = Type, shape = Type)) + 
  geom_point() + 
  geom_smooth(method=lm, se = FALSE, fullrange = TRUE)


model2 = lm(Score~Calories+Type+Fat+Calories*Type, data=pizza)
summary(model2)
str(pizza$Type)
