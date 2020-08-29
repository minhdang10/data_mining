# Getting started
install.packages("ISLR")
library(ISLR)

?College
str(College)

# 1.
model = lm(Grad.Rate~S.F.Ratio, data = College)
summary(model)

# b.
plot(College$S.F.Ratio, College$Grad.Rate, 
     xlab = "Student-Faculty Ratio",
     ylab = "Graduation Rate", 
     main = "Student-Faculty Ratio & Graduation Rates Scatter Plot")
abline(model, col = "red")

# d.
model = lm(Grad.Rate~S.F.Ratio+I(S.F.Ratio^2),data=College)
summary(model)

# e.
model = lm(Grad.Rate~S.F.Ratio*Apps,data=College)
summary(model)

# 2.
model = lm(Grad.Rate~Private+Top25perc+Outstate+Room.Board, data=College)
summary(model)

# c.
confint(model)

# d.
Private = "No"
Outstate = 25000
Room.Board = 4000
Top25perc = 55
new_obs = data.frame(Private, Outstate, Room.Board, Top25perc)
predict(model, new_obs, interval = "prediction")

# e.
par(mfrow=c(2,2))
plot(model)

# 3.
model = lm(Grad.Rate~., data=College)
summary(model)

# b.
install.packages("leaps")
library(leaps)

model_fwd = regsubsets(Grad.Rate~., data=College, nvmax = NULL, 
                       method = "forward")
summary(model_fwd)

model_fwd_summary = summary(model_fwd)
which.max(model_fwd_summary$adjr2)
summary(model_fwd)$which[13,]

# c.
best_model_fwd = lm(Grad.Rate~Private+Apps+Top10perc+Top25perc+F.Undergrad
                    +P.Undergrad+Outstate+Room.Board+Personal+PhD+Terminal
                    +perc.alumni+Expend, data = College)
summary(best_model_fwd)
anova(model, best_model_fwd)

# d. 
plot(model_fwd, scale = "adjr2", main = "Forward Selection: AdjR2")
