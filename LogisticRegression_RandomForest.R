library(randomForest)
library(class)

# Part 1: Classificattion
# 1. The data
data = read.csv("telemarketing.csv")

set.seed(1)
x = 1:nrow(data)
size = nrow(data)*0.8

train.index = sample(x,size,replace = FALSE)

train = data[train.index,]
test = data[-train.index,]
prop.table(summary(data$y))

# 2. Logistic regression
# A. Model building
model = glm(y~.,data=train,family="binomial")
summary(model)

# B. Interpreting coefficients
exp(coef(model))

# C. Model comparison
model.r = glm(y~duration+campaign+contact,data=train,family="binomial")
summary(model.r)

data.frame(full.model=AIC(model),reduced.model=AIC(model.r))

# D. Predictions
# 1.
pred.prob = predict(model,test,type="response")

pred.cl = ifelse(pred.prob>.5,"yes","no")
table(pred.cl)

c.matrix = table(test$y,pred.cl); c.matrix
acc = mean(pred.cl==test$y)
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens.yes,prec.yes)

# 2.
pred.cl2 = ifelse(pred.prob>.8,"yes","no")
table(pred.cl2)

c.matrix2 = table(test$y,pred.cl2); c.matrix2
acc2 = (c.matrix2[1]+c.matrix2[4])/sum(c.matrix2)
sens.yes2 = c.matrix2[4]/(c.matrix2[2]+c.matrix2[4])
prec.yes2 = c.matrix2[4]/(c.matrix2[3]+c.matrix2[4])
data.frame(acc2,sens.yes2,prec.yes2)

# 3. Random Forest
# 1.
set.seed(1)
reg.model = randomForest(y~.,data=train, importance = TRUE)
reg.model 

# 2.
pred.class = predict(reg.model,test,type = "class")
table(pred.class)

c.matrix = table(test$y,pred.class);  c.matrix
acc = mean(test$y==pred.class)
sens = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens,prec)

# 3.
importance(reg.model)
varImpPlot(reg.model,main="Variable Importance Plot")

