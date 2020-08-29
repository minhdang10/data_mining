library(tree)
library(class)

# 1. The Data
data = read.csv("telemarketing.csv")

set.seed(1)
x = 1:nrow(data)
size = nrow(data)*0.8

train.index = sample(x,size,replace = FALSE)

train = data[train.index,]
test = data[-train.index,]

prop.table(summary(data$y))
table(as.factor(data$y))

# 2. Decision Tree
# a.
dec_tree = tree(y~., data = train); summary(dec_tree)
plot(dec_tree)
text(dec_tree)

# b.
best.tree = cv.tree(dec_tree, K = 10)
best.tree

plot(best.tree$size, best.tree$dev, type = "b")

pruned.tree = prune.tree(dec_tree, best = 4)
plot(pruned.tree)
text(pruned.tree)

# c.
pred.class = predict(pruned.tree,test,type = "class")
c.matrix = table(test$y,pred.class);  c.matrix

acc = mean(test$y==pred.class)
sens = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec = c.matrix[4]/(c.matrix[3]+c.matrix[4])

data.frame(acc,sens,prec)

# 3. KNN
# a. Data preparation
summary(data)

normalize = function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}

norm.age = normalize(data$age)
norm.balance = normalize(data$balance)
norm.duration = normalize(data$duration)
norm.campaign = normalize(data$campaign)

norm.data = cbind(data[7],norm.age,norm.balance,data[,3:4],
                  norm.duration,norm.campaign)
summary(norm.data) 

norm.data$housing = as.character(norm.data$housing)

norm.data$housing[norm.data$housing=="no"] = 0 
norm.data$housing[norm.data$housing=="yes"] = 1 
str(norm.data$housing)

norm.data$cellular[norm.data$contact=="cellular"] = 1
norm.data$cellular[norm.data$contact!="cellular"] = 0

norm.data$telephone[norm.data$contact=="telephone"] = 1
norm.data$telephone[norm.data$contact!="telephone"] = 0

norm.data$unknown[norm.data$contact=="unknown"] = 1
norm.data$unknown[norm.data$contact!="unknown"] = 0

norm.data$contact = NULL
summary(norm.data) 

# b. Model building & comparison
set.seed(1)
x = 1:nrow(norm.data)
size = nrow(norm.data)*0.8

train.index = sample(x,size,replace = FALSE)

train = norm.data[train.index,]
test = norm.data[-train.index,]

train.x = train[,2:9]
test.x = test[,2:9]
train.cl = train[,1]

# set the stage for 2 K's = 3 and 5 
rep = seq(3,5,2)
rep.acc = rep
rep.sens = rep
rep.prec = rep
rep.FP = rep
rep.FN = rep

# index for 5-fold cv
set.seed(1)
k = 5
fold = sample(1:k,nrow(train.x),replace=TRUE)

iter = 1 # index for rep iteration
for (K in rep) {
  # space to store metrics from each iteration of 5-fold cv
  kfold.acc = 1:k
  kfold.sens = 1:k
  kfold.prec = 1:k
  kfold.FP = 1:k
  kfold.FN = 1:k
  for (i in 1:k) {
    #data for test and training sets
    test.kfold = train.x[fold==i,]
    train.kfold = train.x[fold!=i,]
    
    # class labels for test and training sets
    test.cl.actual = train.cl[fold==i]
    train.cl.actual = train.cl[fold!=i]
    
    # make predictions on class labels for test set
    pred.class = knn(train.kfold,test.kfold,train.cl.actual,k=K)
    
    # evaluate metrics for "yes"
    c.matrix = table(test.cl.actual,pred.class)
    acc = mean(pred.class==test.cl.actual)
    sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
    prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
    FP = c.matrix[3]
    FN = c.matrix[2]
    # store result for each k-fold iteration
    kfold.acc[i] = acc
    kfold.sens[i] = sens.yes
    kfold.prec[i] = prec.yes
    kfold.FP[i] = FP
    kfold.FN[i] = FN
  }
  
  # store average k-fold perfomance for each KNN model
  rep.acc[iter] = mean(kfold.acc)
  rep.sens[iter] = mean(kfold.sens)
  rep.prec[iter] = mean(kfold.prec)
  rep.FP[iter] = mean(FP)
  rep.FN[iter] = mean(FN)
  iter = iter+1
}
results = as.data.frame(cbind(rep,rep.acc,rep.sens,rep.prec,rep.FP,
                              rep.FN))
names(results) = c("K","accuracy","sensitivity","precision",
                   "false positives","false negatives")
results

# c. Predictions
pred.class = knn(train.x,test.x,train.cl,k=5)
c.matrix = table(test$y,pred.class)
c.matrix

acc = mean(pred.class==test$y)
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
as.data.frame(cbind(acc,sens.yes,prec.yes))
