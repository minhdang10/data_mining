# Data preparation
# 1.
library(ISLR)

# 2.
str(College)
College$Private = NULL
str(College)
sum(is.na(College))

# 3. 
scale.college = scale(College)
summary(scale.college)

# K-means clustering
# 1.
set.seed(1) 
km = kmeans(scale.college, 2, nstart = 10)

# 2. 
km$tot.withinss

# 3.
clust.data = cbind(cluster = km$cluster, College);
head(clust.data)
aggregate(.~cluster, data =  clust.data, FUN = mean)
head(clust.data[clust.data$cluster==2,])

# Hierarchical clustering
# 1. 
hc.dist = dist(scale.college)
hc = hclust(hc.dist, method = "complete"); hc
plot(hc)

# 2.
abline(h = 2,col = "red")
hc.cut = cutree(hc, 2); hc.cut
table(hc.cut)

# 3.
hc.data = cbind(cluster = hc.cut,College)
aggregate(.~cluster, data = hc.data, FUN = mean)
head(hc.data[hc.data$cluster==2,])

# 4. 
hc.data["Boston University",]

