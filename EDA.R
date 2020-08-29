# Section 1
# 1.
conn = url("http://guides.newman.baruch.cuny.edu/ld.php?content_id=39953204")
fb = read.csv(conn, sep = ";")

# 2.
str(fb)
# There are 500 observations (rows) with 19 variables (columns) 
# or we are having the dimensions of 19 x 500 dataset
for (col in names(fb)[2:7]){
  fb[,col] = as.factor(fb[,col])
}

# 3.
levels(fb$Category) =  c("action", "product", "inspiration")
levels(fb$Category)

# 4.
levels(fb$Paid) =  c("non-paid", "paid")
levels(fb$Paid)

# 5. 
print(sum(is.na(fb)))
# There are a total of 6 missing values
fb = na.omit(fb)

# Section 2
# 1.
summary(fb$share)
sd (fb$share)

# 2.
hist(fb$share, xlab = "Number of Shares", ylab = "Number of Posts", 
     main = "Shares Histogram", breaks = 100)
hist(fb$share, xlab = "Number of Shares", ylab = "Number of Posts", 
     main = "Shares Histogram", breaks = 100, xlim = c(0,200))

# 3.
summary(fb$Post.Month)
plot(fb$Post.Month,xlab = "Months", ylab = "Number of Posts", 
     main = "Posting Behavior by Month")
summary(fb$Category)
plot(fb$Category,xlab = "Category", ylab = "Number of Posts", 
     main = "Post Category")
summary(fb$Paid)
prop.table(summary(fb$Paid))

# Section 3
# 1.
plot(fb$share, fb$like, xlab = "Number of Shares", ylab = "Number of Likes",
     main = "Shares and Likes Scatterplot",xlim = c(0,210),ylim = c(0,2000))

# 2.
plot(fb$Post.Month, fb$share, xlab = "Number of Posts by Month", 
     ylab = "Number of Shares",  main = "Shares by Month",ylim = c(0,210))

# 3. 
xtabs(share~Paid, data = fb) 
aggregate(share~Paid, data = fb, summary)

plot(fb$Paid,fb$share, ylab = "Number of Shares", 
        main = "Paid and Non-Paid Posts Performance",ylim = c(0,210))

g1 = fb$share[fb$Paid == "paid"]
g2 = fb$share[fb$Paid == "non-paid"]
t.test(g1,g2)

# Section 4
par(mfrow = c(1,3))

x1 = fb$Paid[fb$Category == "action"]
y1 = fb$share[fb$Category == "action"]
plot(x1,y1,main = "Action", ylim = c(0,210))

x2 = fb$Paid[fb$Category == "product"]
y2= fb$share[fb$Category == "product"]
plot(x2,y2,main = "Product", ylim = c(0,210))

x3 = fb$Paid[fb$Category == "inspiration"]
y3= fb$share[fb$Category == "inspiration"]
plot(x3,y3,main = "Inspiration", ylim = c(0,210))

g3 = fb$share[fb$Category == "product" & fb$Paid == "paid"]
g4 = fb$share[fb$Category == "product" & fb$Paid == "non-paid"]
t.test(g3,g4)
