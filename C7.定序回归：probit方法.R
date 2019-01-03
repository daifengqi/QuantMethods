setwd('E://Github/repository/QuantMethods/data')
dat <- read.table('taq-cat-t-jan042010.txt', header = T)
head(dat)

# Ordered Probit Regression 定序回归模型
# 取序列
vol <- dat$size/100
dat1 <- read.table('taq-cat-cpch-jan042010.txt', header = T)
cpch <- dat1[,1]
pch <- dat1[,2]
cf <- as.factor(cpch)
length(cf)
y <- cf[4:37715]
y1 <- cf[3:37714]
y2 <- cf[2:37713]
vol <- vol[2:37716]
v2 <- vol[2:37713]
cp1 <- pch[3:37714]
cp2 <- pch[2:37713]
cp3 <- pch[1:37712]
# 模型形式
library(MASS)
# probit函数拟合
m1 <- polr(y~ v2+cp1+cp2+cp3+y1+y2, method = 'probit')
summary(m1)
names(m1)

# 模型效果检查
yhat <- m1$fitted.values
print(yhat[1:5,], digits = 3) # digits:保留三个有效数字
# 这是y取到每个值的概率
