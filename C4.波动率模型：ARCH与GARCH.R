setwd('D:QuantitativeMethods')
library(fGarch) # 程序包:Garch模型

dat <- read.table('data/m-intc7308.txt', header = T)
intc <- log(dat[, 2]+1)
Box.test(intc, lag=12, type = 'Ljung')

Box.test(intc^2, lag=12, type = 'Ljung') # 拒绝原假设
acf(intc^2)
pacf(intc^2) # 1,2,3,12都超过临界值，于是建立ARCH(1-3)的模型

# garch(m,0) m指的是滞后阶数，0指使用ARCH模型。
# trace指跟踪。是否看优化过程。
m1 <- garchFit(intc~garch(1,0), data=intc,trace = F)
summary(m1)
# 以上模型为：
# xt = u + rt
# rt = 6t + et
# 6t^2 = a0 + a1rt-1^2

# 更改滞后阶数
m2 <- garchFit(intc~garch(2,0), data=intc,trace = F)
summary(m2)
m3 <- garchFit(intc~garch(3,0), data=intc,trace = F)
summary(m3)

# 预测
predict(m1, 5)
# 关心方差（标准差）

# ARCH模型升级版：GARCH模型
# ~ garch(1,1) 第二个数字改成1就可以


#dat <- read.table('data/sp500.txt', header = T)
sp5 <- scan(file = 'data/sp500.txt')
# scan()读进来是个numeric向量
ts.plot(sp5)

# 与之前一样，看有没有GARCH效应
m1 <- garchFit(~arma(3,0) + garch(1,1), data = sp5,
               trace = F)
# 拟合一个arma型模型

stresi <- residuals(m1, standardize = T)
ts.plot(stresi)
Box.test(stresi, lag = 10,
         type = 'Ljung')
m1 <- garchFit(~garch(1,1), data = sp5,
               trace = F)
# 预测
predict(m1, 5)
