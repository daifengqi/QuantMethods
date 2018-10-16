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


