setwd("C:/Users/internet/Documents/GitHub/QuantitativeMethods/data")


dat <- read.table('m-ibm3dx2608.txt', header = T) # 读入数据
sibm <- dat[,2] # 取return
ts.plot(sibm) # 做时间序列图

# ACF & PACF
acf(sibm)
pacf(sibm)

# 是否满足ACF的假设检验
Box.test(sibm, lag = 5, type = 'Ljung')
libm <- log(sibm + 1)
Box.test(libm, lag = 5, type = 'Ljung')