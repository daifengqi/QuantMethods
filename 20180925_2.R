setwd("C:/Users/internet/Documents/GitHub/QuantitativeMethods/data")

# 获取数据
gnp <- scan(file='dgnp82.txt') # scan只能扫数值型数据
ord <- ar(gnp, method = 'mle') # 拟合AR模型

# 默认aic=TRUE，会使用aic来挑选变量
# ar(x, aic = TRUE, order.max = NULL,
#    method = c('yuan-walker', 'burg', 'ols', 'mle') ...)

# ARIMA模型
dat <- read.table('m-ibm3dx2608.txt', header = T) # 读入文档
vw <- dat[,3]
m3 <- arima(vw, order = c(3,0,0))

# 看结果
m3
(1-0.1158+0.0187+0.1042)*mean(vw)
m3$sigma2 # 残差平方和
ts.plot(m3$residuals)

# 检验残差是否是白噪声
Box.test(m3$residuals, lag=12, type='Ljung') 
# 手动调整 自由度 = 12 - 3 = 9
pv <- 1-pchisq(16.352,9) # 卡方分布的p值pchisq()
pv