setwd("C:/Users/internet/Documents/GitHub/QuantitativeMethods/data")

# 获取数据
gnp <- scan(file='dgnp82.txt') # scan只能扫数值型数据
ord <- ar(gnp, method = 'mle') # 拟合AR模型

# 默认aic=TRUE，会使用aic来挑选变量
# ar(x, aic = TRUE, order.max = NULL,
#    method = c('yuan-walker', 'burg', 'ols', 'mle') ...)
