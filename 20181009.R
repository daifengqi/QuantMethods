setwd('D:/QuantitativeMethods/data') # 默认路径

# 读入数据
dat <- read.table('m-intc7308.txt', header = T)

# log return: 检验自相关性
intc <- log(dat[,2]+1)
plot.ts(intc)
Box.test(intc, lag = 12, type = 'Ljung') # 滞后阶数为12
acf(intc)

# 方差（波动率）
at <- intc-mean(intc)
Box.test(at^2, lag = 12, type = 'Ljung') # 从1—12至少有一个不为零
plot.ts(at^2)