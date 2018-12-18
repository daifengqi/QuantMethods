setwd('D://QuantMethods')
# 取利率数据
z1 <- read.table('data/w-tb3ms7097.txt', header = T)
z1 <- t(z1)
x <- z1[4, 1:1460]/100                   # 取利率
y <- (z1[4, 2:1461] - z1[4,1:1460])/100  # 取利率差（邻期）
# ts.plot(y)
par(mfrow=c(2,2))
plot(x, y, pch = '*', xlab = 'x(t-1)', ylab = 'y(t)') # 第一张图
lines(lowess(x,y))                   # 在原图上加线
title(main = '(a) y(t) v.s. x(t-1)') # 在原图上加标题

fit <- lowess(x,y) # 查看局部最小二乘的结果
# 调用局部最小二乘的值
plot(fit$x, fit$y, type = 'l', xlab = 'x(t-1)', ylab = 'mu',
     ylim = c(-0.002, 0.002))
title(main = '(b) mu(.) v.s. x(t-1)')
