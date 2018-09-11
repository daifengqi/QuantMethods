setwd('D:\\R/data')
library(fBasics) # 程序包：Rmetrics

# Intriduction ----------------------------------
dat <- read.table('m-ibm-6815.txt', header = T)
head(dat)    # 前6行展示
dim(dat)     # 数据维度
summary(dat) # 数据概况

ibm <- dat$RET # 取收益率
ts.plot(ibm,   # 画时序图
        main='Monthly IBM Simple Return: 1968-2015')   

lnibm <- log(ibm + 1) # 取对数收益率
ts.plot(lnibm)        # 画时序图
basicStats(lnibm)     # 显示基本统计量
mean(lnibm)           # 均值
t.test(lnibm)         # 假设检验（正态性）
normalTest(lnibm, method = 'jb') # 假设检验，JB统计量
s3 = skewness(lnibm)             # 峰度
tst = s3/sqrt(6/nrow(dat))       # 计算t统计量
pv = 2*pnorm(tst)                # 计算建设检验的p值

# Differencing ----------------------------------
n=100
t=1:n
x=5-2*t+3*t^2-4*t^3+10*rnorm(n) # 产生时序数据
d1=diff(x) # 做五次差分
d2=diff(d1)
d3=diff(d2)
d4=diff(d3)
d5=diff(d4)
par(mfrow=c(2,3)) # 创建画布
ts.plot(x) # 画5+1张图
ts.plot(d1)
ts.plot(d2)
ts.plot(d3)
ts.plot(d4)
ts.plot(d5)
