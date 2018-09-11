setwd('D:\\R/data')
library(fBasics) # 程序包：Rmetrics

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