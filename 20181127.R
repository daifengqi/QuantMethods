library(tidyverse)            # 程序包：Tidyverse 极乐净土
setwd('D:/QuantMethods/data') # 默认路径

# ADS股票预测模型
dat <- read.table('ibm91-ads.dat', header = T)  # ADS数据
da1 <- read.table('ibm91-adsx.dat', header = T) # ADS滞后一阶数据
# 取出序列
Ai <- dat[,1] 
Di <- dat[,2]
Aim1 <- da1[,4]
Dim1 <- da1[,5]
# 拟合logistic模型
# 拟合Ai（股价是否改变）
m1 <- glm(Ai~Aim1, family = 'binomial')
summary(m1)
# 拟合Di（股价改变方向）
di <- Di[Ai==1]    # 取出Ai=1的地方（表示股价有改变）
dim1 <- Dim1[Ai==1]
di[di==-1] <- 0    # 逻辑回归模型只接受{1,0}
m2 <- glm(di~dim1, family = 'binomial')
summary(m2)

# 拟合Si的模型不再继续拟合

