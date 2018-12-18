setwd("D:/QuantMethods/data")
x <- read.table('m-fac9003.txt', header = T) #前13列是股票，最后一列是市场

### 多元回归
# 自变量
xmtx <- cbind(rep(1,168), x[,14]) # 添加一列截距项
# 因变量(多元)
rtn <- as.matrix(x[,1:13])
# 系数估计
xit.hat <- solve(t(xmtx)%*%xmtx)%*%t(xmtx)%*%rtn # 代入回归系数的计算公式(矩阵表达式)
beta.hat <- t(xit.hat[2,]) #  取回归系数(第一行是截距，第二行是系数)
# 残差
E.hat <- rtn - xmtx%*%xit.hat
# 残差的方差
D.hat <- diag(crossprod(E.hat)/(168-2)) # 用公式求方差,168是个数T,减去1变量,再减1,就是自由度
# 可决系数
r.square <- 1-(168-2)*D.hat/diag(t(rtn)%*%rtn)
## 上式同理于 r.square <- 1-diag(crossprod(E.hat))/diag(t(rtn)%*%rtn)

# 查看结果
t(rbind(beta.hat, sqrt(D.hat), r.square))
# 第一列：对市场的依赖程度；股票的Beta。
# 第二列：拟合回归的MSE。
# 第三列：市场因子对股票的解释程度，即拟合好坏。



### PCA
rtn <- read.table("m-5clog-9008.txt", header = T) # 5只股票的数据
pca.cov <- princomp(rtn) # 主成分分析
names(pca.cov)           # 查看结果包含的内容
pca.cov$sdev       # 特征根
summary(pca.cov)   # 结果汇总
pca.cov$loadings   # 权重(特征向量)
screeplot(pca.cov) # 碎石图
# 使用相关系数矩阵做PCA
pca.corr <- princomp(rtn, cor=T)
## 其他使用方式同上
summary(pca.corr)
