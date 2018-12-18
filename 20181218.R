setwd("D:/QuantMethods/data")
x <- read.table('m-fac9003.txt', header = T) #前13列是股票，最后一列是市场

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
# 第一列：对市场的依赖程度。
# 第二列：
# 第三列：市场因子对股票的解释程度，即拟合好坏。