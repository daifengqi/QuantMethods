# Factor Analysis
setwd('E:/Github/repository/QuantMethods/data')

da <- read.table('m-tenstocks.txt', header = T)
# 取10只股票的对数收益率
rtn <- log(da[,2:11]+1)
# 取每串时间序列的标准差
std <- diag(1/sqrt(diag(cov(rtn))))
# 将收益率标准化
rtns <- as.matrix(rtn)%*%std
# 主成分分析
m1 <- princomp(rtns) # 谱分解
names(m1)
# 取特征根和特征向量
sdev <- m1$sdev  # 特征根的标准差
M <- m1$loadings # 特征向量(权重)
summary(m1)

# 碎石图
screeplot(m1) # 每个特征根解释方差的变化
# 提取前三个作为β
SD <- diag(sdev[1:3])
L=M[,1:3]%*%SD # 特征向量×特征根的标准差=因子
print(round(L,3)) # 打印 公共因子
LLt <- L%*%t(L)     # 协方差矩阵
diag(LLt)           # 每只股票收益率的方差
SigE <- 1-diag(LLt)
SigE
