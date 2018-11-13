setwd('D:/data')
library(nnet)

x <- scan('m-ibmln2699.txt')
y <- x[4:864]
ibm.x <- cbind(x[3:863], x[2:862], x[1:861])
ibm.n <- nnet(ibm.x, y, size = 2, linout = T, skip =T,
              maxit = 10000, decay = 1e-2, abstol=1e-7, rang=0.1) 
# size=2 中间有两个节点
# linout 线性输出
# skip输入输出直接连接
# decay 权重衰减
# abstol=最小容忍残差
# rang [-0.1,0.1]初始权重随机赋值范围
summary(ibm.n)
