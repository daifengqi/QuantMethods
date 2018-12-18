library(quantmod) # 程序包：股票量化
library(tseries)  # 程序包：时序分析
library(fGarch)   # 程序包:Garch模型
# 宝钢股份股票(600019.ss)
# 声明股票数据
setSymbolLookup(BG=list(name='600019.ss',src='yahoo',from="2018-01-01", to='2018-10-25'))
getSymbols("BG")# 获取数据
chartSeries(BG) # 作K线图

# 获取股票价格数据
bg <- as.data.frame(BG)                   # 转换为数据框
bgprice <- bg[,4]                         # 获取收盘价
# 从收益率开始分析
rt <- log(bgprice[2:197]/bgprice[1:196])  # 用收盘价计算对数收益率
ts.plot(rt)  # 查看收益率时序图
adf.test(rt) # 通过平稳性检验

# plot ACF & PACF
acf(rt)  # 相关性都不大
pacf(rt) # 六阶超过了临界值
# 用lag=6的Box检验
Box.test(rt, lag = 6, type = 'Ljung')
# p=0.1125，即接受原假设，没有相关性
# 所以不能用AR/AR(6)来拟合模型

# 接下来讨论方差（波动率）和非线性模型
# 从20181016开始讲波动率模型
at <- rt-mean(rt)
acf(at^2)
pacf(at^2)
Box.test(at^2, lag=12, type = 'Ljung') # 不能拒绝原假设
# 没有ARCH效应，重新选股票的时间范围

# 20181030局部最小二乘
