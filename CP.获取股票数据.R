install.packages('quantmod')
library(quantmod)
# 声明股票数据
setSymbolLookup(WK=list(name='000002.sz',src='yahoo',from="2013-01-01", to='2013-04-24'))
getSymbols("WK")# 获取数据
chartSeries(WK) # 作K线图

# 茅台
setSymbolLookup(GZMT=list(name='600519.ss',src='yahoo')) # 默认取近十年左右
getSymbols("GZMT")
chartSeries(GZMT) 

# 增加图示信息量
chartSeries(WK, up.col='red', dn.col='green', TA="addVo(); addMACD(); addSMA(n=10)")

# 导出
write.csv(WK, file = "foo.csv", row.names = F, quote = F)