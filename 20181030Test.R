library(quantmod)
# 宝钢股份股票(600019.ss)
# 声明股票数据
setSymbolLookup(BG=list(name='600019.ss',src='yahoo',from="2018-01-01", to='2018-10-25'))
getSymbols("BG")# 获取数据
chartSeries(BG) # 作K线图

