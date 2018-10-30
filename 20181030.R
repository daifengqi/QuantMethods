setwd('D://QuantMethods')
z1 <- read.table('data/w-tb3ms7097.txt', header = T)
z1 <- t(z1)
x <- z1[4, 1:1460]/100
