## BLACK-SCHOLES-MERTON Theroy

# BS公式下的欧式看涨期权定价
bscall_option <- function(S,X,rf,sigma,t){
  d1 <- (log(S/X)+(rf+0.5*sigma^2)*t)/(sigma*sqrt(t))
  d2 <- d1-sigma*sqrt(t)
  C <- S*pnorm(d1)-X*exp(-rf*t)*pnorm(d2)
  return(C)
}

# 例子
S=25;X=25;rf=0.08;sigma=0.3;t=0.5
bscall_option(S,X,rf,sigma,t)

# 反解波动率——利用数值解法:Newton Method
newton <- function(S,X,rf,t,op){
  accu <- 1.0e-4
  maxiter <- 100
  ts <- sqrt(t)
  sigma <- 1 # 迭代的初始值
  for(i in 0:maxiter){
    price <- bscall_option(S,X,rf,sigma,t)
    diff <- op - price
    if(abs(diff)<accu) return(sigma)
    else{
      d1 <- (log(S/X)+(rf+0.5*sigma^2)*t)/(sigma*sqrt(t))
      vega <- S*ts*pnorm(d1)
      sigma <- sigma + diff/vega
    }
  }
}

# Example
S=21;X=20;rf=0.1;t=0.25;op=1.875
newton(S,X,rf,t,op)
? pnorm

