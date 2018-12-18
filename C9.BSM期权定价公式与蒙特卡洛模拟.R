## BLACK-SCHOLES-MERTON Theroy

# BS公式下的欧式看涨期权定价
bscall_option <- function(S,X,rf,sigma,t){
  d1 <- (log(S/X)+(rf+0.5*sigma^2)*t)/(sigma*sqrt(t))
  d2 <- d1-sigma*sqrt(t)
  C <- S*pnorm(d1)-X*exp(-rf*t)*pnorm(d2)
  return(C)
}

# Example
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
      vega <- S*ts*dnorm(d1)
      sigma <- sigma + diff/vega
    }
  }
}

# Example
S=21;X=20;rf=0.1;t=0.25;op=1.875
newton(S,X,rf,t,op)
? pnorm
? dnorm


## Monte-Carlo 期权定价方法
payoff_call <- function(price,X){          # 期权回报（价值）
  return(max(0,price-X))
}

price_stimulate <- function(S,r,sigma,t){  # 股票价格的到期值（假设服从几何布朗运动）
  R <- (r-0.5*sigma^2)*t
  sd <- sigma*sqrt(t)
  return(S*exp(R+sd*rnorm(1)))
}

# 蒙特卡洛模拟
Monte_Carlo_european_call_option_pricing <- function(S,X,r,sigma,t,n){
  s_t <- numeric(n)
  sum_payoffs <- numeric(n)
  for(i in 1:n){
    s_t[i] <- price_stimulate(S,r,sigma,t)  # 产生n次股票价格的到期价格
    sum_payoffs[i] <- payoff_call(s_t[i],X) # 对应的期权价值
  }
  s <- mean(sum_payoffs)
  return(exp(-r*t)*s)
}

# Example
S=100;X=100;r=0.1;sigma=0.25;t=1;n=50000
res = Monte_Carlo_european_call_option_pricing(S,X,r,sigma,t,n)

## 这两个解应该是一样的
res                          # 数值模拟的解
bscall_option(S,X,r,sigma,1) # 公式解
## 用到数值解的方法都可以用蒙特卡洛的思想
