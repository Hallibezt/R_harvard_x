library(gtools)
library(dslabs)
library(tidyverse)

qnorm(0.01)
p <- .04 #Rate of defaults
loss_per_foreclosure <- -200000
r <- 0.05 #intrest rate
x <- r *180000
x #intrest every loan
mu <- loss_per_foreclosure*p + x*(1-p)
sigma <- abs(x-loss_per_foreclosure) * sqrt(p * (1-p))#SE[X] is |x-l|* sqrt(p * (1-p))
sigma #Vægið er hátt +-40955 til að minnka vægið(minni líkur á tapi), fá n nógu stórt - law of large numbers
mu #Expected value from each loan
n <- ceiling(qnorm(0.01)^2 * sigma^2 / mu^2) #ceiling is to round it up 
n # Í þessum deal þarf 22163 lán til þess að vera öruggur með 1% áhættu á tapi
n * mu #og ávinningurinn eru 14 milljónir dollara!!

#The problem with this is CLT only works if independent X, per loan, is an independent draw!! Then large n can make sigma effect small
#If the sigma is not independent then the Standard Error is much larger, sqrt(n) large!!!!
#Here is a Monte Carlo very changes in default affect all loans - no longer each loan independent
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
?hist()
