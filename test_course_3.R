
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
deathPay <- -150000
x <- 1150
n <-1000
p <- death_prob %>% filter(sex == "Female" & age == 50) %>% pull(prob) 
mu <- deathPay * p + x*(1-p)
mu
sigma <- abs(x-deathPay) * sqrt(p * (1-p))
E <- n * mu
SE <- sqrt(n) * sigma
E
SE
#CLT to calc prob of loosing money from this setup
pnorm(0, E, SE)

pMale <- death_prob %>% filter(sex == "Male" & age == 50) %>% pull(prob)
xMale <- 1460
muMale <- deathPay * pMale + xMale*(1-pMale)
sigmaMale <- abs(xMale-deathPay) * sqrt(pMale * (1-pMale))
EMale <- n * muMale
SEMale <- sqrt(n) * sigmaMale
#We want expected profit to be 700000 from 1000 insurances, what premium b?
b <- - (deathPay * pMale - 700)/(1-pMale)
b
SEMale
#CLT to calc prob of loosing money from this setup
pnorm(0, EMale, SEMale)

#Pandemic change deathrate to 0.015
pPan <- 0.015 
muPan <- deathPay * pPan + x*(1-pPan)
muPan
sigmaPan <- abs(x-deathPay) * sqrt(pPan * (1-pPan))
E <- n * muPan
SE <- sqrt(n) * sigmaPan
E
SE
pnorm(0, E, SE)
#Prob of loosing one million and going out of business
pnorm(-1000000, E, SE)

#What is the lowest death prob so that losing money exceeds 90%
prob <- seq(0.01, 0.03, 0.0025)
expectedValue <- deathPay * prob + x*(1-prob)
standardError <- abs(x-deathPay) * sqrt(prob * (1-prob))
sold1000E <- n *expectedValue
sold1000SE <- sqrt(n) *standardError
pnorm(0,sold1000E, sold1000SE)
#lowest prob of death so change loosing one million goes to 90%
pnorm(-1000000, sold1000E, sold1000SE)

#sampling model of previous
set.seed(25)
#what is loss if prob is 0.015 in millions
loss <- sample(c(-150000, 1150), 1000, replace = TRUE, prob = c(0.015, (1-0.015)))
sum(loss)/10^6
set.seed(27)
B <- 10000
monteCarlo <- replicate(B, {
  loss <- sample(c(-150000, 1150), 1000, replace = TRUE, prob = c(0.015, (1-0.015)))
  sum(loss)/10^6
})
#The probability of loosing 1 mill or more accoring to monte carlo
mean(monteCarlo <= -1 )

#What premium so that chance of loosing is 5% with death prob 0.15
p_default <- 0.015
z = qnorm(0.05)
x <- -deathPay*( n*p_default - z*sqrt(n*p_default*(1 - p_default)))/ ( n*(1 - p_default) + z*sqrt(n*p_default*(1 - p_default)))
x
muProfit <- deathPay * p_default + x*(1-p_default)
muProfit *1000

#monte carlo for above prob of loosing money
set.seed(28)
monteCarlo <- replicate(B, {
  loss <- sample(c(-150000, x), 1000, replace = TRUE, prob = c(0.015, (1-0.015)))
  sum(loss)
})
#The probability of loosing 1 mill or more accoring to monte carlo
mean(monteCarlo < 0 )

#changing deathrate during pandemic, that changes the probability of default for all borrowers simultaneously not independent!
set.seed(29)
profit <- replicate(B, {
  new_p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  loss <- sample(c(-150000, x), 1000, replace = TRUE, prob = c(new_p, (1-new_p)))
  sum(loss)
})
sum(profit)/10000
mean(profit < 0 )
mean(profit < -1000000 )
