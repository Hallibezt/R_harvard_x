library(gtools)
library(dslabs)
library(tidyverse)

#SAT -0.25 for wrong, 1 for right with 5 choices and 44 questions
  #THE CLT part
  #One right ap+b(1−p)
    prob_right <- 1 * 1/5
    prob_wrong <- 1 - prob_right
  #Expected value for guessing one question
    one_right <- (1*prob_right) + (-0.25 * (1-prob_right)) 
    one_right
  #Expected score on all 44
    avg <- 44 * one_right # sem sagt 0
  # Standard Error of 44
    single_error <- abs(-0.25 - 1) * sqrt(prob_right*(1-prob_right))
    single_error
    se <- sqrt(44) * single_error    
    1-pnorm(8, avg, se)
  #THE MONTE CARLO PART
    set.seed(21)
    B <- 10000
    monteCarlo <- replicate(B, {
      X <- sample(c(1, -0.25), 44, replace = TRUE, prob = c(1/5, 4/5))
      sum(X)
    })
    mean(monteCarlo >= 8)    
    
    #New SAT no penalty, 1 for right with 4 choices and 44 questions
    #One right ap+b(1−p)
    prob_right1 <- 1 * 1/4
    prob_right1
    prob_wrong1 <- 1 - prob_right1
    #Expected value for guessing one question
    one_right1 <- (1*prob_right1) + (0 * (1-prob_right1)) 

    #Expected score on all 44
    avg <- 44 * one_right1 # sem sagt 0
    avg
    #What is the lowest p such that the probability of scoring over 35 exceeds 80%?
    p <- seq(0.25, 0.95, 0.05) 
    one_right2 <- (1*p) + (0 * (1-p)) 
    avg2 <- 44 * one_right2
    avg2
    quantile(avg2, p)    
    
    #Betting on Roulette bet on 5/38 is a win +6 and 33/38 loose -1
    n <- 500 #500 BETS
    p_win <- 5/38
    p_win
    p_loose <- 1 - p_win    
    p_loose
    #Expected value of one bet
    one <- (6*p_win) + (-1 * (1- p_win))
    one        
    #Standard error of one bet
    oneError <- abs(-1 - 6) * sqrt(p_win*(1-p_win))
    oneError    
    #What is the expected value of the average payout over 500 bets?
    #Remember there is a difference between expected value of the average and expected value of the sum.
    avgBet500 <- (500 * one)/500 # sem sagt 0
    avgBet500    
    #What is the standard error of the average payout over 500 bets?
    seBet500 <- (sqrt(500) * oneError)/500  
    seBet500    
    #Expected value of the SUM 500 bets
    EsumBet500 <- (500 * one)
    EsumBet500
    #Standard Error of 500 bets
    SEsumBet500 <- (sqrt(500) * oneError)
    SEsumBet500
    #calculate the probability of losing money over 500 bets, Pr(X≤0).
    pnorm(0, EsumBet500, SEsumBet500)
    