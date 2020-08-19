library(gtools)
library(dslabs)
library(tidyverse)
ds_theme_set()
take_poll(25)
#Simple poll simulation
#Earn with Red/Blue 25$ prize, each sample element costs 0.10$ and interval right guess gives 50% price
#Blue are p, Red are 1-p and spread of is p-(1-p) or 2p-1
#Statistical inference is to predict p using the sample only
#E[avg(X)] = p - expected value of the sample proportion, X-bar, is p
#SE[avg[X]] = sqrt(p*(1-p)/N) - since N lowers standard error, we can minimize its effect by having N large

#CLT and estimates
x_bar <-0.48
se <- sqrt(x_bar * (1-x_bar)/25)
pnorm(0.01/se) - pnorm(-0.01/se) #probability of sample estimate is same as population p are only 7.9% Pr(|x-bar - p| <= 0.01)

#Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, 
#how large does the sample size have to be to have a standard error of about 1%?
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
#qqnorm(errors)
#qqline(errors)

#=============Confidence intervals and p-values===================
#grey area is confidence interval
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#Solving for z with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

#Monte Carlo simulation to show that 95% confidence interval includes p 95% times
p <- 0.45
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

#Compute p-value for spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

#COOL NOTKUN A GROUP_BY OG FILTER
polls_us_election_2016
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100 )
polls <- polls %>% mutate(error = abs(d_hat-0.021))
polls %>% group_by(pollster) %>% filter(n()>=5)%>% ggplot(aes(pollster, error)) +
  geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#=================Section 4 Poll aggregation===========

#====Example of election forcasting by aggregating polls
d <- 0.039 #MoE 3.9%
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread by monte carlo for each of the 12 polls
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1 #[lower/upper confidence level for Democrats]
})


# generate a data frame storing results of the 12 monte carlos
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
head(polls)
#Each and one of the individual polls include confidence interval of 0 - meaning a tossup
#but   by combining all the twelve as one large poll and can get smaller 95% confidence interval, maybe not includin 0
#Problem is we do not have access to the raw poll data from the 12 polls but math can help to 
#mimic it and create a poll of all sample combined 
#create average estimate of the spread in propotion to each samplesize
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

#Find out estimate for obama votes 
p_hat <- (1+d_hat)/2
#margin of error of the aggregated poll is just 1.8%
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size)) 
#Aggregated spread is 3.1 %
round(d_hat*100,1)
#+- aggregated MoE -+ 1.8 %
round(moe*100, 1)
#That means confidence level of 3,1-1,8 = 1,3 and 3,1+1,8 = 4,9 ==== NOT INCLUDING 0 so no tossup
# and includes the actual resault of 3.9 % margin

######Statistical modeling is needed in real situation - data driven models/Bayesian statistics
# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N) #which is standard error of our estimate or x_bar - mu(mean) or sigma(sd)/sqrt(N)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% summarise( s = sd(spread))

####Help to understand BAYES thinking
##We choose 100000 random persons where the rate of the disease is 1/3900
prevalance <- 1/3900
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prevalance, 1-prevalance))
N_D <- sum(outcome == "Disease")
N_H <- sum(outcome == "Healthy") 
accuracy <- 0.99 #The accuracy of the test
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)
#Real positives are disease/disease+false positives 
0.111^2/(0.111^2+0.027^2)
0.275 + (1-0.9441379)*(0.45-0.275)
sqrt(0.00069)
?between 
?n()
?coord_flip
?sign
#Rotate Axis Labels Perpendicular to the Axis
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  sample(x, N, replace = TRUE)
  interval <- c(mu - qnorm(0.975)*sd(x), mu + qnorm(0.975)*sd(x))
  between <-between(mu,interval[1], interval[2])
})

#========================association and chi-squared tests=============================
#how to create two-by-two table for binary data
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab
fisher.test(tab, alternative = "greater")




#=======================TEST BREXIT==============================
library(tidyverse)  
options(digits = 3)
data(brexit_polls)
p=0.481
d <- 2*p-1 
N <- 1500
remain <-
se <- sqrt(p*(1-p)/N)
d_se <- 2*sqrt(p * (1-p)/ N)
d_se

brexit_polls <- brexit_polls %>%  mutate(x_hat = (spread+1)/2)
avg_d <-mean(brexit_polls$spread)
d_sd <- sd(brexit_polls$spread)
d_sd
avg_x_har <-mean(brexit_polls$x_hat)
avg_x_har
sd_x_har <-sd(brexit_polls$x_hat)
sd_x_har

#Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
brexit_polls[1,]
cis <-  c(brexit_polls$x_hat - qnorm(0.975)* sqrt(brexit_polls$x_hat*(1-brexit_polls$x_hat)/brexit_polls$samplesize),brexit_polls$x_hat + qnorm(0.975)* sqrt(brexit_polls$x_hat*(1-brexit_polls$x_hat)/brexit_polls$samplesize) )
cis

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize))
june_polls <- june_polls %>% mutate(se_d = 2*se_x_hat)
june_polls <- june_polls %>% mutate(lower = spread - qnorm(0.975)*se_d, upper = spread + qnorm(0.975)*se_d)
june_polls <- june_polls %>% mutate(hit = between(spread, lower,upper))
nrow(june_polls)
has_zero <- june_polls %>% filter(sign(lower) != sign(upper))
victory <- june_polls %>% filter(sign(lower) > 0)

right <-june_polls %>% filter(-0.038 >= lower & -0.038 <= upper )

nrow(has_zero)/nrow(june_polls)
nrow(victory)/nrow(june_polls)
nrow(right)/nrow(june_polls) 
cis <- c(june_polls$x_hat - qnorm(0.975)* sqrt(june_polls$x_hat*(1-june_polls$x_hat)/june_polls$samplesize),june_polls$x_hat + qnorm(0.975)* sqrt(june_polls$x_hat*(1-june_polls$x_hat)/june_polls$samplesize) )
cis

#Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
june_polls %>% group_by(pollster) %>% summarise(avgHit = mean(hit)) %>% arrange(avgHit)
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot()

#now work on the bias caused by poll type, where online is more likely to vote out but telephone vote remain
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type <- combined_by_type %>% mutate(se = sqrt(p_hat*(1-p_hat)/N))
combined_by_type
combined_by_type %>% mutate(lower = spread - qnorm(0.975)* (2 * sqrt(p_hat*(1-p_hat)/N)), upper = spread + qnorm(0.975)* (2 * sqrt(p_hat*(1-p_hat)/N)))


brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit
#Use brexit_hit to make a two-by-two table of poll type and hit status. Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.
two_by_two <- table(brexit_hit$hit, brexit_hit$poll_type)
two_by_two
chisq.test(two_by_two)$p.value
#Determine which poll type has a higher probability of producing a confidence interval that covers the correct value of the spread. 
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]
# statistically significant
chisq.test(two_by_two)
chisq.test(two_by_two)$p.value < 0.05

#Odds ratio of online and telephone poll hit rate --- remember odds ratio is the ratio between the false/true odds
oddsRatioOnline <- (two_by_two[[2,1]]/(two_by_two[[1,1]]+two_by_two[[2,1]]))/(two_by_two[[1,1]]/(two_by_two[[1,1]]+two_by_two[[2,1]]))
oddsRatioOnline
oddsRatioPhone <- (two_by_two[[2,2]]/(two_by_two[[1,2]]+two_by_two[[2,2]]))/(two_by_two[[1,2]]/(two_by_two[[1,2]]+two_by_two[[2,2]]))
oddsRatioPhone
ratioBetween <- oddsRatioOnline/oddsRatioPhone
ratioBetween
#Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type). 
# Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. Include the individual data points colored by poll type.
# Add a horizontal line indicating the final value of \(d = -.038\).
brexit_polls %>% ggplot(aes(enddate, spread, col = poll_type)) + geom_smooth(method = "loess", span = 0.4) + geom_point() + geom_hline(yintercept=-0.038)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long                                                                                                                                      
brexit_long %>% ggplot(aes(enddate, proportion, col = vote)) + geom_smooth(method = "loess", span = 0.3) 
