library(dslabs)
library(tidyverse)
install.packages("ggthemes")
install.packages("ggrepel")
library(ggrepel)
library(ggthemes)
  data(murders)
data(heights)
options(digits = 3)
data("olive")
head(olive)

murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

ls()
ls
log
data()
log(exp(1))
a <- 2
b <- (-1)
c <- (-4)

((-b)-sqrt((b^2)-(4*a*c)))/(2*a)
(-b-sqrt(b^2-4*a*c))/2*a   
log(1024,4)
data("movielens")
str(movielens)
nrow(movielens)
nlevels(movielens$genres)
murder_rate <- murders$total/murders$population*100000 
x = murders$total[51]/murders$population[51]*100000
x
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
time
speed <- distance/time
hours <- distance/speed
hours

index <- (murders$total/murders$population*100000) <= 0.2
index 
murders$state[index]
str(murders)

str(heights)
average <- mean(heights$height)
average
 
ind <- filter(heights, height > average & sex == "Female")
nrow(ind)
nrow(heights)
mean(heights$sex == "Female")
mean(heights$sex == "Male")
 min(heights$height)
max(heights$height)
match(minHeight,heights$height)
heights$sex[1032]
x <- 50:82
#How many of the integers in x are NOT heights in the dataset?
#Use the sum() and %in% functions in addition to the ! operator.
sum(!(x %in% heights$height))
heights2 <- mutate(heights, ht_cm = height*2.54)
heights2$ht_cm[18]
mean(heights2$ht_cm)
females <- filter(heights2, sex == "Female")
nrow(females)
mean(females$ht_cm)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(olive$palmitic~olive$region)
#Write an ifelse() statement that returns 1 if the sex is Female and 2 if the sex is Male.
#What is the sum of the resulting vector?
sum(ifelse(heights$sex == "Female", 1, 2))

inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

heights2 <- mutate(heights, toFeet = inches_to_ft(height))
x <- filter(heights2, toFeet >= 5)
x
nrow(x)
prop.table(table(heights$sex))


x <- heights %>% filter(sex=="Male") %>% pull(height)
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
#range of 1 pnorm works well
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
#range over 1 not working well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
#porportion in a range
mean(x>69 & x>=72)
summary(heights$height)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
head(percentiles)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
qnorm(p)

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles

r <- murders %>% #get the average murder rate to see if states are above/under
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb))+ 
  geom_point( aes(col = region), size = 3) + #ALWAYS AES for mapping
geom_text_repel() + scale_x_continuous(trans = "log10") +
 scale_y_continuous(trans = "log10")+ #can use scale_x_log10()
  xlab("something")+ ylab("something")+ geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +scale_color_discrete(name = "Region") +theme_economist()  
heights %>% ggplot(aes(height, fill = sex, )) + geom_density(alpha = 0.2)

#Rotate Axis Labels Perpendicular to the Axis
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  