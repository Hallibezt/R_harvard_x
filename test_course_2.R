options(digits = 3)    # report 3 significant digits
library(tidyverse)
install.packages("titanic")
install.packages("gridExtra")
library(gridExtra)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
?titanic_train

titanic %>% ggplot(aes(Age, ..count..,  fill = Sex)) + geom_density(alpha = 0.2) + facet_grid(.~Sex)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age)) 

ggplot(titanic, aes(sample=Age))+geom_qq(dparams = params) + geom_abline()

ggplot(titanic, aes(Survived, fill = Sex)) + geom_bar()

titanic %>% ggplot(aes(Age, ..count..,  fill = Survived)) + geom_density(alpha = 0.2) + facet_grid(. ~ Survived)

titanic %>% filter(Fare != 0) %>% ggplot(aes(Survived,Fare)) + geom_boxplot() + geom_jitter(alpha=0.2)+ scale_y_continuous(trans = "log2")

a <- titanic %>% ggplot(aes(Pclass, fill =Survived)) + geom_bar()
b <-titanic %>% ggplot(aes(Pclass, fill =Survived)) + geom_bar(position = position_fill())
c <-titanic %>% ggplot(aes(Survived, fill =Pclass)) + geom_bar(position = position_fill())
grid.arrange(a,b,c, ncol = 3)
#Create a grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class.

titanic %>% ggplot(aes(Age, ..count..,  fill = Survived)) + geom_density(alpha = 0.2) + facet_grid(Sex ~ Pclass)

#STARS PART
data(stars)
head(stars)
sd(stars$magnitude)
mean_magnitude <- mean(stars$magnitude)
stars %>% ggplot(aes(temp)) + geom_density()

stars %>%  ggplot(aes(temp,magnitude, color = type)) + geom_point(size=10)+ scale_y_reverse() +scale_x_reverse()
  
#Climate change PART
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
head(temp_carbon)
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_line()  +  +geom_hline(aes(yintercept = 0, col = "blue"))
p 
x <- temp_carbon %>% filter(year >= 1900 &  year <2000) 
mean(x$temp_anomaly)
temp_carbon %>% filter(temp_anomaly< -0.0045, !is.na(temp_anomaly))%>% pull(year)%>% max()
#Use multiple geom_lines in one timeplot
temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept=1850)
     ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
     
 temp_carbon %>% ggplot(aes(year, carbon_emissions)) + geom_line()
 
 co2_time <- historic_co2 %>% ggplot(aes(year, co2, color = source) ) + geom_line()
co2_time + scale_x_continuous(limits = c(-3000,2018))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

#
#Rotate Axis Labels Perpendicular to the Axis
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))