
library(gtools)
library(dslabs)
library(dplyr)
library(tidyverse)
data(heights)
head(esoph)
#rep function can create sample of x times y
beads <- rep(c("red","blue"), times = c(2,3))
beads
sample(beads, 2) #gives two random elements from beads dataset
events <- replicate(10000, sample(beads,1))
tab <- table(events) # summarize the events in a table
prop.table(tab) # summarize the events from the table as proportions
?set.seed

#create a deck of cards in R
paste() #Glues variables together
number <- "Three"
suit <- "Hearts"
paste(number, suit)
#and for vectors like
paste(letters[1:5], as.character(1:5))
#all combinations of a list use expand.grind()
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))




#Our deck of cards
suits <- c("Hearts", "Clubs", "Spades", "Diamonds")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(suit = suits, number = numbers)
deck <- paste(deck$suit, deck$number)
deck
kings <- paste(suits, "King")
mean(deck %in% kings)
hands<- permutations(52, 2, v = deck)
nrow(hands) #possible ways to pick 2 cards from the deck
first_cardPicked <- hands[,1] #all rows column 1
second_cardPicked <- hands[,2] #all rows column2 
#Number of kings in first column
sum(first_cardPicked %in% kings)
sum(first_cardPicked %in% kings & second_cardPicked %in% kings) / sum(first_cardPicked %in% kings) #both kings

#the permutation() method, no repetions - order matters 1,3 != 3,1
#combinations() method = reps allowed, order matters 1,3 != 3,1  much more results since we can reuse elements
#all 7 digit phone numbers
all_numbers <- permutations(10, 7, v = 0:9) #First size of the source vector (10 numbers), then size of target(squence), what numbers are in the source vector
n <- nrow(all_numbers) #returns how many rows(phone numbers there are)
index <- sample(n, 5) #choose 5 random indexes from the total indexes
all_numbers[index,]

#Two ways to solve birtday paradox, first monte carlo then math

#Monte Carlo
compute_prob <- function(n, B = 10000){ #takes variable n and tries it 10 thousend times
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace =TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
#now we want to see the liklyness for a range 1-60 persons
n <- seq(1,60)
prob <- sapply(n, compute_prob) #have to use sapply to apply the vector to the function
plot(prob) #Takes a little time, cause we need to generate 60*10000 examples
?prod
#We can use math and get this much faste, no need for generating all those examples
exact_prob <- function(n){
  #The multiplication rule to calculate Pr(noSharedBirthday) = (1*364/365*363/365*365-n-1/365)
  prob_unique <- seq(365, 365-n+1)/365
  1 - prod(prob_unique) #prod(vectors) returns product of the whole vector - margföldun - that is 1-Pr(NoSameBirthd)
}

eprod <- sapply(n, exact_prob) #same as in Monte Carlo but now to the math function
plot(n,prob)
lines(n,eprod, col = "red")

#HOW MANY MONTE CARLOS ARE ENOUGH?
B <-10^seq(1, 5, len = 100) #bilið er frá 1-5 splittað upp í 100 og sem veldið
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 
#log10(B) kringum 3 = 10^3 eða 1000 næst jafnvægi þannig að í raun gæfi 1000 iteration nógu góða niðurstöðu í stað 10000 og krefst minni computer power

simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

#MONTE HALL CODE
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3) #Three doors created
  prize <- sample(c("car","goat","goat"))# puts prizes in random order
  prize_door <- doors[prize == "car"] # note which door has prize by assigning the index of car in prize to index of doors
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)   # open door with no prize and that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize TRUE or FALSE
})
mean(stick) # probability of choosing prize door when sticking == TRUE_TOTAL/TOTAL ITERATIONS

?combinations()

r <-permutations(3,3)
nrow(r)
3/8*2/7*1/6
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
fromJamaica <- replicate(B, {
   winners <- sample(runners, 3)
   all(winners == "Jamaica")
})
mean(fromJamaica)
entree <- c(as.character(1:6))
side <- c(as.character(7:12))
drink <- c(as.character(13:14))
?combinations
oneMeal <- expand_grid(1:6,7:12,13:17,18:19)
nrow(oneMeal)
rows <- combinations(6,2)
nrow(rows)*6*3
options <- function(n){
  entreeOptions <- 6
  drinks <- 3
  sides <- nrow(combinations(n,2))
  combinations <- entreeOptions*drinks*sides
}
sapply(2:12, options)

#Cancer assignment
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_cases
all_controls <- sum(esoph$ncontrols)
all_controls
typeof(esoph$alcgp)
levels(esoph$alcgp)
highest <- esoph %>% filter(alcgp == "120+")
all_cases_highest <- sum(highest$ncases)
all_cases_highest/all_cases
all_cases_highest/all_cases
all_controls_higest <- sum(highest$ncontrols)
all_controls_higest/all_controls
(all_cases_highest/all_cases)/(all_controls_higest/all_controls)
all_cases_highest/(all_cases_highest+all_controls_higest)
lowest <- esoph %>% filter(alcgp == "0-39g/day")
all_cases_lowest <- sum(lowest$ncases)
all_controls_lowest <- sum(lowest$ncontrols)
all_cases_lowest/(all_cases_lowest+all_controls_lowest)
levels(esoph$tobgp)
smokeless10g <- esoph %>% filter(tobgp == "0-9g/day")
caseless10g <- sum(smokeless10g$ncontrols)
1-caseless10g/all_controls
smokeHighest <- esoph %>% filter(tobgp == "30+")  
sum(smokeHighest$ncases)/all_cases
sum(smokeHighest$ncontrols)/all_controls
highBoth  <- esoph %>% filter(tobgp == "30+" & alcgp == "120+")  
sum(highBoth$ncases)/all_cases
sum(highBoth$ncontrols)/all_controls
highOr  <- esoph %>% filter(tobgp == "30+" | alcgp == "120+")  
sum(highOr$ncases)/all_cases
sum(highOr$ncontrols)/all_controls
(sum(highOr$ncases)/all_cases)/(sum(highOr$ncontrols)/all_controls)
#Cool to use summarize to create results we want in table!!!!!!!!!!!!!!!
esoph %>% filter(alcgp == "120+") %>% summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)
# OR maybe pull() to get one column straight within a pipe :  esoph %>% filter(alcgp == "120+" %>% pull(ncontrols))
?between

