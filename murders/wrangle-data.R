#Here we create R data from the raw csv data 
library(tidyverse)
murders <- read_csv("data/murders.csv")
murders <- murders %>% mutate(regions = factor(region), rate = total/population * 10^5)
save(murders, file = "rda/murders.rda")