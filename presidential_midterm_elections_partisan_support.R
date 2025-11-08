########################
### 16 October 2023  ###
########################
rm(list=ls())
### Set the working directory
setwd("C:/Users/amuly/OneDrive/Documents/UChicago/coding_projects/problem_set")

### Read in data
library(tidyverse)
mcdon <- read.csv("mcdonald1.Csv")

### Create a midterm variable
mcdon$midterm[mcdon$year %in% seq(1952, 2020, 4)] <- 0
mcdon$midterm[mcdon$year %in% seq(1954, 2020, 4)] <- 1

### Create a variable with turnout as share of voting-age population (VAP)
mcdon$share_VAP <- mcdon$votes_higho / mcdon$vap

### Data Exploration
# mean(mcdon$share_VAP[mcdon$midterm == 0]) #avg turnout rate for prez elections: 0.5603668
# mean(mcdon$share_VAP[mcdon$midterm == 1]) #avg turnout for midterm elections: 0.4024117
# mcdon$year[mcdon$share_VAP == max(mcdon$share_VAP)] #Highest turnout in 1960
# mcdon$year[mcdon$share_VAP == min(mcdon$share_VAP)] #Lowest turnout in 2014
# mean(mcdon$share_VAP[mcdon$year <= 1970]) # 1957-1970 average turnout: 0.5356224
# mean(mcdon$share_VAP[mcdon$year > 1970 & mcdon$year <= 1990]) # 1972–1990 aveerage turnout: 0.4547951
# mean(mcdon$share_VAP[mcdon$year > 1990 & mcdon$year <= 2020]) # 1992-2020 average turnout: 0.4682283

### Turnout rate over time for presidential elections.
plot(subset(mcdon, midterm ==0, select = c(year, share_VAP)),
     type = "b",
     ylab = "Turnout Rate (Votes for highest office as a share of voting age population)",
     xlab = "Year",
     main = "Turnout Rate for Presidential Elections (1952-2020)") 

### Read in and merge second dataset
mcdon2 <- read.csv("mcdonald2.Csv")
mcdon.merged <- merge(mcdon, mcdon2, by = "year")

### Create a variable with turnout as a share of voting-eligible population (VEP)
mcdon.merged$share_VEP <- mcdon.merged$votes_higho / (mcdon.merged$vap - mcdon.merged$felon_inel - mcdon.merged$noncit_pop + mcdon.merged$overseas_el)

### Data Exploration
# mean(mcdon.merged$share_VEP[mcdon.merged$midterm == 0]) # average turnout rate in presidential elections: 0.5898207
# mean(mcdon.merged$share_VEP[mcdon.merged$midterm == 1]) # average turnout rate in midterm elections: 0.4230842
# mean(mcdon.merged$share_VEP[mcdon.merged$year <= 1970]) # 1957-1970 avg VEP turnout: 0.5428786
# mean(mcdon.merged$share_VEP[mcdon.merged$year > 1970 & mcdon.merged$year <= 1990]) # 1972–1990 avg VEP turnout 0.4770995
# mean(mcdon.merged$share_VEP[mcdon.merged$year > 1990 & mcdon.merged$year <= 2020]) # 1992–2020 avg VEP turnout: 0.5072949

# Plot presidential turnout as shares of VAP and VEP.
long_df <-  mcdon.merged %>%
    filter(midterm == 0) %>%
    select(year, share_VAP, share_VEP) %>%
    pivot_longer(cols = c(share_VAP, share_VEP), names_to = "category", values_to = "rates")
   
long_df %>%
  ggplot(aes(x = year, y = rates, color = category)) +
  geom_line() +
  labs(x = "Year", y = "Turnout Rate") +
  ggtitle("Turnout Rate for Presidential Elections 1952-2020")