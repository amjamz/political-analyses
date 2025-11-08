#######################
### 20 October 2023 ###
#######################

### Set the working directory
setwd("C:/Users/amuly/OneDrive/Documents/UChicago/coding_projects/problem_set")
library(tidyverse)

### Read in the data set
wash <- read.csv(file = "washington.csv")

### Plot a histogram of the AAUW legislative score
hist(wash$aauw, freq = FALSE, xlab = "Score", ylab = "Frequency", main = "AAUW's legislative scores", na.rm = TRUE)

### Data Exploration
# mean(wash$aauw) # 47.30805
# median(wash$aauw) # 38


### Regression of legislative support for feminist issues on partisan composition of the district.
linreg_wash <- lm(wash$aauw ~ wash$demvote) 
linreg_wash # Intercept: -65.33 coeff: 223.83
plot(wash$aauw ~ wash$demvote, type = "p", ylab = "AAUW Legislative Score", xlab = "Democratic vote share in prior presidential election", main = "Representative legislative support for feminist issues by district partisan composition")
abline(lm(wash$aauw ~ wash$demvote), col = "blue", lwd = 4)

### Create an indicator variable for republican legislators.
### coded =1 if the legislator is a Republican 
### coded = 0 if the legislator is a Democrat or Independent
wash$republican <- ifelse(wash$party == 2, 1, 0)

### Data Exploration
# mean(wash$aauw[wash$republican == 1]) # Mean AAUW score for Republican legislators: 11.98246
# mean(wash$aauw[wash$republican == 0]) # Mean AAUW score for Democratic/Independent legislators: 86.21739

### Regression of legislative support for feminist issues on the indicator for Republican
lm(wash$aauw ~ wash$republican) # Intercept: 86.22 coeff: -74.23