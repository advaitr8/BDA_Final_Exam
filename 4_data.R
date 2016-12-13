setwd("C:/Users/Julian Bautista/Documents/School Stuff/Semesters/4 Fall 2016/Applied Statistics III/Final Exam/BDA_Final_Exam")
library(readstata13)
library(dplyr)

#pulling the data from web
pew <- read.dta13('http://www.stat.columbia.edu/~gelman/bda.course/pew_research_center_june_elect_wknd_data.dta')
elect <- read.csv("http://www.stat.columbia.edu/~gelman/bda.course/2008ElectionResult.csv")

#combining party affiliation from heat2 and heat 4 with corresponding marital
pop <- data.frame(pew$state, pew$heat2, pew$marital)
pop2 <- data.frame(pew$state,pew$heat4, pew$marital)
pop <- pop[complete.cases(pop$pew.heat2),]
pop2 <- pop2[complete.cases(pop2$pew.heat4),]
names(pop) <- c("state", "party", "marital")
names(pop2) <- c("state", "party", "marital")
marriage <- rbind(pop,pop2)

#drop third parties
marriage <- filter(marriage, party == "rep/lean rep" | party == "dem/lean dem")

#drop hawaii and  alaska
marriage <- filter(marriage, state != "hawaii" & state != "alaska")
elect <- filter(elect, state != "Hawaii" & state != "Alaska")

#0 if republican, 1 if democrat; 0 if not married, 1 if married
marriage$party <- ifelse(marriage$party == "rep/lean rep", 0,1)
marriage$marital <- ifelse(marriage$marital == "married", 1,0)

#remove dropped state factors then generate state id
marriage$state <- factor(marriage$state)
marriage$id <- as.numeric(marriage$state)

#removes index 
#rowname(marriage) <- NULL

#removing excess data.frames
rm("pop","pop2","pew")

#creating variables
state_id <- marriage$id
party <- marriage$party
marital <- marriage$marital
state <- levels(marriage$state)
