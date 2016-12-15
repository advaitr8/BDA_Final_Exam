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
marriage <- filter(marriage, state != "hawaii" & state != "alaska" & state != "washington dc")
elect <- filter(elect, state != "Hawaii" & state != "Alaska" & state != "District of Columbia")

#0 if republican, 1 if democrat; 0 if not married, 1 if married
marriage$party <- ifelse(marriage$party == "rep/lean rep", 0,1)
marriage$marital <- ifelse(marriage$marital == "married", 1,0)

#remove dropped state factors then generate state id
marriage$state <- factor(marriage$state)
marriage$id <- as.numeric(marriage$state)

#drop NAs
marriage <- marriage[complete.cases(marriage$marital),]

#removes index 
#rowname(marriage) <- NULL

#removing excess data.frames
rm("pop","pop2","pew")


#create pred variables for generated quantities
preds <- marriage %>% group_by(state) %>% summarise(party_pred = mean(party), mar_pred = mean(marital))

#creating variables
state_id <- marriage$id
party <- marriage$party
marital <- marriage$marital
state <- levels(marriage$state)
N <- length(party)
party_pred <- preds$party_pred
mar_pred <- preds$mar_pred
state_id_pred <- c(1:48)

#create a function to capitalize letters of first word of state
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
state <- sapply(state, simpleCap)
#state[8] <- "Washington DC"

#remove attributes of state
attributes(state) <- NULL
