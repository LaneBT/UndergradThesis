#Investigating Slime Mold (Physarum polycephalum) Intelligence Through Maze Solving Ability
#Undergraduate Honors Thesis - Data Analysis
#annotated code utilizing data from the both rounds of data collection
#just for visualization, both revised and unrevised methods

getwd()

dueldata <- read.csv("DuelMethod_endroute.csv")


#Data Visualization----

#Run Number by DAY, DR, DIV

library(tidyr)
library(ggplot2)

figurestacked_duel <- pivot_longer(dueldata,c(4,5,6),names_to="trait",values_to="value",values_drop_na=TRUE)[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

ggplot(figurestacked_duel, aes(x=RunNum, y=value, color=trait))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~trait,nrow=3,scales = "free")+
  ggtitle("Effect of maze attempt on Different Variables")+
  labs(x="Maze Attempt Number",y="Value")+
  scale_color_manual(name = "Variables", labels = c("Day to Completion","Efficiency Rating","Number of Diversions"), values = c("red", "green", "blue"))

#Just ER

ggplot(dueldata,aes(x=RunNum, y=ER))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Maze Run Number", y="Efficiency Rating")+
  geom_smooth(method = "lm")

#Just Day

ggplot(dueldata,aes(x=RunNum, y=Day))+
  geom_point()+
  ggtitle("Days for P. polycephalum to Complete the Maze")+
  labs(x="Maze Run Number", y="Day")+
  geom_smooth(method = "lm")

#Bubble Chart about contamination percent cover?

#creating percent contamination cover, new column
library(tidyr)

dueldatacont<-dueldata
dueldatacont<-dueldata[c(1,2,3,4,5,6,7,8,9,10,15)]

dueldatacont<-na.omit(dueldatacont)
dueldatacont$cPER <- dueldatacont$cCOVER/datacont$totalsq

#start with basic geom_point
ggplot(dueldatacont, aes(x=RunNum, y=ER, size = cPER)) +
  geom_point(alpha=0.7)


#NOTE: add in color variable as where contamination started? I am not sure this is displaying the information I need displayed.

#OTHER FIGURE IDEAS----
#Chord Diagram... with information separated by family by maze run? or by day? OR by maze run and then by family


