#Investigating Slime Mold (Physarum polycephalum) Intelligence Through Maze Solving Ability
#Undergraduate Honors Thesis - Data Analysis
#annotated code utilizing data from the second round of data collection (revised methods)

#Utilizing data from the second data collection trial

#create wd

getwd()

#This is only the data from completed maze attempts
#ie the final day when the mold has reached the center of the maze.

rawdata2 <- read.csv("DataTrial2_endroute.csv")

#Manipulate Data----

#creating percent contamination cover, new column
datacont<-rawdata2
datacont<-na.omit(datacont)
datacont$cPER <- datacont$cCOVER/datacont$totalsq


#creating figures----

# run number by efficiency rating

library(ggplot2)

ggplot(rawdata2,aes(x=RunNum, y=ER, color=Group))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Maze Run Number", y="Efficiency Rating")

ggplot(rawdata2,aes(x=RunNum, y=ER))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Maze Run Number", y="Efficiency Rating")+
  geom_smooth(method = "lm")

ggplot(rawdata2,aes(x=RunNum,y=ER, group=RunNum))+
  geom_boxplot()+
  ggtitle("Efficiency Rating over Number of Maze Attempts")+
  labs(x="RunNum",y="ER")+
  geom_jitter(color="red", size=0.8, alpha=0.9)


#run number by number of diversions

ggplot(rawdata2,aes(x=RunNum,y=DIV, fill=DIV))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Number of Diversions over Maze Run Number")+
  labs(x="RunNum",y="DIV")
#ewwww so gross I dont like. Not clear

ggplot(rawdata2,aes(x=RunNum,y=DIV, group=RunNum))+
  geom_boxplot()+
  ggtitle("Number of Diversions")+
  labs(x="RunNum",y="DIV")+
  geom_jitter(color="red", size=0.8, alpha=0.9)


#run number by days to completion

ggplot(rawdata2,aes(x=RunNum, y=Day, color=Group))+
  geom_point()+
  ggtitle("Days for P. polycephalum to Complete the Maze")+
  labs(x="Maze Run Number", y="Day")

ggplot(rawdata2,aes(x=RunNum, y=Day))+
  geom_point()+
  ggtitle("Days for P. polycephalum to Complete the Maze")+
  labs(x="Maze Run Number", y="Day")+
  geom_smooth(method = "lm")

ggplot(rawdata2,aes(x=RunNum,y=ER, group=RunNum))+
  geom_boxplot()+
  ggtitle("Days for P. polycephalum to Complete the Maze")+
  labs(x="RunNum",y="Day")+
  geom_jitter(color="red", size=0.8, alpha=0.9)

#run numver by all?

library(tidyr)
figurestacked <- pivot_longer(rawdata2,c(4,5,6),names_to="trait",values_to="value",values_drop_na=TRUE)[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

ggplot(figurestacked, aes(x=RunNum, y=value, color=trait))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~trait,nrow=3,scales = "free")+
  ggtitle("Effect of maze attempt on Different Variables")+
  labs(x="Maze Attempt Number",y="Value")+
  scale_color_manual(name = "Variables", labels = c("Day to Completion","Efficiency Rating","Number of Diversions"), values = c("red", "green", "blue"))

#Visualizing ER changes by family




#statistical analysis----

#I am concerned I don't have enough data for an anova

#anova - run num and ER
a_RunNumER <- aov(ER ~ RunNum, data = rawdata2)
summary(a_RunNumER)

#To my understanding, Tukey HSD requires catagories as factors
#this should allow Tukey test to work
data.aov.factor = aov(ER ~ factor(RunNum), data = rawdata2)
TukeyHSD(data.aov.factor)

#anova - run num and DIV
a_RunNumDIV <- aov(DIV ~ RunNum, data = rawdata2)
summary(a_RunNumDIV)

data.DIV.factor = aov(DIV ~ factor(RunNum), data = rawdata2)
TukeyHSD(data.DIV.factor)

#anova - run num and Day
a_RunNumDay <- aov(Day ~ RunNum, data = rawdata2)
summary(a_RunNumDay)

data.Day.factor = aov(Day ~ factor(RunNum), data = rawdata2)
TukeyHSD(data.Day.factor)

#to summarize, nothing statistically significant here.

#Should I compare the first run within a group to the last run?
#ie J1 ER versus J5 ER? 



#Completing data analysis using Multicomp?? ----
#Look into how to code this


#Understanding Contamination----







