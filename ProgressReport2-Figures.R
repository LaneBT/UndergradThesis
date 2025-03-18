# Investigating Slime Mold (Physarum polycephalum) Intelligence Through Maze Solving Ability
#Undergraduate Honors Thesis - Preliminary Data Analysis
#completed for Second Progress Report
#annotated code utilizing data from the first of data collection (un-revised methods)


# Thesis R Code: Second Progress Report

#Utilizing data from first data collection trial -----

getwd()

rawdata <- read.csv("ProgReport2.csv")

library(ggplot2)

#to make contamination cover percent
rawdata1<-rawdata

rawdata1 <- rawdata1[-1,]
rawdata1$ContPer <- rawdata1$ContCover/rawdata1$totalsq

#Data Collection 1, FIGURES ONLY----

# run number by efficiency rating

ggplot(rawdata,aes(x=RunNum, y=ER, color=Group))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Maze Run Number", y="Efficiency Rating")

ggplot(rawdata,aes(x=RunNum, y=ER, color=Cont))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Maze Run Number", y="Efficiency Rating")

ggplot(rawdata,aes(x=Cht, y=ER, color))+
  geom_point()+
  ggtitle("How Instances of Cheating Effected Efficiency in Maze Attempts")+
  labs(x="Instances of Cheating", y="Efficiency Rating")


ggplot(rawdata1,aes(x=ContPer, y=ER))+
  geom_point()+
  ggtitle("Efficiency Rating by Percent Contamination Cover")+
  labs(x="Contamination Cover (percent)",y="Efficiency Rating")


ggplot(rawdata,aes(x=RunNum,y=Div, fill=Div))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Number of Diversions over Maze Run Number")+
  labs(x="RunNum",y="Div")


ggplot(rawdata,aes(x=RunNum,y=Div, group=RunNum))+
  geom_boxplot()+
  ggtitle("Number of Diversions")+
  labs(x="RunNum",y="Div")+
  geom_jitter(color="red", size=0.8, alpha=0.9)

#efficiency rating and contamination?

library(tidyr)

data3 <- pivot_longer(rawdata1,c(6,7,12),names_to="trait",values_to="value",values_drop_na=TRUE)[c(1,2,3,4,5,6,7,8,9,10,11)]

ggplot(data3, aes(x=ER, y=value, color=trait))+
  geom_line()+
  facet_wrap(~trait,nrow=3,scales = "free")+
  ggtitle("Efficiency Rating by Different Recorded Variables")+
  labs(x="Efficiency Rating",y="Value")+
  scale_color_manual(name = "Variables", labels = c("Cheating Instances","Percent Contamination Cover","Number of Diversions"), values = c("red", "green", "blue"))


ggplot(rawdata, aes(x=Day))+
  geom_bar()

ggplot(data3,aes(x=RunNum, y=Day, color=value,))+
  geom_point()+
  scale_fill_distiller('pr',palette='Spectral', 
                       breaks = c(0.2, 0.5, 0.8), limits = c(0,1))


#Trying out basic stats--- First Data Collection ----

#Anova Tests

# run and er # (????)
ERanova <- aov(ER~RunNum,data=rawdata1)
summary(ERanova)


#To my understanding, Tukey HSD requires catagories as factors
#this should allow Tukey test to work
ERanova.factor = aov(ER ~ factor(RunNum), data = rawdata1)
TukeyHSD(ERanova.factor)


#Run and Day

dayanova <- aov(Day~RunNum,data=rawdata1)
summary(dayanova)

dayanova.factor = aov(Day ~ factor(RunNum), data = rawdata1)
TukeyHSD(dayanova.factor)

#Run and div

divanova <- aov(Div~RunNum,data=rawdata1)
summary(divanova)
TukeyHSD(divanova)
#did not work properly....revise

divanova.factor = aov(Div ~ factor(RunNum), data = rawdata1)
TukeyHSD(divanova.factor)

#nothing significant

