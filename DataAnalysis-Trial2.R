# Investigating Slime Mold (Physarum polycephalum) Intelligence Through Maze Solving Ability
#Undergraduate Honors Thesis - Data Analysis
#annotated code utilizing data from the second round of data collection (revised methods)

#Utilizing data from the second data collection trial

#create wd

getwd()

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


#statistical analysis----


