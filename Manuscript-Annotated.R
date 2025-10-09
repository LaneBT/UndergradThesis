#Investigating slime mold (Physarum polycephalum) memory through maze solving ability
#Undergraduate Honors Thesis, Lane Birusingh Tellegen
#Annotated Statistical Analysis 


#Install all necessary packages
install.packages("nlme")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("RColorBrewer")

#Add packages to library
library(nlme)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)


#Creating working directory
rawdata2 <- read.csv("Data2_v2.csv")

#creating a new column, turning RunNum (maze attempt number) into a factor
rawdata2$RunNumFac<-factor(rawdata2$RunNum)
rawdata2$CONTAMFac<-factor(rawdata2$CONTAM)

#This is what we did together during office hours-----

#linear regression model with randomization (randomizing for group, run number is nested in Group)
#looking at how progression of path length (dependent variable) changes over maze run attempts (independent variable)
#Expected results: as maze run number increases, path length should climb less rapidly 
#(ie the mold is more 'precise', ends with a lower/shorter path legth, which means it is MORE EFFICIENT)

options(contrasts=c("contr.helmert","contr.poly"))
ERmodelfac<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(ERmodelfac)
anova(ERmodelfac,type="marginal")
#Significant

#Graph of how efficiency rating changes over maze attempts 
ERPlot<- ggplot(rawdata2,aes(x=Day, y=ER, color=RunNumFac))+
  geom_jitter()+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  labs(x="Day", y="Path Length", color="Maze Attempt Number", tag= "A")+
  geom_smooth(method = "lm", se = FALSE)+ #se=FALSE removes shaded error bars
  scale_color_brewer(palette = "Greens")

#Repeat with diversions as the dependent variable 
#diversions are growth down dead-end paths
options(contrasts=c("contr.helmert","contr.poly"))
DIVmodelfac<-lme(DIV~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(DIVmodelfac)
anova(DIVmodelfac,type="marginal")
#Significant

DIVPlot<- ggplot(rawdata2,aes(x=Day, y=DIV, color=RunNumFac))+
  geom_jitter()+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  labs(x="Day", y="Dead Ends", color="Maze Attempt Number", tag = "B")+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_brewer(palette = "PuRd")

#repeat with cheating as dependent variable
#cheating is growth over maze walls
options(contrasts=c("contr.helmert","contr.poly"))
CHTmodelfac<-lme(CHT~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(CHTmodelfac)
anova(CHTmodelfac,type="marginal")
#Significant

CHTPlot<- ggplot(rawdata2,aes(x=Day, y=CHT, color=RunNumFac))+
  geom_jitter()+
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )+
  labs(x="Day", y="Cheating", color="Maze Attempt Number", tag ="C")+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_brewer(palette = "YlOrRd")


#creating a combined figure 

install.packages("gridExtra")
library(gridExtra)

grid.arrange(ERPlot, DIVPlot, CHTPlot, nrow=3)


#Remove contaminated plates from the data set and repeat statistics
#This is to identify if the contaminated plates have a significant effect on the statistical results
#see appendix of manuscript for more information


#Linear Regression Models without contaminated plates

puredatall<-rawdata2[!rawdata2$CONTAM == "1", ]
puredatall<-rawdata2[!rawdata2$RunNumFac == "5", ]


#ER

options(contrasts=c("contr.helmert","contr.poly"))
CERmodelfac<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(CERmodelfac)
anova(CERmodelfac,type="marginal")
#Significant

#Graph of how efficiency rating changes over maze attempts 
ggplot(puredatall,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts: Contamination Removed")+
  labs(x="Day", y="Efficiency Rating", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "Greens")


#DIV

options(contrasts=c("contr.helmert","contr.poly"))
CDIVmodelfac<-lme(DIV~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(CDIVmodelfac)
anova(CDIVmodelfac,type="marginal")
#Significant

ggplot(puredatall,aes(x=Day, y=DIV, color=RunNumFac))+
  geom_point()+
  ggtitle("Number of Diversions (deadends) over Maze Attempts: Contamination Removed")+
  labs(x="Day", y="Dead Ends", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "PuRd")


#CHT

options(contrasts=c("contr.helmert","contr.poly"))
CCHTmodelfac<-lme(CHT~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(CCHTmodelfac)
anova(CCHTmodelfac,type="marginal")
#Significant

ggplot(puredatall,aes(x=Day, y=CHT, color=RunNumFac))+
  geom_point()+
  ggtitle("Progression of cheating (wall jumping) over Maze Attempts: Contamination Removed")+
  labs(x="Day", y="Cheating Instances (wall jumping)", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "YlOrRd")




