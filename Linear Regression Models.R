#Linear Regression Models and Data Visualization


rawdata2 <- read.csv("Data2_v2.csv")
rawdata2$RunNumFac<-factor(rawdata2$RunNum)

enddata2<- read.csv("endrouteData2_v2.csv")



library(nlme)

#looking at how progression of ER changes over DAY, separated by run number-----
options(contrasts=c("contr.helmert","contr.poly"))
ERmodelfac<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(ERmodelfac)
anova(ERmodelfac,type="marginal")
#significant relationship!!


library(ggplot2)
ggplot(rawdata2,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  geom_smooth(method = "lm")






#SAME THING BUT WITH DIV ---> I DONT THINK THIS IS A APPROPRIATE MODEL FOR THIS-----

#looking at how progression of number of Diversions changes over DAY, separated by run number
options(contrasts=c("contr.helmert","contr.poly"))
DIVmodelfac<-lme(DIV~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(DIVmodelfac)
anova(DIVmodelfac,type="marginal")
#LOOK BACK AT THIS!!! Becuase DIV is factorial (1-4)

ggplot(rawdata2,aes(x=Day, y=DIV, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Dead Ends")+
  geom_smooth(method = "lm")



#Same thing but with Cht-----

#looking at how progression of cheating instances changes over DAY, separated by run number
options(contrasts=c("contr.helmert","contr.poly"))
CHTmodelfac<-lme(CHT~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(CHTmodelfac)
anova(CHTmodelfac,type="marginal")
#Results


ggplot(rawdata2,aes(x=Day, y=CHT, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Cheating Instances (wall jumping)")+
  geom_smooth(method = "lm")



#General linear effects with randomization for group-----
#fitting linear regression model with random effects

ERmodel<-lme(ER~RunNum, random=~1|Group,data=enddata2)
summary(ERmodel)
anova(ERmodel)

Daymodel<-lme(Day~RunNum, random=~1|Group,data=enddata2)
summary(Daymodel)
anova(Daymodel)

Divmodel<-lme(DIV~RunNum, random=~1|Group,data=enddata2)
summary(Divmodel)
anova(Divmodel)






#Without Contamination-----
#getting error "Singularity in backsolve at level 0, block 1'

puredatall<-rawdata2[!rawdata2$CONTAM == "1", ]

options(contrasts=c("contr.helmert","contr.poly"))
ERmodelfac_p<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(ERmodelfac_p)
anova(ERmodelfac_p,type="marginal")
#significant relationship!!


library(ggplot2)
ggplot(puredatall,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  geom_smooth(method = "lm")





#I am not sure these work.

puredatend<-enddata2[!enddata2$CONTAM == "1", ]

ERmodel_p<-lme(ER~RunNum, random=~1|Group,data=puredatend)
summary(ERmodel_p)
anova(ERmodel_p)

Daymodel_p<-lme(Day~RunNum, random=~1|Group,data=puredatend)
summary(Daymodel_p)
anova(Daymodel_p)

Divmodel_p<-lme(DIV~RunNum, random=~1|Group,data=puredatend)
summary(Divmodel_p)
anova(Divmodel_p)









