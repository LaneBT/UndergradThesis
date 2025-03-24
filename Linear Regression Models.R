#Linear Regression Models and Data Visualization


library(nlme)

#looking at how progression of ER changes over DAY, separated by run number
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








