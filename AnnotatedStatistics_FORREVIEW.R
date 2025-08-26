# Statistical Analysis, Undergraduate Thesis
#FOR REVIEW AND REVISION BY PI (Thank you Dr. Farrer!!)


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
#lloking at how progression of Efficiency Rating changes over maze run attempts
#Expected results: as maze run number increases, ER should climb less rapidly 
#(ie the mold is more 'precise', ends with a lower efficiency rating, which means it is MORE EFFICIENT)

options(contrasts=c("contr.helmert","contr.poly"))
ERmodelfac<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(ERmodelfac)
anova(ERmodelfac,type="marginal")
#Significant

#Graph of how efficiency rating changes over maze attempts 
ggplot(rawdata2,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "Greens")


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




















#for contamination location


ggplot(crazy2,aes(x=Day, y=ER, color=ContaminationLocation))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  geom_smooth(method = "lm")


#lin reg contamination presence
options(contrasts=c("contr.helmert","contr.poly"))
Cmodelfac<-lme(ER~RunNumFac*Day*CONTAMFac, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(Cmodelfac)
anova(Cmodelfac,type="marginal")
#Significant

#Graph of this relationship
ggplot(rawdata2,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  geom_smooth(method = "lm")



#This is stuff I did by myself-----

#First I created the same type of model as the one above, but for both Diversions (DIV) and instances of cheating (CHT)

#The diversion variable (DIV) is talking about number of dead ends... it is only 0-4
#QUESTION: Should this also be factorial?

options(contrasts=c("contr.helmert","contr.poly"))
DIVmodelfac<-lme(DIV~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(DIVmodelfac)
anova(DIVmodelfac,type="marginal")
#Significant

ggplot(rawdata2,aes(x=Day, y=DIV, color=RunNumFac))+
  geom_point()+
  ggtitle("Number of Diversions (deadends) over Maze Attempts")+
  labs(x="Day", y="Dead Ends", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "PuRd")
#This graph confuses me though bc it says the p-value is significant but the graph doesnt really show that?
#Did I do this incorrectly?

#The cheating variable (CHT) is referring to growth over the maze walls, counting a direct line from agar to agar as one.

options(contrasts=c("contr.helmert","contr.poly"))
CHTmodelfac<-lme(CHT~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=rawdata2)
summary(CHTmodelfac)
anova(CHTmodelfac,type="marginal")
#Significant

ggplot(rawdata2,aes(x=Day, y=CHT, color=RunNumFac))+
  geom_point()+
  ggtitle("Progression of cheating (wall jumping) over Maze Attempts")+
  labs(x="Day", y="Cheating Instances (wall jumping)", color="Maze Attempt Number")+
  geom_smooth(method = "lm")+
  scale_color_brewer(palette = "YlOrRd")

#QUESTION: Are these the best types of models for these variables??

#I also created just similar linear regression models with randomization
#Randomized for Group (ie the conserved lineages that I recorded maze run number within)
#Completed with only data from the final plate of each run (end route data)


enddata2<- read.csv("endrouteData2_v2.csv")


ERmodel<-lme(ER~RunNum, random=~1|Group,data=enddata2)
summary(ERmodel)
anova(ERmodel)
#insignificant

Divmodel<-lme(DIV~RunNum, random=~1|Group,data=enddata2)
summary(Divmodel)
anova(Divmodel)
#insignificant

CHTmodel<-lme(CHT~RunNum, random=~1|Group,data=enddata2)
summary(CHTmodel)
anova(CHTmodel)
#insignificant

#this model does not include the temporal 'day' variable
#This model: Comparing how many days it took to complete the maze ('Day') by maze attempt number
Daymodel<-lme(Day~RunNum, random=~1|Group,data=enddata2)
summary(Daymodel)
anova(Daymodel)
#insignificant

#QUESTION: I am unclear on if any of these models are relevant, or just the top ones.
#I kind of think they are not...



#Statistics from a data set where all contaminated plates are removed-----

#Trying to remove all data points where contamination was marked



#Repeating models above with new dataframe
#QUESTION: I actually can't run any of these...
#This is giving me an error 'Error in MEEM(object, conLin, control$niterEM) : Singularity in backsolve at level 0, block 1'
#I've been troubleshooting it but haven't been able to fix it. I think its an issue with the df

#ER over days by RunNum
options(contrasts=c("contr.helmert","contr.poly"))
ERmodelfac<-lme(ER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(ERmodelfac)
anova(ERmodelfac,type="marginal")

ggplot(puredatall,aes(x=Day, y=ER, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  geom_smooth(method = "lm")

#DIV over days by RunNum
options(contrasts=c("contr.helmert","contr.poly"))
DIVmodelfac<-lme(DIV~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(DIVmodelfac)
anova(DIVmodelfac,type="marginal")

ggplot(puredatall,aes(x=Day, y=DIV, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Dead Ends")+
  geom_smooth(method = "lm")

#CHT over days by RunNum
options(contrasts=c("contr.helmert","contr.poly"))
CHTmodelfac<-lme(CHT~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=puredatall)
summary(CHTmodelfac)
anova(CHTmodelfac,type="marginal")

ggplot(puredatall, aes(x=Day, y=CHT, color=RunNumFac))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Cheating Instances (wall jumping)")+
  geom_smooth(method = "lm")



#Dealing with visualizing the contamination-----

#Creating a new column that is percent contamination cover
datacont<-rawdata2

#First I need to change the value in the total column (its the wrong one...oopsie!)
datacont$totalsq[datacont$totalsq == 201] <- 324
datacont$totalsq <- as.numeric(datacont$totalsq)

#create a new column with the percent plate cover
datacont$cPER <- datacont$cCOVER/datacont$totalsq

#What if...I try to do the same linear regression model but with contamination cover?

options(contrasts=c("contr.helmert","contr.poly"))
CONTmodelfac<-lme(cPER~RunNumFac*Day, correlation=corAR1(form=~Day|Group/RunNumFac), random=~1|Group,data=datacont)
summary(CONTmodelfac)
anova(CONTmodelfac,type="marginal")
#verrrrry Significant

ggplot(datacont,aes(x=Day, y=cPER, color=RunNumFac))+
  geom_point()+
  ggtitle("Contamination cover by day over Maze Attempts")+
  labs(x="Day", y="Contamination (percent cover)")+
  geom_smooth(method = "lm")
#this actually shows how later runs have higher contamination

#QUESTION: is there other statistical models I should be thinking about?

#MY PLAYGROUND: some data visualization... other interesting models? (THIS NEEDS LESS REVIEW THAN STATS/NO REVIEW)-----
#I am more confident in my ability to create visualizations

#Relationships I want to investigate:
#RELATIONSHIP: between where the contamination is and efficiency rating

crazy2<- pivot_longer(datacont,c(11,12,13,14),names_to="ContaminationLocation",values_to="Present",values_drop_na=TRUE)
crazy2<-crazy2[!crazy2$Present == "0", ]
ggplot(crazy2,aes(x=Day, y=ER, color=ContaminationLocation))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")+
  facet_wrap(~Group, scale="free")
#I think this shows that at certain days, if the contamination is only at the start, the efficiency rating is lower
#I think this might be confounded by me transferring the contamination to the start...also doesnt account for run number. Really just not a great visualization


#what about back to back barplots with ER and Cont Cover

Databar<-pivot_longer(datacont,c(5,18),names_to="Name",values_to="value",values_drop_na=TRUE)

  
ggplot(Databar, aes(fill=Group, y=value, x=RunNum)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Name, scale="free")
#This is getting closer, but really I need two different y-axes

#or I could literally just make two bar charts

ggplot(datacont, aes(y=ER, x=RunNum,fill=Group)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Group, scale="free")
ggplot(datacont, aes(y=cPER, x=RunNum,fill=Group)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Group, scale="free")
#they really should be on the same chart.



#Other stuff, just for fun:

#Bubble chart, run number by ER, colored by group and size by contamination percent

ggplot(datacont, aes(x=RunNum, y=ER, size = cPER, col=Group)) +
  geom_point(alpha=0.7)
#eh not that informative

# what if I create a bubble chart for one of the linear regression models
#this is the graph from the model we did in Office Hours
ggplot(datacont,aes(x=Day, y=ER, color=RunNumFac, size=cPER))+
  geom_point()+
  ggtitle("Efficiency of P. polycephalum over Maze Attempts")+
  labs(x="Day", y="Efficiency Rating")
#I don't know that this is really informative either, but looks cool

#trying to visualize the linear regression data in another way
#I am going to try something totally crazy for fun
crazy<- pivot_longer(datacont,c(5,6,8,11,12,13,14,18),names_to="Variable",values_to="Value",values_drop_na=TRUE)

ggplot(crazy, aes(x=RunNum, y=Value, fill=Group)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~Variable, scale="free")
#not informative


#This is me trying to map all the 


install.packages("remotes")
remotes::install_github("wilkelab/ggridges")
#I don't know that this worked

install.packages("ggridges")
library(ggridges)
#but this did

install.packages("viridis")
library(viridis)


crazy3<- pivot_longer(datacont,c(5,6,8,18),names_to="Variable",values_to="Value",values_drop_na=TRUE)
crazy3<-crazy3[!crazy3$Value == "0", ]

crazy3J<-subset(crazy3,Group=="J")

ggplot(crazy3J, aes(x = Day, y = Variable, fill = Value)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") + #I dont really know what this line does.... the color I think so need to fix
  labs(title = 'Variables over Time') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )+
  facet_wrap(~RunNum, scale="free")

#trying to make a ridgechart showing just efficiency rating over days by plate
rawdata2J<-subset(rawdata2,Group=="J")

ggplot(rawdata2J, aes(x = Day, y = ID, fill = ER)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  labs(title = 'ER over Plates') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )+
  facet_wrap(~RunNum, scale="free")
#literally no data is showing up


#try again but with more basic code
ggplot(rawdata2J, aes(x = Day, y = ID, fill = ER)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  facet_wrap(~RunNum, scale="free")
#still no data. What is the deal




## Contamination statistics -- for appendix??



