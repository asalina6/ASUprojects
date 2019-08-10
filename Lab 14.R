######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: April 27, 2017
# LAB 14
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 14")
require("car")
require("plotrix")


####Lab 13 Exercise 1- Katydid regression practice #######
#Loading the data 
#Here X=log frequency, Y=log length
adat=read.csv("vision.csv")

model=lm(SWS1~population*water,data=adat)
model
anova.model=anova(model)
anova.model

#######Test assumtpions#####

res=residuals(model)
qqnorm(res)
shapiro.test(res)

leveneTest(res~population*water,data=adat)
#######################

#Confidence Intervals

MSresid=anova.model$Mean[4]
DFresid=anova.model$Df[4]

#getting sample size

n=tapply(adat$SWS1,list(adat$population,adat$water),length)
n

avg=tapply(adat$SWS1,list(adat$population,adat$water),length)
avg

###SE
SE=sqrt(MSresid/n)
tcrit=qt(.975,DFresid)
interval=SE*tcrit

lowerbound=avg-interval
upperbound=avg+interval

for(i in 1:length(avg))
{
cat("the 95% of", avg[i], "is (",lowerbound[i],",",upperbound[i],")\n")
}

##plotting

library(gplots)

#Clear water
plotCI(x=1:2,
       y=avg[,1],
       uiw=interval[,1],
       type="b", #plot lines and both points
       xaxt="n",
       col="red",
       ylim=c(6,11),
       ylab="SWS1",
       xlab="Population"
       )
#tea water
plotCI(x=1:2+.01,
       avg[,2],
       uiw=interval[,2],
       type="b",
       xaxt="n",
       add=TRUE,
       ylim=c(6.5,10),
       col="gold")


axis(1,at=1:2,labels=c("Swamp","Spring"))
legend("topright",
       lty=1,
       col=c("red","gold"),
       legend=c("Tea","Water"),
       bty="n")







