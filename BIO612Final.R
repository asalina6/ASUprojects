######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: May 3rd 2017
# FINAL
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Final Exam")
require("car")
require("gplots")


####Final Exam Exercise 1- patients paired t-test #######
#Loading the data 

adat=read.csv("surgery.csv")
before=adat$before
mean(before)
sd(before)
after=adat$after
mean(after)
sd(after)
difference=after-before

##Test normality##
qqnorm(difference,main="The Normal Probability Plot of the Differences")
qqline(difference)
shapiro.test(difference)

#paired T-Test
t.test(before,after,paired=T)

#Fail to reject H_0, so there is no differences in the mean

matplot(t(adat), type="b",pch=19, col=1, lty=1,xaxt= "n",xlim=c(1,4),
        main="Weight of Patients (before and after)",xlab = "Surgery",ylab = "Weight (kg)")
axis(1,at =2:3, labels=c("before","after"))

#################################################################33
# Exercise 2- Mouse
##############################################################
rm(list = ls(all = TRUE)) 

adat=read.csv("ataxia.csv")
treatment=adat$treatment
lifespan=adat$lifespan


ex=subset(lifespan,treatment=="Exercise")
no_ex=subset(lifespan,treatment=="No exercise")

###Normality Test###
#par(mfrow=c(1,2))
qqnorm(ex,main="Normal Probability Plot of Exercise Data")
qqline(ex)
qqnorm(no_ex,main="Normal Probability Plot of No Exercise Data")
qqline(no_ex)
shapiro.test(ex)
shapiro.test(no_ex)

###Means##
mean(ex)
mean(no_ex)
###Is Data Homo?
var(ex)
var(no_ex)
sd(ex)
sd(no_ex)
leveneTest(lifespan,treatment)

#DATA IS NOT NORMAL, TRANSFORM IT###
#LOG IT###
loglifespan=log(lifespan)
logex=subset(loglifespan,treatment=="Exercise")
logno_ex=subset(loglifespan,treatment=="No exercise")


###Test for Normality###
qqnorm(logex,main="Normal Probability Plot of Log Exercise Data")
qqline(logex)
qqnorm(logno_ex,main="Normal Probability Plot of Log No Exercise Data")
qqline(logno_ex)
shapiro.test(logex)
shapiro.test(logno_ex)

###STILL FAILS. Data is non-normal#####


########Variance Testing########
leveneTest(loglifespan,treatment)

#It is homoscedastic.

#########Non-Normal + Homoscedastic = Wilcox Test########
wilcox.test(loglifespan~treatment)

#Plot it
boxplot(loglifespan~treatment,xlim=c(0.5,2.5),main="Treatment of Ataxia", ylab="Lifespan (Log(days))")
summary(loglifespan)

######################################################################3
# Exercise 3 - Shade treatments and Carbon Transfer (One Factor ANOVA) 
################################################################33
rm(list = ls(all = TRUE)) 

adat=read.csv("carbon.csv")
shade=adat$shade
transfer=adat$transfer

fit=lm(transfer~shade,data=adat)
resids=residuals(fit)
###Test the Error Distribution####

#First Normality

qqnorm(resids,main="The Normal Probability Plot of Residuals")
qqline(resids)

shapiro.test(resids)
#It is normal

#Second Homoscedasticity
leveneTest(resids~shade)
#It is homosecdastic.

####Begin ANOVA####
anov=anova(fit)
anov

###Post hoc####
tukey.fit=aov(transfer~shade,data=adat)
TukeyHSD(tukey.fit)


###Plot it######

Averages=tapply(transfer,shade,mean)
std.dev=tapply(transfer,shade,sd)
nsize=tapply(transfer,shade,length)
var=std.dev^2

MSerr=anov$`Mean Sq`[2]
dferr=anov$Df[2]

standarderr=sqrt(MSerr/nsize[1])
alpha=.05
tcrit=qt(1-alpha/2,dferr)
interval=standarderr*tcrit


plotCI(Averages, uiw=interval,xaxt="n",ylab ="Carbon Transfer (mg)",
       main="Tukey Test - Averages",xlab = 
         "Shading",ylim = c(-3,35))
axis(1, at=1:3, labels = c("Deep","None","Partial"))
text(x=1:4, y=Averages+interval+2, col= "blue", labels=c("[a]","[b]","[a,b]"))

#########################################################333
# Exercise 4 - coagulation in blood of rats - One factor ANOVA
#############################################################

rm(list = ls(all = TRUE)) 

adat=read.csv("blood.csv")
diet=adat$diet
time=adat$coag.time

fit=lm(time~diet,data=adat)
resid=residuals(fit)

###Is it normal?
qqnorm(resid,main="Normal Probability Plot of Residuals")
qqline(resid)

shapiro.test(resid)
###NOT NORMAL

#Is it homo.?
leveneTest(resid~diet)
##NOT HOMOSKEDASTIC

#########LOG time to tRANSFORM IT##########
logtime=log(time)

fit2=lm(logtime~diet,data=adat)
logresid=residuals(fit2)
###Is it normal?
qqnorm(logresid,main="Normal Probability Plot of Residuals (With log(time))")
qqline(logresid)

shapiro.test(logresid)
##NORMAL

#Is it homo.?
leveneTest(logresid~diet)
##HOMOSKEDASTIC


#########ANOVA#######
anov=anova(fit2)
anov

####Post Hoc - Tukey######
tukey.fit=aov(logtime~diet,data=adat)
TukeyHSD(tukey.fit)

######Plot it ######
Averages = tapply(logtime,diet,mean) 
std.dev = tapply(logtime,diet,sd)
nsize=tapply(logtime,diet,length)
############## Plot ##############
MSerr = anov$`Mean Sq`[2]
dferr = anov$Df[2]
standarderr = sqrt(MSerr/nsize[1])
alpha=.05
tcrit = qt(1-alpha/2, dferr)
interval = standarderr*tcrit
plotCI(Averages, uiw=interval,xlab = 
         "Diet", ylab ="Coagulation Time of Blood (log(minutes))",
       main="Tueky Test - Averages", xaxt="n",ylim = c(.5,2))
axis(1, at=1:4, labels = c("A","B","C","D"))
text(x=1:4, y=Averages+interval+.2, col= "blue", labels=c("[a,b]","[b]","[a,b]","[a]"))

##############################################################
# Exercise 5 - Influence of flies genes - Two Factor ANOVA
#################################################################
rm(list = ls(all = TRUE)) 

adat=read.csv("flies.csv")

fem=adat$female
male=adat$second.male
offspring=adat$offspring.percent

###Test the error distribution for normality/homo.####
fit=lm(offspring~fem*male,data=adat)
resid=residuals(fit)


###Is it normal?
qqnorm(resid,main="Normal Probability Plot of Residuals")
qqline(resid)

shapiro.test(resid)
###NORMAL

#Is it homo.?
leveneTest(resid~fem*male)
##HOMOSKEDASTIC

##########ANOVA 2 Factor#############
anov=anova(fit)
anov



##############################################################
# Exercise 6 - Parasite - Correlation
#################################################################
rm(list = ls(all = TRUE)) 

adat=read.csv("parasites.csv")

interv=adat$interval
spores=adat$spores

plot(interv,spores,xlab="Number of Spores",ylab="Time Interval (days)",main="Scatterplot of Spores and Time Interval")

#Non-linear. Trans the data

loginterv=log(interv)
logspores=log(spores)


plot(loginterv,logspores,xlab="Number of Spores log(count)",ylab="Time Interval log(days)",main="Scatterplot of Spores and Time Interval")

#linear

#########Test the Transformed Data########
fit=lm(logspores~loginterv,data=adat)
resid=residuals(fit)



###Is it normal?
qqnorm(resid,main="Normal Probability Plot of Residuals")
qqline(resid)

shapiro.test(resid)
###NORMAL

mod=fitted(fit)
plot(resid~mod, xlab="Fitted Values",ylab="Residuals",main="The Trans Data of Spores with the Time Interval")

###Correlation Test#####
cor.test(loginterv,logspores)


######Regression######
summary(fit)
a=-12.1840
b=4.4372
anov=anova(fit)
anov
######
#regression test
#########
stderrb=sqrt( (anov$`Mean Sq`[2])/(anov$`Sum Sq`[1]))  
t=b/stderrb
pt(t,length(loginterv)-1)





###plot it

plot(loginterv,logspores, main="Linear Regression of Time interval on Spores ", 
     xlab = "the Time Interval (log(days))", ylab = "the Number of Spores log(count)")
abline(fit, col="blue", lwd =2)

x = loginterv
y = logspores
mod2 = lm(y~x)

####prediction ######
xinput = data.frame(x=log(70))
predict(mod2,xinput,interval = "confidence") #for a mean value

