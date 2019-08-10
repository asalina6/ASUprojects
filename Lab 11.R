######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: April 11, 2017
# LAB 11
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 11")
#For levene's test
require("car")
require("plotrix")
#I'd rather not do it by hand
require(PMCMR)

####Lab 11 Exercise 1- Epilepsy #######
#Loading the data 
adat=read.csv("epilepsy.csv")
group=adat$group
group=factor(group,levels = c("CFS","No CFS","No seizures"))
volume=adat$volume

####(1) fit linear model to data #####

epsilepsy.lm=lm(volume~group,data=adat)

####(2) Get residuals ######

resid=residuals(epsilepsy.lm)

#####(3) Test for homoskedasticity by stripchart and Levene Test #####
stripchart(resid~group,data=adat,vertical=T,method="jitter",main="Residuals of Drug-resistant Epilepsy",ylab="residuals")
leveneTest(resid~group,data=adat)

#Equal variances confirmed

#####QQ Plot and sharpiro-wilk test#####
qqnorm(resid, main="Normal Q-Q plot",ylab="Standardized residuals")
qqline(resid)

shapiro.test(resid)

###Data is not normal. Repeat steps again, using log fit###
group=factor(group,levels = c("CFS","No CFS","No seizures"))
volume=adat$volume
adat$logvolume=log(volume)

####(1) fit linear model to data #####

epsilepsy.lm=lm(logvolume~group,data=adat)

####(2) Get residuals ######

resid=residuals(epsilepsy.lm)

#####(3) Test for homoskedasticity by stripchart and Levene Test #####
stripchart(resid~group,data=adat,vertical=T,method="jitter",main="Residuals of Drug-resistant Epilepsy (log)",ylab="residuals (log)")
leveneTest(resid~group,data=adat)

#Equal variances confirmed

#####QQ Plot and sharpiro-wilk test#####
qqnorm(resid, main="Normal Q-Q plot (log)",ylab="Standardized residuals (log)")
qqline(resid)

shapiro.test(resid)

###DATA IS STILL NOT NORMAL. MUST PERFORM KRUSKAL-WALLIS####

#changing it back to regular data#
#Loading the data
group=factor(group,levels = c("CFS","No CFS","No seizures"))
volume=adat$volume
kruskal.test(volume~group,data=adat)
#######Do Nemenyi test ########
rankvol=rank(volume)
sumranks=tapply(rankvol,group,sum)
#calculate standard error
#the k groups have equal n
k=3
n=length(volume)/k
N=length(volume)
std.err=sqrt((n*N*(N+1))/12)
#Now do the calculations
q1=abs((sumranks[1]-sumranks[2])/std.err)
p1=1-ptukey(q1,k,Inf)
q2=abs((sumranks[1]-sumranks[3])/std.err)
p2=1-ptukey(q2,k,Inf)
q3=abs((sumranks[2]-sumranks[3])/std.err)
p3=1-ptukey(q3,k,Inf)
#posthoc.kruskal.nemenyi.test(x=volume, g=group, dist="Tukey")

#########Box plot #########3
a = subset(adat$volume,group=="CFS")
b = subset(adat$volume,group=="No CFS")
c = subset(adat$volume,group=="No seizures")
Means = tapply(adat$volume,group,mean)
boxplot(volume~group,main="Investigated Volume in the Hippocampus", ylab="Volume",ylim=c(40,125))
text(1,99,labels=c("[a]"),col="blue"  )
text( c(2,3),  c(113.4,118.1),labels=c("[b]","[b]"),col="red" )

########################################################################################################################
####Lab 11 Exercise 2- daphnia #######
# resets R to fresh
rm(list = ls(all = TRUE)) 
#Loading the data 
adat=read.csv("daphnia.csv")
density=adat$density
density=factor(density,levels = c("low","medium","high"))
resist=adat$resistance


####(1) fit linear model to data #####

daphnia.lm=lm(resist~density,data=adat)

####(2) Get residuals ######

resid=residuals(daphnia.lm)

#####(3) Test for homoskedasticity by stripchart and Levene Test #####
stripchart(resid~density,data=adat,vertical=T,method="jitter",main="Residuals of Resistance to Cyanobacteria",ylab="residuals")
leveneTest(resid~density,data=adat)

#Equal variances confirmed

#####QQ Plot and sharpiro-wilk test#####
qqnorm(resid, main="Normal Q-Q plot",ylab="Standardized residuals")
qqline(resid)

shapiro.test(resid)

####NOT NORMAL######


###Data is not normal. Repeat steps again, using log fit###
adat=read.csv("daphnia.csv")
density=adat$density
density=factor(density,levels = c("low","medium","high"))
resist=adat$resistance
adat$logresist=log(resist)


####(1) fit linear model to data #####

daphnia.lm=lm(logresist~density,data=adat)

####(2) Get residuals ######

resid=residuals(daphnia.lm)

#####(3) Test for homoskedasticity by stripchart and Levene Test #####
stripchart(resid~density,data=adat,vertical=T,method="jitter",main="Residuals of Resistance to Cyanobacteria (log)",ylab="residuals (log)")
leveneTest(resid~density,data=adat)

#Equal variances confirmed

#####QQ Plot and sharpiro-wilk test#####
qqnorm(resid, main="Normal Q-Q plot (log)",ylab="Standardized residuals (log)")
qqline(resid)

shapiro.test(resid)

###DATA IS NORMAL.PERFORM ANOVA####
Anov=anova(daphnia.lm)
Anov
###################################################
#Tukey-Kramer test 
###################################################
hi = subset(adat$logresist,density=="high")
lo = subset(adat$logresist,density=="low")
me = subset(adat$logresist,density=="medium")
n=length(hi)
x_bar = tapply(adat$logresist,density,mean)
diff1 = abs(x_bar[1]-x_bar[2]) 
diff2 = abs(x_bar[1]-x_bar[3]) 
diff3 = abs(x_bar[2]-x_bar[3])
MS.err = Anov$`Mean Sq`[2]
degfree.err = Anov$Df[2]
#sample sizes are all the same in each group.
SE = sqrt(1/2*(MS.err/n+MS.err/n))  
q1 = abs(diff1/SE)
p1 = 1 - ptukey(q1, 3, degfree.err) 
q2 = abs(diff2/SE)
p2 = 1 - ptukey(q2, 3, degfree.err) 
q3 = abs(diff3/SE)
p3 = 1 - ptukey(q3, 3, degfree.err) 
#R
tukey_mod = aov(adat$logresist~density,data=adat)
TukeyHSD(tukey_mod)
######################CI plots###############3
#plot CI
alpha=.05
std.err=sqrt(MS.err/n)
tcrit = qt(1-alpha, degfree.err)
interval = std.err*tcrit
plotCI(x_bar, uiw=interval,xaxt="n", ylim=c(-.85,-.35),ylab ="Phase Shift",
       main="Averages from Tukey-Kramer Test Comparing the Density of Cyanobacteria",xlab = 
         "")

axis(1, at=1:3, labels = c("high","low","medium"))
text(x=1:3, y=x_bar+interval+.05,labels=c("[a]","[b]","[a,b]"),col="blue")
stdev = tapply(adat$logresist,density, sd)

