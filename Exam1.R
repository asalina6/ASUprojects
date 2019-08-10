# Clear the console
cat("\014")  
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Feb 26, 2017
#
######################################################

#####First set up########

# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612")


######MIDTERM#####

#Exercise 1

nectardata=c(25.5,27.8,28.0,32.2,33.7,35.8,32.7,32.3,32.2,36.2,26.4,39.2,39.4,36.8,33.3,34.5  , 34.1  , 33.3  , 27.7  , 33.8  , 36.8   ,32.0  , 32.3 ,  37.4 ,  24.0)
nectarmean=mean(nectardata)
nectarmean
nectardeviation=sqrt(var(nectardata))
nectardeviation

nectarstandarderror=nectardeviation/sqrt(length(nectardata))
nectarstandarderror
#Calculate confidence interval
cat("(",nectarmean-2*nectarstandarderror, ",", nectarmean+2*nectarstandarderror,")")

hist(nectardata,freq=FALSE,xlab="Nectar Load of Returning Bee",ylab="Probability Density", ylim=c(0,.2),xlim=c(20,45))

x=seq(nectarmean-4*nectardeviation,nectarmean+4*nectardeviation,.1)
densities= dnorm(x,nectarmean,nectardeviation)
lines(x,densities,type="l")

####Exercise 2 ####
p=2/3
n=14
expectedmeanlitter=n*p
expectedmeanlitter
variancelitter=n*p*(1-p)
variancelitter
standarddevlitter=sqrt(variancelitter)
standarddevlitter

#Exactly 4
dbinom(4,n,p)
#5 or more 
1-pbinom(4,n,p)

#####Exercise 3 ###
meanheight=164
stddevheight=7

#women taller than 175
1-pnorm(175,meanheight,stddevheight)

#90% of women are expected to be below
qnorm(.9,meanheight,stddevheight)

#between 160 and 150
pnorm(160,meanheight,stddevheight)-pnorm(150,meanheight,stddevheight)


#####Exercise 4 ####
X=19
N=49
p.hat=19/49

p.prime=(X+2)/(N+4)

p.stderror=sqrt( (p.prime*(1-p.prime))/(N+4)   )

cat("(",p.prime-2*p.stderror, ",", p.prime+2*p.stderror,")")


###SMaller confidence interval with more n as p.stderror is a function of N and X. 


####EXERCISE 5 ##########33


#Step 1: State hypothesis 
# 

#STEP 1
#H0: The receiver is not telepathic (i.e., she has a same success expected from random guessing).
# p=.5
#HA: The receiver is telepathic (i.e., she has a higher rate of success than expected from random guessing).
# that means p>.5
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is X, the observed number of successes
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
xcorrect=26
ntele=40
p.tele=26/40
#STEP 5
#Calcuate the pvalue:
binom.test(26,40,p=.5,alternative=c("greater"))

#This is the illustration.
names=seq(0,40,1)
probs=dbinom(names,40,p=.5)
barplot(probs, col=c("grey","red")[(names>25) + 1], names.arg = names, xlab="Number of Successes",ylab="Probability Density",ylim=c(0,.15))



###Exercise 6

observation = c(30,15,8)

angles = c(0,20,40) 

#STEP 1 
#H0: The probability of a bird hitting a window does not change with window ang
#HA: he probability of a bird hitting a window changes  with window angle.
#STEP 2
alpha=.05
#STEP 3
#The test stastic is chi squared
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
expected=sum(observation)/length(observation)
chisquared=sum((observation-expected)^2/expected)


expectedvector=rep(expected,3)

countsMatrix2=matrix(c(observation,expectedvector), ncol=3,byrow=TRUE)
barplot(as.table(countsMatrix2), xlab = "Degrees",ylab = "Birds killed",main = "Birds killed by windows at different angles", names.arg=angles, col=c("darkblue","red"),beside=TRUE,legend=c("Observed","Expected"))

#STEP 5
#Calcuate the pvalue:


chisq.test(observation)


###Exercise 7

females=c(0,1,2,3,4)
observedterritories=c(0,20,0,0,0)

##TEST TIME###

#STEP 1
#H0:the number of females in territories having 4 fish follows a binomial distribution.
#HA:the number of females in territories having 4 fish does not follow a binomial distribution.
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is chi square.
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
#First we calculate the chi-squared by hand.
#calculate expected
binomdist=dbinom(females,length(observedterritories)-1,p=sum(females*observedterritories)/((length(observedterritories)-1)*sum(observedterritories)))
expectedterritories=binomdist*sum(observedterritories)

chisquaredterritories=sum(((observedterritories-expectedterritories)^2)/expectedterritories)
#STEP 5
#Calcuate the pvalue:

#Automatic chisquare test
MatrixCalc=matrix(c(0,20,0,0,0),nrow=1)
table=as.table(MatrixCalc)
chisq.test(table,p=binomdist)


countsMatrix=matrix(c(observedterritories,expectedterritories),nrow=2,byrow=TRUE)
barplot(as.table(countsMatrix),main="Distribution of Territories with exactly 4 fish with X amount of females", names.arg=females, col=c("darkblue","red"),beside=TRUE,legend=c("Observed","Expected"),xlab="X: Number of Females.",ylab="Number of Territories with X females.")






