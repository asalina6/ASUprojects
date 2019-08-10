######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Feb 22, 2017
#
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 6")

####Lab 6 Exercise 1 #######

data=read.csv("cats.csv", header = TRUE)
observed=data$falls
names=data$month

barplot(observed, names.arg=names, xlab="Months",ylim=c(0,20), ylab="Amount of Cats",main="Distribution of 129 Cats injured by falling in Manhattan.")


#STEP 1
#H0: Falls are (equally) randomly distributed throughout the year
#That means for each month, the amount of falls expected is 119/12
expecval=119/12
#HA: Falls are not (equally) randomly distributed throughout the year 
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is chi squared
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
#First we calculate the chi-squared by hand.
expected=rep(expecval,length(names))
chisquared=sum(((observed-expected)^2)/expected)
#STEP 5
#Calcuate the pvalue:
paraesti=0
degfree=length(names)-paraesti-1
pval=1- pchisq(chisquared,degfree)


print("we reject the null hypothesis that the the probability that falls are equally distributed across months of the year, at a signifiance level of .05, with pval of .037. This was performed by Chi square test value of 20.664 with 11 degrees of freedom.")


table1=as.table(observed,expected)

chisq.test(table1)

########Lab 6 Exercise 2 ######################

#With a ratio of 1:2:1 of red:pink: white of n=40 individuals, it
#is expected that we have 10 red, 20 pink, 10 white
observed2=c(10,21,9)
expected2=c(10,20,10)


countsMatrix2=matrix(c(10,21,9,10,20,10),nrow=2,byrow=TRUE)
barplot(as.table(countsMatrix2),main="Distribution of Snapdragon colors in a cross of heterozygous individuals.", names.arg=c("red", "pink", "white"), col=c("darkblue","red"),beside=TRUE,legend=c("Observed","Expected"),xlab="Colors",ylab="Amount of individuals in each color")




#STEP 1
#H0: In a cross between heterozygous individuals, 
#the ratio of red:pink:white offspring is 1:2:1.
#HA: In a cross between heterozygous individuals, 
#the expected ratio of red:pink:white offspring is NOT 1:2:1.
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is chi square
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
#First we calculate the chi-squared by hand.
chisquared2=sum(((observed2-expected2)^2)/expected2)
#STEP 5
#Calcuate the pvalue:
paraesti=0
degfree2=length(observed2)-paraesti-1
pval2=1- pchisq(chisquared2,degfree2)
print("we fail to reject the null hypothesis that In a cross between heterozygous individuals, the ratio of red:pink:white offspring is 1:2:1., at a signifiance level of .05, with pval of .92774. This was performed by Chi square test of value 0.15 with 2 degrees of freedom.")


MatrixCalc2=matrix(c(10,21,9),nrow=1)
table2=as.table(MatrixCalc2)
chisq.test(table2,p=c(.25,.5,.25))

###Now we count 4000 flowers wih 1000 red, 2100 white, 900 white.
#I WILL DO THIS BY HAND TO DOUBLE CHECK
observed3=c(1000,2100,900)
expected3=c(1000,2000,1000)
((observed3-expected3)^2)/expected3
chisquared3=sum(((observed3-expected3)^2)/expected3)
paraesti=0
degfree3=length(observed3)-paraesti-1
pval3=1- pchisq(chisquared3,degfree3)
print("we reject the null hypothesis that In a cross between heterozygous individuals, the ratio of red:pink:white offspring is 1:2:1., at a signifiance level of .05, with pval of .0005. This was performed by Chi square test of value 15 with 2 degrees of freedom.")


#Automatic square test
MatrixCalc3=matrix(c(1000,2100,900),nrow=1)
table3=as.table(MatrixCalc3)
chisq.test(table3,p=c(.25,.5,.25))

############################
#Lab 6 Exercise 3
###########33
observedburrows=c(12,25,23,19,17)
pupvals=c(0,1,2,3,4)
totalpups=sum(4*observedburrows)
pupsinfected=sum(observedburrows*pupvals)
p=pupsinfected/totalpups
p

###Assume p=pupsinfected/totalpups
binompdf=dbinom(pupvals,4,p)
expectedburrows=binompdf*sum(observedburrows)
expectedburrows

countsMatrix3=matrix(c(observedburrows,expectedburrows),nrow=2,byrow=TRUE)
barplot(as.table(countsMatrix3),main="Distribution of Burrows with X amount of infected pups.", names.arg=pupvals, col=c("darkblue","red"),beside=TRUE,legend=c("Observed","Expected"),xlab="X: Number of infected pups.",ylab="Number of Burrows with X infected pups")

##TEST TIME###

#STEP 1
#H0:the number of parasitized pups follows a binomial distribution.
#HA:the number of parasitized pups does not follow a binomial distribution.
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is chi square, number of infected pups in a burrow.
#STEP 4
#collect data and calculate test statistic
#This is where we have the raw data.
#First we calculate the chi-squared by hand.
chisquaredburrows=sum(((observedburrows-expectedburrows)^2)/expectedburrows)
#STEP 5
#Calcuate the pvalue:
paraesti=0
degfreeburrows=length(observedburrows)-paraesti-1
pvalburrows=1-pchisq(chisquaredburrows,degfreeburrows)
print("we reject the null hypothesis that number of parasitized pups follows a binomial distribution, at a signifiance level of .05, with pval of .000003. This was performed by Chi square test of value 30.78 with 4 degrees of freedom.")
#
