######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Jan 20th, 2017
#
######################################################

#####First set up########

# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 2")

##########Lab 2 Exercise 1 - Manually calculate a certain probablity##############

#Formula for combination of n choose r is n!/(r! (n-r)!). In this case, given 9 children
# how many combinations are there such that there are 3 females and 6 males?
TotalCombination = factorial(9)/(factorial(3)*factorial(6))

#We now calculate the probability of this occuring. Since p=.5 and (1-p)=.5
#Then the probability of this happening is p^n = (.5)^9
probabilityof9choose3=.5^9

#The total probability is the probabilityof9choose3 * TotalCombinations
TotalProbability = probabilityof9choose3*TotalCombination
#output
TotalProbability


##########Lab 2 Exercise 2 - use program to calculate all probablity##############

#Note we calculate with i-1 since x=0 is a possibility and the array starts with index of 1.
###DEFINE Prob and NumLiz
NumLiz=9
Prob=.5

Probabilitydistribution=c(0,0,0,0,0,0,0,0,0,0)
TotalCombinationVector=c(0,0,0,0,0,0,0,0,0,0)
Probabilityofeachevent=c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:10)
  {
  Probabilityofeachevent[i]=(Prob^(i-1))*((1-Prob)^(NumLiz-i+1))
  TotalCombinationVector[i]=factorial(NumLiz)/(factorial(i-1)*factorial(NumLiz-(i-1)))
  Probabilitydistribution[i]=Probabilityofeachevent[i]*TotalCombinationVector[i]
}


#########Lab 2 Exercise 3 - Use dbinom to calculate mean.
binomProbabilitydistribution=c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:10){
  binomProbabilitydistribution[i]=dbinom(i-1,NumLiz,Prob)
}
barplot(binomProbabilitydistribution,names.arg=c("0","1","2","3","4","5","6","7","8","9"),main="Binomial Distribution of Females out of 9 Children", ylim=c(0,.3),xlab="Number of Female Children", ylab="Probability of the number of Female Children")

#We calculate the mean by hand
#First create the possible values of the random binomial variable X (female children)
#mean = sum(x*P(x))
outcomes=c(0,1,2,3,4,5,6,7,8,9)
#Next multiply that with each probabilty and sum it up to get the mean.
productOutcomesProbability=outcomes*binomProbabilitydistribution
meanVal=sum(productOutcomesProbability)

#Use shortcut formula to calculate mean for binomial distribution: n*p. 
meanValShortCut=NumLiz*Prob

#we calculate the formal variance by hand
#var= sum( (x-mean)^2 Px(X))
VarianceBinom = c(0,0,0,0,0,0,0,0,0,0)
VarianceBinom = seq(1,10,1)
for(i in 1:10)
{
VarianceBinom[i]=(i-1-meanVal)^2*binomProbabilitydistribution[i]
}
Varval=sum(VarianceBinom)
#Now we take the square root of this to get std dev
stddev=sqrt(Varval)
#Use shortcut formula to calculate variance of binom dist: np(1-p)
shortcutVarVal=NumLiz*Prob*(1-Prob)
#Use shortcut to calculate stddev
shortcutstddev=sqrt(shortcutVarVal)

#####Lab 2 Exercise 4 - made a PDF and CDF on top of each other
Cummulative= c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:10)
{
Cummulative[i]=pbinom(i-1,9,.5)
}
barplot(Cummulative, main="PDF (red) vs CDF (Grey)",names.arg=c("0","1","2","3","4","5","6","7","8","9"),xlab="Number of Females out of 9 children.", ylab="Probabilty of having X females out of 9 children.")
par(new=TRUE)
barplot(binomProbabilitydistribution,col="red",add=TRUE)
#par(new=TRUE) 

#########Lab 2 Exercise 5 - Answer the following questions 

ans1=dbinom(4,9,.5)
ans2=dbinom(7,9,.5)
ans3=pbinom(7,9,.5)
ans4=1-pbinom(3,9,.5)
#This is probability of 3 or less females
ans5=pbinom(3,9,.5)
#add up the probabilities
ans6=(1-pbinom(6,9,.5))+(1-pbinom(6,9,.5))

#####Lab 2 Exercise 6 - Answer the following questions
ans7 = qbinom(.25,9,.5)
ans8 = qbinom(.05,9,.5)
ans9 = qbinom(.95,9,.5)
ans10=qbinom(.5,9,.5)

#####Lab 2 Exercise 7 - experiment
Experiments = rbinom(100,9,.5)
MeanExperi=mean(Experiments)