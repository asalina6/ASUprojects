######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Feb 7, 2017
#
######################################################

#####First set up########

# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 4")

####Lab 4 Exercise 1#######
#read data from csv file
lizarddata = read.csv("lizard.csv") 
lizardlength = lizarddata$length
n=length(lizardlength)
##calculate mean directly
sum(lizardlength)/16
##calcuate mean with function
average1=mean(lizardlength)
average1

##calculate variance directly
varlengths=sum((lizardlength-average1)^2/(n-1))
varlengths
##calculate variance with function
var(lizardlength)


##calculate std deviation by calculation
stddev=sqrt(varlengths)
stddev
#calculat by R
sd(lizardlength)


###standard error
stderror=stddev/sqrt(n)
stderror
String=paste("the confidence interval is:","(",average1-2*stderror,",",average1+2*stderror,")" )
String

#####Lab 4 Exercise 2 ####

#histogram
hist(lizardlength, xlab="X",ylab="Density", breaks=20, main="Density plot", freq=FALSE, prob=TRUE)
# new histogram with layover
hist(lizardlength, xlab="X",ylab="Density", breaks=20, main="Density plot", freq=FALSE, prob=TRUE)
x=seq(average1-4*stddev,average1+4*stddev,.1)
densities= dnorm(x,average1,stddev)
lines(x,densities, type="l")

####Lab 4 Exercise 3 ####
#Do standard error of 25
stderror25=stddev/sqrt(25)

#create sequence
x=seq(average1-4*stddev,average1+4*stddev,.1)
#calculate densities
densities= dnorm(x,average1,stddev)
densities15=dnorm(x,average1,stderror)
densities25=dnorm(x,average1,stderror25)
#plot
hist(lizardlength, xlab="X",ylab="Density", breaks=20, main="Density plot", freq=FALSE, prob=TRUE, ylim=c(0,.11))
lines(densities~x, type="l",col="red",lwd=3)
lines(densities15~x,type="l",col="blue",lwd=3)
lines(densities25~x,type="l",col=6,lwd=3)
legend("topright",title="Types of PDF of Lizard Lengths",legend=c("Best Estimate of PDF","PDF of average of 15","PDF of average of 25"),bty="n",lwd=3,col=c("red","blue",6),cex=0.7)

####Lab 4 Exercise 4 ####
mu = 12
sigma = 2
stderror10 = sigma/sqrt(10)
stderror50 = sigma/sqrt(50)

#prob of random sampel male with have horn < 11mm
pnorm(11,mu,sigma)
#prob of average length of random sample of 10 horns  < 11mm
pnorm(11,mu,stderror10)
#prob of average length of r.s. 10 horns > 12.5mm
1-pnorm(12.5,mu,stderror10)
#prob of average length of r.s. 50 horns > 12.5mm
1-pnorm(12.5,mu,stderror50)
#.025 quantile of r.s. 10
qnorm(.025,mu,stderror10)
#.975 quantile of r.s. 10
qnorm(.975,mu,stderror10)


###Lab 4 Exercise 5 ####
set.seed(3000)
mu2 = 6.2
var2=.25
sample=rnorm(10,mu2,sqrt(var2))
sample
#estimate mean
average2=mean(sample)
average2
#estimate variance
var(sample)
#estimate   stddev
stddev2=sd(sample)
stddev2
#standard error
stderror2=stddev2/sqrt(length(sample))
stderror2
#Output confidence interval
String2=paste("the confidence interval is:","(",average2-2*stderror2,",",average2+2*stderror2,")" )
String2

##Change sample size to 90.
sample2=rnorm(90,mu2,sqrt(var2))
average3=mean(sample2)
stderror3=stddev2/sqrt(length(sample2))
String3=paste("the new confidence interval of the mean (with 90 samples) is:","(",average3-2*stderror3,",",average3+2*stderror3,")" )
String3