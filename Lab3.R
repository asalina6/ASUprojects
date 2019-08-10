######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Jan 31st, 2017
#
######################################################

#####First set up########

# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory


##LAB 3 EXERCISE 1 #######
mu=100
sigma2=400
sigma=sqrt(sigma2)
x=seq(mu-4*sigma,mu+4*sigma,length=1000)

normVal= dnorm(x,mu,sigma)
cumnormVal=pnorm(x,mu,sigma)
plot(x,normVal,main="PDF", xlab="X (lizard lengths)", ylab="Probability density of X (lizardlengths)")
plot(x,cumnormVal, main="CDF",xlab="X (lizard lengths)",ylab="Probability")

####LAB 3 Exercise 2 ######
ans1=dnorm(75,mu,sigma)
ans1
ans2=pnorm(75,mu,sigma)
ans2
ans3= 1-pnorm(120,mu,sigma)
ans3
ans4=pnorm(115,mu,sigma)-pnorm(95,mu,sigma)
ans4
ans5=(1 - pnorm(60,mu,sigma))-(1-pnorm(140,mu,sigma))
ans5
ans6=(1 - pnorm(mu-1.3*sigma,mu,sigma))-(1-pnorm(mu+1.3*sigma,mu,sigma))
ans6
ans7=pnorm(mu-1.5*sigma,mu,sigma) + (1-pnorm(mu+1.5*sigma,mu,sigma))
ans7
ans8=pnorm(mu-.7*sigma,mu,sigma)
ans8
ans9=c(qnorm(.25,mu,sigma),qnorm(.50,mu,sigma),qnorm(.75,mu,sigma),qnorm(.9999,mu,sigma))
ans9
ans10=qnorm(2/3,mu,sigma)
ans10
ans11=qnorm(.8,mu,sigma)
ans11

###### LAB 3 Exercise 3 ##############

mu=100

sigma21=100
sigma22=400
sigma23=625

sigma1=sqrt(sigma21)
sigma2=sqrt(sigma22)
sigma3=sqrt(sigma23)

x=seq(mu-4*sigma,mu+4*sigma,length=1000)

normVal1= dnorm(x,mu,sigma1)
normVal2= dnorm(x,mu,sigma2)
normVal3= dnorm(x,mu,sigma3)

cumnormVal1= pnorm(x,mu,sigma1)
cumnormVal2= pnorm(x,mu,sigma2)
cumnormVal3= pnorm(x,mu,sigma3)

#cumnormVal=pnorm(x,mu,sigma)
names=c("100","400", "625")
plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9), "red", "forestgreen")
#PDF
plot(x,normVal,main="PDF (All Variances)", xlab="X (lizard lengths)", ylab="Probability density of X (lizardlengths)", col=rgb(r=0.0,g=0.0,b=0.9), lty=1, cex.lab=1, lwd=2, pch="-")
lines(x,normVal2, col="red",pch=20, lty=2)
lines(x, normVal3, col="forestgreen",pch=20, lty=3)
legend("topright", names, cex=.8, col=plot_colors, lty=c(2,2,3), lwd=2, bty="n", title ="Variance values")
#CDF
plot(x,cumnormVal,main="CDF (All Variances) ", xlab="X (lizard lengths)", ylab="Probability", col=rgb(r=0.0,g=0.0,b=0.9), lty=1, cex.lab=1, lwd=2, pch="-")
lines(x,cumnormVal2, col="red",pch=20, lty=2)
lines(x, cumnormVal3, col="forestgreen",pch=20, lty=3)
legend("bottomright", names, cex=.8, col=plot_colors, lty=c(2,2,3), lwd=2, bty="n", title ="Variance values")

######Lab 3 Exercise 4

# Spies < 180 men, 173 female. All units in cm.
mumen=177
sigmamen=7.1
muwomen=163.3
sigmawomen=6.4

ans12=1-pnorm(180,mumen,sigmamen)
ans12
ans13=pnorm(173,muwomen,sigmawomen)
ans13
####If men have an acceptance rate of .663628 (.33631 failure), we want women to have 
#this same acceptance rate (or failure rate. its symmetric). 
inverseAns=qnorm(.663628,muwomen,sigmawomen)
doublecheck=1-pnorm(inverseAns,muwomen,sigmawomen)
ans14=inverseAns
ans14
ans15=(183-180)/sigmamen
ans15

### Lab 3 Exercise 5

#pick weird numbers

meanval=38830
variances=400
##R is not symbolic I believe. I gave it some values.
stddev=sqrt(variances)

ans16=pnorm(meanval+1*stddev,meanval,stddev)-pnorm(meanval-1*stddev,meanval,stddev)
ans16
ans17=pnorm(meanval+5*stddev,meanval,stddev)-pnorm(meanval-5*stddev,meanval,stddev)
ans17
