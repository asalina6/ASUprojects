######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: April 16, 2017
# LAB 12
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 12")


####Lab 12 Exercise 1- Biodiversity #######
#Loading the data 
#Here X=attachment, Y=Birds
adat=read.csv("green.csv")
x=adat$attachment
y=adat$birds
plot(x,y,xlab="Attachment",ylab="Birds",main="Scatterplot of Attachment vs Birds")

#Assumptions:
#Random and independent sampling.
#X and Y have a bivariate normal distribution.
#The relation between X and Y is linear.
#Homogeneity of variances

#Calculate r, the estimator of rho

x.avg=mean(x)
y.avg=mean(y)
r=(  sum(  (x-x.avg)*(y-y.avg)))/sqrt((sum((x-x.avg)^2)*(sum((y-y.avg)^2))))

#Calculate 95% CI of r
n=length(x)
sr=sqrt( (1-r^2)/(n-2)  )

#Fishers transform
z=1/2*log((1+r)/(1-r))
sz=sqrt(1/(n-3))
alpha=.05
Zalpha=qnorm(1-alpha/2,0,1)

Interval=Zalpha*sz
zlower=z-Interval
zupper=z+Interval
cat("The 95% CI of z=",z, " is:",zlower,",",zupper)

###Inverse
rupper=( exp(2*zupper) -1 )/(exp(2*zupper)+1)
rlower=(exp(2*zlower) -1 )/(exp(2*zlower)+1)
cat("The 95% CI of r=",r, " is:",rlower,",",rupper)


###Hypothesis for r####
#STEP 1: State Hypothesis
#H0: There is no significant correlation between attachment and birds
#HA: There is a significant  correlation between attachment and birds

#STEP 2: Choose significance level
alpha=.05

#STEP 3: Choose test statistic
#T statistic, with the t-test.

#STEP 4: Calculate the test stastic from observed and expected
tval=r/sr
degfree=n-2
#STEP 5: P value
#Times by two since this is a two-sided test.
pval=2*(1-pt(tval,degfree))
cat("we reject the null hypothesis that there is no correlation between attachment and birds at a signifiance level of",alpha,"with a pval of:",pval,".This was performed by student's simple t-test which gave a t value of:", tval, "with:",degfree, "degrees of freedom.")


###AUTOMATIC FUNCTIONS
cor(x,y)
cor.test(x,y)



#####Lab 12 Exercise 2 - trillium #####
#Loading the data 
# resets R to fresh
rm(list = ls(all = TRUE)) 
adat=read.csv("trillium.csv")
dist=adat$distance
recruit=adat$recruitment
plot(dist,recruit,xlab="Distance",ylab="Recruit", main="Distance vs Recruit")

###Transform only log(recruit)
logrecruit=log(recruit)
plot(dist,logrecruit,xlab="Distance",ylab="Recruit(log)", main="Distance vs Log(recruit)")

###Transform onlly log(dist)
logdist=log(dist)
plot(logdist,recruit,xlab="Distance (log)",ylab="Recruit", main="Log(distance) vs Recruit")

###Transform both
plot(logdist,logrecruit,xlab="Distance (log)",ylab="Recruit (log)", main="Log(distance) vs Log(recruit)")


####Distance vs Log(recruit) is the best choice###

###Automatic Tests##
cor(logdist,recruit)
cor.test(logdist,recruit)


################Exercise 3: Wine #################
#Loading the data 
# resets R to fresh
rm(list = ls(all = TRUE)) 
adat=read.csv("wine.csv")
judge.A=adat$judge.A
judge.B=adat$judge.B
plot(judge.A,judge.B,xlab="Judge A's rating", ylab="Judge B's rating", main="Judge A's rating vs Judge B's rating on the same wine sample.")
cor.test(judge.A,judge.B)
cor.test(judge.A,judge.B,method="spearman")

###Let's decide which is more appropriate###