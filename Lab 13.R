######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: April 25, 2017
# LAB 13
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 13")
require("car")
require("plotrix")


####Lab 13 Exercise 1- Katydid regression practice #######
#Loading the data 
#Here X=log frequency, Y=log length
adat=read.csv("katydid.csv")
y=adat$log.freq
x=adat$log.length
plot(x,y,xlab="Length (log(mm))",ylab="Frequency (Log(kilohertz))",main="Scatterplot of Log(length) vs Log(frequency)")


##Do a least squares regression#####
n=length(x)
xbar=mean(x)
ybar=mean(y)
Covar=sum(  (x-xbar)*(y-ybar) )
Sumsquares=sum( (x-xbar)^2  )
b=Covar/Sumsquares
a=ybar-b*xbar
#lines(x,a+b*x) #This is a + bx
yhat=a+b*x

MSresid=sum((y-(a+b*x))^2)/(n-2)
sb=sqrt(  (MSresid)/(sum(   (x-xbar)^2)) )

alpha=.05
degfree=n-2
tcrit=qt(1-alpha/2,degfree)
cat("The 95% confidence interval of ",b, "is:(",b-tcrit*sb,b+tcrit*sb,")")

###Hypothesis for B####
#STEP 1: State Hypothesis
#H0: There is no significant regression between length and frequency
#HA: There is a significant regression between length and frequency

#STEP 2: Choose significance level
alpha=.05

#STEP 3: Choose test statistic
#T statistic, with the t-test.

#STEP 4: Calculate the test stastic from observed and expected
tval=(b-0)/sb
degfree=n-2
#STEP 5: P value
#Times by two since this is a two-sided test.
pval=2*(1-pt(abs(tval),degfree))
cat("we reject the null hypothesis that there is no significant regression between length and frequency at a signifiance level of",alpha,"with a pval of:",pval,".This was performed by one sample t-test which gave a t value of:", tval, "with:",degfree, "degrees of freedom.")


####Calculate the coefficient of determination
SSr = sum((yhat-ybar)^2)
SSt = sum((y-ybar)^2)
Rsq = SSr/SSt
cat("The coefficient of determination is ", Rsq)



#####Regression (automatic)####
fit=lm(y~x)
summary(fit)

####Scatterplot+abline######
plot(x,y,xlab="Log(length) (Log(mm))",ylab="log(Frequency) (log(kilohertz))",main="Scatterplot of Log(length) vs Log(frequency)")
abline(a,b)


##########################Exercise 2 ##############################3
res=residuals(fit)
fitvals=fitted(fit)
plot(res~fitvals,ylab="Residuals",xlab="Fitted values",main="Residuals vs Fitted values")
#linearity and homogenous
qqnorm(res) #tests normality

################Exercise 3 ####################

#y=a+b*x
xinput=1.83
y.predict=a+b*xinput
y.predict

#individual
se = sqrt(MSresid*(1+1/n+(xinput-xbar)^2/sum((x-xbar)^2)))
y.upper = y.predict + tcrit*se
y.lower = y.predict - tcrit*se
cat("The 95% confidence interval of the Individual prediction",y.predict,"is (",y.lower,",",y.upper,")")
#mean
se = sqrt(MSresid*(1/n+(xinput-xbar)^2/sum((x-xbar)^2)))
y.upperm = y.predict + tcrit*se
y.lowerm = y.predict - tcrit*se
cat("The 95% confidence interval of the Mean prediction is ", y.predict , "and the 95% confidence interval is (",
    y.lowerm, ",", y.upperm, ")")

########Exercise 4 #############
xinput = data.frame(x=1.83)
predict(fit,xinput,interval = "predict") #for an individual value
predict(fit,xinput,interval = "confidence") #for a mean value

