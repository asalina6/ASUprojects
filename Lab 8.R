######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Mar 20, 2017
# LAB 8
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 8")

####Lab 8 Exercise 1 #######
closeAllConnections()
data=read.csv("turning.csv")
#sink()

mean_estimator=mean(data$angle)
mean_estimator
std_dev=sqrt(var(data$angle))
std_err= std_dev/sqrt(length(data$angle))
#For exact confidence interval, we need t-critical value
#For 95% confidence interval, we have alpha=.05
alpha=.05
crit_val=qt(alpha/2,length(data$angle)-1)

#Note: We do +/- instead of -/+ since it gives us a terribly ordered interval
#i.e.  (+,-), while it should be (-,+)
cat( "The Exact confidence interval for the mean is:(", mean_estimator+crit_val*std_err ,",",  mean_estimator-crit_val*std_err,")" )


#####we perform the 1-sample t-test #########



#STEP 1
#H0: People tend to equally turn in either direction on average. (mu_not=0)
#HA: People tend to turn in one direction more on average than the other direction. (mu_not =/=.0) 
mu_not=0
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is t
#STEP 4
#collect data and calculate test statistic
t_val=(mean_estimator-mu_not)/std_err
#STEP 5
#Calcuate the pvalue:
degfree=length(data$angle)-1
pval=2*pt(t_val,degfree)


cat("At a significance level of", alpha, ", We fail to reject the null hypothesis that people tend to equally turn in either direction on average. (Pval:",pval,", degrees of freedom:",degfree, ", t-test value:",t_val, ")")


###Do it automatically

t.test(data$angle,mu=mu_not,conf.level=0.95)


############################################3
#Lab 8 Exercise 2

rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 8")

data=read.csv("tributaries.csv")
A=data$upstream
B=data$downstream

mean_estimator_upstream=mean(A)
mean_estimator_downstream=mean(B)

mean_difference = mean_estimator_upstream-mean_estimator_downstream

#Calculated pooled deviation to calculated standard deviation.
degfree=length(A)+length(B)-2
pooled_variance=(sum((A - mean_estimator_upstream)^2)+(sum((B - mean_estimator_downstream)^2)))/(degfree)
#standard error of the difference between the means
std_error_updown=sqrt( pooled_variance/length(A) + pooled_variance/length(B)   )

#For exact confidence interval, we need t-critical value
#For 95% confidence interval, we have alpha=.05
alpha=.05
crit_val=qt(alpha/2,degfree)

cat( "The Exact 95% confidence interval for the difference of the means is:(", mean_difference+crit_val*std_error_updown ,",",  mean_difference-crit_val*std_error_updown,")" )

#Two-sample t-test by hand.

#STEP 1
#H0: the number of species does not differ between upstream and downstream locations (The mean between the two is the same). (mu_not=0)
#HA: the number of species differs between upstream and downstream locations(the mean between the two is not the same). (mu_not =/=.0) 
mu_not=0
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is t
#STEP 4
#collect data and calculate test statistic
t_val=(mean_difference-mu_not)/std_error_updown
#STEP 5
#Calcuate the pvalue:
pval=2*pt(t_val,degfree)


cat("At a significance level of", alpha, ", We fail to reject the null hypothesis that the number of species does not differ between upstream and downstream locations. (Pval:",pval,", degrees of freedom:",degfree, ", two-sample t-test, t-test value:",t_val, ")")



###Automatic test###

t.test(A,B,var.equal=T,paired=F)



###Lab 8 Exercise 3 #######
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 8")
install.packages("gplots")
library("gplots")

data=read.csv("beer.csv")
  
A=data$Straight
B=data$Curved
C=A-B
  
mean_drinking_A=mean(A)
mean_drinking_A
mean_drinking_B=mean(B)
mean_drinking_B
  
#Calculated pooled deviation to calculated standard deviation.
degfree=length(A)+length(B)-2
pooled_variance=(sum((A - mean_drinking_A)^2)+(sum((B - mean_drinking_B)^2)))/(degfree)
pooled_variance
#standard error of the difference between the means
std_err_drinking=sqrt( pooled_variance/length(A) + pooled_variance/length(B)   )
  
#For exact confidence interval, we need t-critical value
#For 95% confidence interval, we have alpha=.05
alpha=.05
crit_val=qt(alpha/2,degfree)
  
  
cat( "The Exact 95% confidence interval for the mean of straight glasses is:(", mean_drinking_A+crit_val*std_err_drinking ,",",  mean_drinking_A-crit_val*std_err_drinking,")" )
cat( "The Exact 95% confidence interval for the mean of curved glasses is:(", mean_drinking_B+crit_val*std_err_drinking ,",",  mean_drinking_B-crit_val*std_err_drinking,")" )
  
plotCI(x=c(mean_drinking_A,mean_drinking_B),uiw=std_err_drinking, pch=22,pt.bg="black",main="Error bar plot",gap=0,xlab="Means of the data.",ylab="Total time of Alcohol Consumption (minutes)")
  
####Mean difference and confidence interval #####
  
  
####Now we perform the pair t-test #######
C=A-B
mean_drinking_C=mean(C)
n=length(C)
degfree=n-1
std_dev_C=sqrt( var(C) )
std_err_C=std_dev_C/sqrt(n)
  
  
#For exact confidence interval, we need t-critical value
#For 95% confidence interval, we have alpha=.05
alpha=.05
crit_val=qt(alpha/2,degfree)
cat( "The Exact 95% confidence interval for the difference in mean drinking is:(", mean_drinking_C+crit_val*std_err_C ,",",  mean_drinking_C-crit_val*std_err_C,")" )
  
  
  
  
  
  
#Paired t-test by hand.
  
#STEP 1
#H0: The mean difference between straight glass drinkers and curved glass drinkers is zero.
#HA: The mean difference between straight glass drinkers and curved glass drinkers is not zero. 
mu_not=0
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is t
#STEP 4
#collect data and calculate test statistic
t_val=(mean_drinking_C-mu_not)/std_err_C
#STEP 5
#Calcuate the pvalue:
pval=2*(1-pt(t_val,degfree))
  
cat("At a significance level of", alpha, ", We reject the null hypothesis that The mean difference between straight glass drinkers and curved glass drinkers is zero. (Pval:",pval,", degrees of freedom:",degfree, ", paired t-test, t-test value:",t_val, ")")
  
  
##Automatic test ###
t.test(A,B,paired=T)
  
  