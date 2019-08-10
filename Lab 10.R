  ######################################################
  #
  # Author: Armando Salinas
  #         asalina6@asu.edu
  # Created: April 4, 2017
  # LAB 10
  ######################################################
  
  #####First set up########
  # Clear the console
  cat("\014")  
  # resets R to fresh
  rm(list = ls(all = TRUE)) 
  #set directory
  setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 10")
  
  
  ####Lab 10 Exercise 1 #######
  
  #Read the data in.
  closeAllConnections()
  data=read.csv("crabs.csv")
  
  stripchart(rate~group,data=data,vertical=T,main="The Rate of Heat gained in Four Types of Crab",method="jitter",ylab="The Rate of Heat Gained (Celsius/log(minute))",xlab="Types of Crab")
  all_rates=data$rate
  group=data$group
  #Use Tapply to obtain the means of each group.
  xarray=tapply(all_rates,group,mean)
  #Plot points of the means and then add a legend.
  points(xarray,pch=17,col="red")
  legend("topright",c("Average"),pch=17,col="red")
  
  ##Partition groups for later
  females=subset(all_rates,group=="Female")
  males=subset(all_rates,group=="Intact male")
  nominor=subset(all_rates,group=="No minor")
  nomajor=subset(all_rates,group=="No major")
  
  #####Lab 10 Exercise 2: ANOVA by Hand #########
 
  
  ####STEP 1: HYPOTHESIS#####
  
  #H0: The means of the rate of heat gain are equal for all 4 types of crabs
  #HA: There is at least one mean different from the other means between the 4 types of crab
  
  ####STEP 2: significance level####
  alpha=.05
  
  ###STEP 3: Test Statistic #####
  
  #The test statistic is F= MSGroups/MSError
  
  ###STEP 4:  Calculate the test stastic from observed and expected####
  
  
  #Calculate Average of All, X
  X=mean(all_rates)
  #Calculate the number of categories/groups
  k=4
  #Calculate the n of each category/group
  Narray=tapply(all_rates,group,length)
  #Calculate total N
  N=sum(Narray)
  
  ###Calcuate MS Groups####
  
   ##MSGroups=SSGroups/vgroups##
    
    #SS Groups
    SSGroups=sum(Narray*(xarray-X)^2)
    SSGroups
    #vgroups
    degfreegroups=k-1
    degfreegroups
    #MSGroups
    MSgroups=SSGroups/degfreegroups
    MSgroups
    
   ###Calcuate MSerr####
    
    ##MSerr=SSerr/verr##
    
    #SSerr
    SSerr=sum((females-xarray[1])^2)+sum((males-xarray[2])^2)+sum((nominor-xarray[4])^2)+sum((nomajor-xarray[3])^2)
    SSerr
    #Verr
    degfreeerr=N-k
    degfreeerr
    #MSerr
    MSerr=SSerr/degfreeerr
    MSerr
  ###STEP 5: P values ######
  Fval=MSgroups/MSerr
  Fval
  #If H0 true, F follows F distribution
  Pval=1-pf(Fval,degfreegroups,degfreeerr)
  Pval
  cat("we reject the null hypothesis that The means of the rate of heat gain are equal for all 4 types of crabs at a signifiance level of",alpha,"with a pval of:",Pval,".This was performed by ANOVA test which gave a fvalue of:", Fval, "with:",degfreegroups, "degrees of freedom of the group and",degfreeerr,"degrees of freeodm of the error.")
  
  
 #####Lab 10 Exercise 3: Automatic ANOVA############
  
fit=lm(rate~group,data=data)
anova(fit)

#####Lab 10 Exercise 4: Make CI for each group#########
#The mean heat of each group
xarray
standard.err=sqrt(MSerr/Narray)  
tcritical=qt(1-alpha/2,degfreeerr)
LowerCI=xarray-standard.err*tcritical
UpperCI=xarray+standard.err*tcritical
for(i in 1:length(LowerCI))
{
  cat("The exact Confidence Interval for:",xarray[i]," is:(",LowerCI[i],UpperCI[i],").\n")
}
#####Lab10 Exercise 5: Turkey Test by Hand#####
#If the difference is negative, we could always take absolute value.
difference1=xarray[1]-xarray[2] #female vs male
difference2=xarray[1]-xarray[3] #female vs no major
difference3=xarray[1]-xarray[4] #femael vs no minor
difference4=xarray[3]-xarray[2] #male vs no major
difference5=xarray[2]-xarray[4] #male vs no minor
difference6=xarray[3]-xarray[4] #major vs minor

#the sample size of the group is the same, so the SE is the same.
Standard.err=sqrt(.5*(2*MSerr/Narray[1]))
q1=(difference1/Standard.err)
p1=1-ptukey(q1,k,degfreeerr)
q2=(difference2/Standard.err)
p2=1-ptukey(q2,k,degfreeerr)
q3=(difference3/Standard.err)
p3=1-ptukey(q3,k,degfreeerr)
q4=(difference4/Standard.err)
p4=1-ptukey(q4,k,degfreeerr)
q5=(difference5/Standard.err)
p5=1-ptukey(q5,k,degfreeerr)
q6=(difference6/Standard.err)
p6=1-ptukey(q6,k,degfreeerr)
#Will present them in a table.

######################################
#Turkey automated test - Lab 10 Exercise 6 
##############
fit2=aov(fit)
Tukey=TukeyHSD(fit2,"group",ordered=T)
###########################
# Tukey bar plots error - Lab 10 Exercise 7
#####################
plot(Tukey,las=1)
