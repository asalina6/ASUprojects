  ######################################################
  #
  # Author: Armando Salinas
  #         asalina6@asu.edu
  # Created: Mar 27, 2017
  # LAB 9
  ######################################################
  
  #####First set up########
  # Clear the console
  cat("\014")  
  # resets R to fresh
  rm(list = ls(all = TRUE)) 
  #set directory
  setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 9")
  
  
  ####Lab 9 Exercise 1 #######
  #####################################################################################
  
  #Read the data in.
  closeAllConnections()
  data=read.csv("finches.csv")
  carrot=subset(data,diet=="carot")
  no.carrot=subset(data,diet=="no.carot")
  ################Testing Normality assumption#######################
  PHA_all=data$PHA
  PHA_carrot=carrot$PHA
  PHA_nocarrot=no.carrot$PHA
  diet=data$diet
  
                ###See Normality Graphically###
  par(mfrow=c(1,2))
  #The data appears not to be normal and skewed right.
  qqnorm(PHA_carrot,main="a.) Carotenoids Normal probability plot")
  qqline(PHA_carrot)
  
  #Appears not to be normal.
  qqnorm(PHA_nocarrot,main="b.) No Carotenoids Normal probability plot")
  qqline(PHA_nocarrot)
  
  #Appears to be normal.
  
                      ###Shapiro Test###
  
  shapiro.test(PHA_carrot)
  #w=.97, pval=.93 so definitely normal
  shapiro.test(PHA_nocarrot)
  #w=.94 and pval=.61 so definitely normal.
  
  #We confirm that the data is normal and normality is assured.
  
  ##############################Equality of Variances##########################
                            
                            ###Estimate Variances by Hand ####
  #individual variances
  Carrot.var=var(PHA_carrot)
  Carrot.var
  carrot.sd=sqrt(Carrot.var)
  carrot.sd
  noCarrot.var=var(PHA_nocarrot)
  noCarrot.var
  nocarrot.sd=sqrt(noCarrot.var)
  nocarrot.sd
  #pooled variances
  
  
                            ###Graphically by Histogram###
  histogram(PHA_carrot,type="density")
  #Skewed Right
  histogram(PHA_nocarrot,type="density")  
  #Normal 
  #histogram(~PHA_all|diet,data=data,type="density",xlab="PHA levels")
  
  #Since skewed right =/= Normal, variances appear to be not equal.
  
                            ###Levine Test###
  
  leveneTest(PHA_all~diet)
  
  ################Welch's T-Test#################
  
  #the Assumptions are met.
  t.test(PHA_carrot,PHA_nocarrot)
  
  ############ Boxplot###############
  par(mfrow=c(1,1))
  boxplot(PHA_all~diet,data,main="PHA levels data",xlab="Categories",ylab="PHA Level")


####Lab 9 Exercise 2 #######
#####################################################################################
rm(list = ls(all = TRUE))
# Clear the console
cat("\014")  
#Read the data in.
data=read.csv("smoking.csv")
strict=subset(data,behavior=="strict")
loose=subset(data,behavior=="loose")
################Testing Normality assumption#######################
CCR_all=data$CCR
CCR_strict=strict$CCR
CCR_loose =loose$CCR
behavior=data$behavior

###See Normality Graphically###
par(mfrow=c(1,2))
#Appears not to be normal and skewed right.
qqnorm(CCR_strict, main="a.) CCR in strict controls Normal probability plot")
qqline(CCR_strict)

#Appears not to be normal and skewed heavily right.
qqnorm(CCR_loose, main="b.) CCR in loose controls Normal probability plot")
qqline(CCR_loose)

#Appears to be normal.

###Shapiro Test###

shapiro.test(CCR_strict)
#w=.70, pval=1.594*10^-6, definitely reject.
shapiro.test(CCR_loose)
#w=.60852, pval=2.2*10^-16, definitely reject.

#We confirm that the data is not normal and normality is violated. 

##############################Equality of Variances##########################

###Estimate Variances by Hand ####
#individual variances
strict.var=var(CCR_strict)
strict.var
strict.sd=sqrt(strict.var)
strict.sd
loose.var=var(CCR_loose)
loose.var
loose.sd=sqrt(loose.var)
loose.sd
#Variances definitely not equal.


###Graphically by Histogram###

histogram(CCR_strict,type="density",main="CCR Levels in strict household")
#Skewed Right heavily
histogram(CCR_loose,type="density",main="CCR Levels in loose household")  
#Also Skewed Right heavily

#variances appear to be not equal.

###Levine Test###

leveneTest(CCR_all~behavior)

#################Transform the data######################

#Testing Normality
shapiro.test(log(CCR_strict))
shapiro.test(log(CCR_loose))
#Normaility is ensured!!

#Testing Homoskedasticity
leveneTest(log(CCR_all)~behavior)

#logstd devations
logstrict.var=var(log(CCR_strict))
logstrict.sd=sqrt(logstrict.var)
logstrict.sd
logloose.var=var(log(CCR_loose))
logloose.sd=sqrt(logloose.var)
logloose.sd


################ T-Test on transformed #################

#the Assumptions are met.
t.test(log(CCR_strict),log(CCR_loose),var.equal=T)


#####Box plot#############3
par(mfrow=c(1,1))
boxplot(log(CCR_all)~behavior,data,main="CCR levels data (log)",xlab="Categories",ylab="CCR Level (log)")


####Lab 9 Exercise 3 #######
#####################################################################################
rm(list = ls(all = TRUE)) 
#Read the data in.
data=read.csv("dengue.csv")
WB1=subset(data,strain=="WB1")
wild=subset(data,strain=="Wild")
################Testing Normality assumption#######################
titer_all=data$titer
titer_WB1=WB1$titer
titer_wild=wild$titer
strain=data$strain

###See Normality Graphically###
par(mfrow=c(1,2))
#Appears to be normal besides 1 outlier
qqnorm(titer_WB1,main="a.) WB1 Titer Normal Probability plot")
qqline(titer_WB1)

#Appears to be normal besides 1 outlier
qqnorm(titer_wild,main="b.) Wild Titer Normal Probability plot")
qqline(titer_wild)

#Appears to be normal.

###Shapiro Test###

shapiro.test(titer_WB1)
#w=.42316, pval =1.19*10^-6, Reject
shapiro.test(titer_wild)
#w=.42,pval=1.09*10^-6, Reject

#We confirm that the data is not normal and normality is violated. 
##############################Equality of Variances##########################

###Estimate Variances by Hand ####
#individual variances
wb1.var=var(titer_WB1)
wb1.var
wb1.sd=sqrt(wb1.var)
wb1.sd
wild.var=var(titer_wild)
wild.var
wild.sd=sqrt(wild.var)
wild.sd
#Variances definitely not equal.


###Graphically by Histogram###

histogram(titer_WB1,type="density",main="a.) Density histogram of Titer from WB1")
#Skewed Right heavily
histogram(titer_wild,type="density",main="b.) Density histogram of Titer from wild")  
#Also Skewed Right heavily

#variances appear to be not equal.

###Levine Test###

leveneTest(titer_all~strain)
#Somehow we fail to reject. Pval=.36.

#################Transform the data######################

#Testing Normality
shapiro.test(log(titer_WB1))
shapiro.test(log(titer_wild))
#Normaility is still violated under log
shapiro.test(sqrt(titer_WB1)+.5)
shapiro.test(sqrt(titer_wild)+.5)
#Normaility is still violated under root

#It appears transformation will not help normality. We must do
# Mann-Whitney test


################ Mann-Whitney test/wilcox #################

#the Assumptions are met.
wilcox.test(titer_WB1,titer_wild)

####Box plot#####
par(mfrow=c(1,1))
boxplot(titer_all~strain,data,main="Titer levels data",xlab="Categories",ylab="Titer")

