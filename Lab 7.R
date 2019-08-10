######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Feb 23, 2017
#
######################################################

#####First set up########
# Clear the console
cat("\014")  
# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 7")

####Lab 7 Exercise 1 #######

data=read.csv("yawn.csv")
observation=table(data$eyes, data$behavior)

mosaicplot(observation,,col=c("red","green"))


noyawn=sum(observation[1,])
yawn=sum(observation[2,])
covered=sum(observation[,1])
visible=sum(observation[,2])
total=sum(observation)

#Calculate expected values

Expectednoyawncovered=(noyawn*covered)/total
Expectedyawncovered=(yawn*covered)/total
Expectednoyawnvisible=(visible*noyawn)/total
Expectedyawnvisible=(yawn*visible)/total

#Give it a skeleton
expected=observation

expected[1,1]=Expectednoyawncovered
expected[1,2]=Expectednoyawnvisible
expected[2,1]=Expectedyawncovered
expected[2,2]=Expectedyawnvisible

mosaicplot(expected,,col=c("red","green"))


# Step 1 - Hypothesis
#H0: Yawning is independent of eye visibility
#HA: Yawning is not independent of yawn visibility
# Step 2 - Choose signifance level
alpha=.05
#step 3 - Test stastic
#Chi squared
#step 4 Calculate the test stastic from observed and expected.
chisquared=sum((observation-expected)^2/expected)
chisquared

#Calculate pval
#First calculate df: (r-1)(c-1)
df=1 #(2-1)(2-1)=1
#p value
pval=1-pchisq(chisquared,df)
pval
print("we fail to reject the null hypothesis that Yawning is independent of eye visibility at a signifiance level of .05, with pval of .1205. This was performed by Chi square test value of 2.411 with 1 degree of freedom.")

chisq.test(observation,correct = FALSE)

#####Lab 7 Exercise 2 ####3

data2=read.csv("bats.csv",header=T)
observation2=table(data2$Fed.,data2$Regurgitated.)
mosaicplot(observation2,,col=c("red","green"))

hungry=sum(observation2[,1])
partially=sum(observation2[,2])
no=sum(observation2[1,])
regurgitated=sum(observation2[2,])
total2=sum(observation2)

expectedhungryno=(hungry*no)/total2
expectedhungryregurg=(hungry*regurgitated)/total2
expectedpartialno=(partially*no)/total2
expectedpartialregurg=(partially*regurgitated)/total2

expected2=observation2

expected2[1,1]= round(expectedhungryno)
expected2[1,2]= round(expectedhungryregurg)
expected2[2,1]=round(expectedpartialno)
expected2[2,2]=round(expectedpartialregurg)

mosaicplot(expected2,,col=c("red","green"))

fisher.test(observation2)
