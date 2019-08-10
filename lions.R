######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Jan 12th, 2017
#
######################################################

rm(list = ls(all = TRUE))  # resets R to fresh

### Lab 1 Exercise 5 - lions

#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 1 R basics")

#read data from csv file
liondata = read.csv("lions.csv") 

#partition data set into males.
malesdata=subset(liondata,liondata$Sex == "male")
#partition data set into females.
femalesdata=subset(liondata,liondata$Sex == "female")

#this is the original plot (just males)
plot(malesdata$Black, malesdata$Age, xlab="Proportion black", ylab="Age", main="Male Lions: Proportion vs Age")

#second plot with the added female points.
plot(malesdata$Black~malesdata$Age, ylab="Proportion black", xlab="Age", main="Lions: Proportion vs Age")
points(femalesdata$Black~femalesdata$Age, col="red")
legend(1, pch=c(1,1),legend=c("Males", "Females"),
       col=c("black", "red"),bty="o", cex=0.8)