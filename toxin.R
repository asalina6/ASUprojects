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

### Lab 1 Exercise 3 - Toxin

#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 1 R basics")

#read in the data from csv file.
toxindata = read.csv("toxin.csv") 

#plot the concentration of toxin.
plot(toxindata$Concentration~toxindata$Time, main="Concentration of Toxin", xlab="Time (seconds)", ylab="Concentration")

#plot the log of the concentration of toxin.
plot(toxindata$Time, log(toxindata$Concentration),xlab="Time (seconds)", main="Log of Concentration", ylab="log(Toxin) (Concentration)")
