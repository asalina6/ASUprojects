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
  
  ### Lab 1 Exercise 4 - Lizards
  #set directory
  setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 1 R basics")
  
  #read data from csv file
  lizarddata = read.csv("lizards.csv") 
  
  #partition data so that we obtain all male data.
  maledata = subset(lizarddata, lizarddata$Sex == "male")
  
  #partition data so that we obtain all female data.
  femaledata = subset(lizarddata, lizarddata$Sex == "female")
  
  #calculate the averages of the male and female weights.
  meanMale = mean(maledata$Weight)
  meanFemale = mean(femaledata$Weight)
  
  #store the averages of weights into a vector
  Averages=c(meanMale,meanFemale)
  
  #store the names into a vector.
  AverageNames=c("Male","Female")
  
  #make a bar plot.
  barplot(Averages, names.arg=AverageNames, main="Average Weight of Lizards", xlab="Sex", ylab="Weight")