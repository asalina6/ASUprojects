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

### Lab 1 Exercise 2 - BMI
getwd()   #Gets the working directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 1 R basics") #sets the working directory
getwd()   #Double checking to make sure directory is set

mydata = read.csv("BMI.csv") #This reads in a csv file

c("The size of the data frame is:") 
dim(mydata) #Gives me the size by [columns rows]

c("The number of columns is:", ncol(mydata))  #Double checking the column and row size.
c("The number of rows is:", nrow(mydata))

names(mydata) #gets the variable names of the data frame.

Averageweight = mean(mydata$weight) #Define average weight to be the mean of the weight column in mydata.
Averageweight #Gives me an output

convertedHeight = mydata$height/100  #Changes cm to meters
BMI = (mydata$weight)/(convertedHeight)^2 #Formula for BMI

plot(BMI~mydata$weight, xlab="Weight (kg)",ylab="BMI ( kg/m^2)") #Plots the graph.



