######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Jan 12th, 2017
#
######################################################

# Lab 1 Exercise 1 - Sex ratio at birth of Red Deer

#ls() is a list of all variables
#rm() = remove
#rm(list=ls()) <-- make a list of these vectors and remove all of them.

rm(list = ls(all = TRUE))  # resets R to fresh

deer.id= c("deer1","deer2","deer3", "deer4","deer5","deer6")
females = c(3,2,1,5,4,3)
males = c(2,5,3,4,1,2)

#Calculate the sex ratio of children of the first deer

SexRatios1 = females[1]/(females[1]+males[1])

#Calculate the sex ratios of the children of all of the deer.

SexRatios = females/(females+males)

#Calculate the average of the sex ratios.

AverageofRatios = mean(SexRatios)
