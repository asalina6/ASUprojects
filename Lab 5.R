######################################################
######################################################
######################################################
#
# Author: Armando Salinas
#         asalina6@asu.edu
# Created: Feb 14, 2017
#
######################################################

#####First set up########

# resets R to fresh
rm(list = ls(all = TRUE)) 
#set directory
setwd("C:/Users/Armando/Dropbox/GRAD SCHOOL/Grad School Year 1/BIO612/Lab 5")

####Lab 5 Exercise 1 - Binomial Dist review#######
p=.25
n=24

#ans1 a.	The probability that exactly 8 plants will have wrinkled peas.
dbinom(8,n,p)
#ans2 b.	The probability that 8 or fewer plants will have wrinkled peas.
pbinom(8,n,p)
#ans3 c.	The probability that 12 or more plants will have wrinkled peas.
1-pbinom(11,n,p)
#ans4 d.	The 0.025 quantile of the number of plants with wrinkled peas.
qbinom(.025,n,p)

###Lab 5 Exercise 2 #####
# resets R to fresh
rm(list = ls(all = TRUE)) 
n=25
x=15
#ans estimate probability p that offspring is female
p.hat=x/n
p.hat
p.prime = (x+2)/(n+4)
stderror=sqrt(p.prime*(1-p.prime)/(n+4))
###Agresti-Coull method 95%

String=cat("The 95% confidence interval of",p.hat," is: (", p.prime-2*stderror,",",p.prime+2*stderror,")")

#The interval contains p=.5, so it will not reject the upcoming null hypothesis.



#STEP 1
#H0: The probability of having a female is .5 (p=.5)
p=.5
#HA: the probability of having a female is not .5 (p=/=.5)
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is X, the observed number of females among the 25.
#STEP 4
#collect data and calculate test statistic
#This is where x=15, n=25
#STEP 5
#Calcuate the pvalue:
Test1=(1-pbinom(x-1,n,p))+(pbinom(n-x,n,p))
String2=cat("The p=value is:",Test1," and the alpha value is:",alpha)

if(Test1 < alpha)
{
  print("Reject Null")
}
if(Test1 > alpha)
{
  print("Do not reject Null")
}

Test2=binom.test(x,n,p=0.5)
Test2
###################
## Lab 5 Exercise 3
# resets R to fresh
rm(list = ls(all = TRUE)) 
x=2
n=18
#proportion that  participates choose dog food as favorite
p.hat=x/n
p.hat
#proportion that its randomly chosen
choices=5
randomchosen=1/choices
randomchosen

# We begin the hypothesis testing
#STEP 1: 
#H0: the probability dog food is picked as the favorite equals 0.20
#HA: the probability dog food is picked as the favorite is less than 0.20
#(People prefer human food over dog food)
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is X, the observed number of participates choosing dog food as their favorite.
#STEP 4
#collect data and calculate test statistic
#This is where x=2, n=18
#STEP 5
#Calcuate the pvalue:
test3=binom.test(x,n,p=.2,alternative="less")
test3
print("We do not reject the null hypothesis that the probability dog food is picked as the favorite equals .2, at a signifiance level of .05. Exact binomial test with a tail with p value of .2713 ")
#################################3
#Lab 5 Exercise 4
# resets R to fresh
rm(list = ls(all = TRUE)) 
n=24
x=11
p=.25

# We begin the hypothesis testing
#STEP 1: 
#H0: the probability that genes are found on the X chorosome of fly is 0.25
#HA: the probability that genes are found on the X chorosome of fly is greater than 0.25
#STEP 2
#Significance level is alpha = .05 
alpha=.05
#STEP 3
#The test stastic is X, the observed number of spermatogenesis genes are found on the X chromosome. 
#STEP 4
#collect data and calculate test statistic
#This is where x=11, n=24
#STEP 5
#Calcuate the pvalue:
test3=binom.test(x,n,p=.25, alternative="greater")
test3
print("we reject the null hypothesis that the the probability that genes are found on the X chorosome of fly is 0.25, at a signifiance level of .05. Exact binomial test with tail with p value of .02134 ")
#################################3

