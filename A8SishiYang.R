#Sishi Yang (2389198) Assignment 8 

#Question 1
#First compute time of all the 4 tasks
t1 = runif(1000, min = 5, max = 9)
t2 = rexp(1000, rate = 0.1)
t3 = rpois(1000, lambda=4)
t4 = runif(1000, min = 3, max = 10)
total = ifelse(t1+t2 > t3+t4, t1+t2, t3+t4)  #Return the greatest amout of time using ifelse
#Question 1.a 
meantime = mean(total)
meantime  
#The mean is 17.7701
mediantime = median(total)
mediantime  
#The median is 14.50789

#Question 1.b
#First get the standard devation 
sd = sd(total)
sd  
#The standard devation is 9.456737
#Calculate the probability
pnorm(12, mean=meantime, sd=sd)
#The probability is 0.2708788 to finish all tasks in 12 hours.

#Question 1.c: Create a plot of the density of the total completion time
plot(density(total))


#Question 2
#First calculate the profits of each stock
A = 30*95*rnorm(1000, mean = 0.1, sd = 0.05)
B = 15*100*runif(1000, min = (-0.1), max = 0.15)
C = 25*25*runif(1000, min = (-0.15), max = 0.25)
D = 50*50*rnorm(1000, mean = 0.2, sd = 0.3)
#Question 2.a Sum them up to get the total profits
totalp = A+B+C+D
expected_profit = mean(totalp)
#The total expected profit is 885.99

#Question 2.b
medianp = median(totalp)
medianp
#The median is 796.9459
sdp = sd(totalp)
sdp
#The standard devation is 754.5121

#Question 2.c
p1 = 1 - pnorm(100, mean=mean(totalp), sd=sdp)
p1
#The probability that you don¡¯t lose money is 0.8317835
p2 = 1- pnorm(1500, mean=mean(totalp), sd=sdp)
p2
#The probability you will make at least $1500 profit is 0.1855896


#Question 3
#Open and read the file
Health <- read.csv("D:/R/Homework/Assignment 8/Health.csv")
View(Health)
#The null hypothesis is the mean number of office visits is 5.5.
#Create the test statistic for the sample
tstat = mean(Health$ofp)
tstat
#The result is 5.774399
#The following is to create synthetic samples to get mean
f1 =function()
{
  x = mean(rpois(nrow(Health), lambda=5.5))
  return (x)
}
sdist = replicate(10000,f1())   #Repeat the function 10000 times.
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat) 
abline(v=mean(sdist)-gap) 
abline(v=mean(sdist)+gap) 
#Get the p value
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist) 
pvalue
#The p value is 0, less than 0.05, so we reject the hypothesis that mean number is 5.5.


#Question 4
#Open and read the file
Walmart <- read.csv("D:/R/Homework/Assignment 8/Walmart.csv")
View(Walmart)
#Get the store 1 and non-holiday sales dataset named m
sales = Walmart[Walmart$Store == 1 & Walmart$IsHoliday == FALSE,]
m = sales$Weekly_Sales
m

#Question 4.a Assuming normality 
#Mean
#Create 1000 samples from the normal distribution and compute the mean for each sample.
meansales1 = replicate(1000, mean(rnorm(length(m), mean(m), sd(m)))) #The number should use length function
#create the 95% confidence interval
l_side = quantile(meansales1,probs=0.025)
l_side
r_side = quantile(meansales1,probs=1-0.025)
r_side
#The result is  2.5%: 21067.88   97.5%: 22168.86

#Median
#Create 1000 samples from the normal distribution and compute the median for each sample.
mediansales1 = replicate(1000, median(rnorm(length(m), mean(m), sd(m))))
#create the 95% confidence interval
l_side = quantile(mediansales1,probs=0.025)
l_side
r_side = quantile(mediansales1,probs=1-0.025)
r_side
#The result is 2.5%: 20926.14   97.5%: 22278.56

#Standard deviation
#Create 1000 samples from the normal distribution and compute the sd for each sample.
sdsales1 = replicate(1000, sd(rnorm(length(m), mean(m), sd(m))))
#create the 95% confidence interval
l_side = quantile(sdsales1,probs=0.025)
l_side
r_side = quantile(sdsales1,probs=1-0.025)
r_side
#The result is 2.5%: 27189.66   97.5%: 27977.23

#Question 4.b Using bootstrapping
#Mean
#Create 1000 samples using bootstrapping and compute the mean for each sample.
meansales2 = replicate(1000,mean(sample(m,length(m),replace = T))) 
#create the 95% confidence interval
l_side = quantile(meansales2,probs=0.025)
l_side
r_side = quantile(meansales2,probs=1-0.025)
r_side
#The result is 2.5%: 21115.46   97.5%: 22147.59

#Median
#Create 1000 samples using bootstrapping and compute the median for each sample.
mediansales2 = replicate(1000,median(sample(m,length(m),replace = T)))
#create the 95% confidence interval
l_side = quantile(mediansales2,probs=0.025)
l_side
r_side = quantile(mediansales2,probs=1-0.025)
r_side
#The result is 2.5%: 9919.323    97.5%: 10532.6

#Standard deviation
#Create 1000 samples using bootstrapping and compute the sd for each sample.
sdsales2 =replicate(1000,sd(sample(m,length(m),replace = T)))
#create the 95% confidence interval
l_side = quantile(sdsales2,probs=0.025)
l_side
r_side = quantile(sdsales2,probs=1-0.025)
r_side
#The result is 2.5%: 26855.06    97.5%: 28320.33


#Question 5
#The null hypothesis is that there is no difference between the two groups.
#Create the two datasets
under_sixty = c(75, 77, 80, 69, 73, 76, 78, 74, 75, 81, 75, 80, 79, 80)
above_sixty = c(68, 74, 77, 71, 73, 75, 80, 77, 78, 72, 69, 71, 75, 78)
#Create the test statistic for the sample
tstat = abs(mean(under_sixty)-mean(above_sixty)) 
tstat #The result is 2.428571
n = length(under_sixty)

#Create the sampling distribution assuming the hypothesis is true
f2 = function()
{
  x = sample(c(under_sixty,above_sixty)) 
  m1 = mean(x[1:n]) 
  m2 = mean(x[(n+1):length(x)]) 
  abs(m1-m2) 
  return(abs(m1-m2))
}
sdist = replicate(10000,f2())  #Repeat the function 10000 times.
gap = abs(mean(sdist)-tstat)
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist) 
pvalue
#The p value is 0.0673, larger than 0.05, so we cannot reject the hypothesis.
#There is no difference between the two groups. Thus, the restaurant is not discriminating against the elderly.

#Question 6
View (Health)
#The null hypothesis is that the 75% percentile value of age is 7.7.
#Create the test statistic for the sample
tstat = sum(ifelse(Health$age>7.7,1,0)) 
#Create the sampling distribution assuming the hypothesis is true
f3 = function() 
{
  v = c(1,0) 
  p = c(0.25,0.75) 
  x = sample(x = v,replace = T,prob = p,size = length(Health$age)) 
  return(sum(x)) 
}
sdist = replicate(10000,f3())   #Repeat the function 10000 times.
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat) 
abline(v=mean(sdist)-gap) 
abline(v=mean(sdist)+gap) 
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist) 
pvalue
#The p value is 0.0263, less than 0.05, so we reject the hypothesis.





