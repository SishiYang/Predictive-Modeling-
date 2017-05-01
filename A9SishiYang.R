#Sishi Yang Assignment 9
#All the starting values and limits are explained in the word documents.
#Question 1
#Import dataset and view it
Corn <- read.csv("D:/R/Homework/Assignment9/Corn.csv")
View(Corn)
#Create a linear regression model
attach(Corn)
reg1 = lm(yield~nitrate)
reg1
#Coefficients:Intercept:6148.50   nitrate slope:51.52 
linear = 6148.50 + 51.52*nitrate
linear #Get the linear regression predictions as "linear"

#Create the likelihood function based on the fact error is normally distributed
LLgrowth = function(A,b,sigma)
{
  x = yield-A*(1-exp(1-b*nitrate))
  LLsum = sum(dnorm(x,mean = 0,sd = sigma,log = T))
  return(-1*LLsum)
}
#Use mle2 to optimize the result
sd(yield) #sd is 2171.241. It's the starting value for sigma
mean(yield)/(1-exp(1)) #The result is -4401.659, the starting value for A
res = mle2(minuslogl = LLgrowth,start = list(A=-4401.659,b=0,sigma=2171.241), method = "L-BFGS-B") 
summary(res)
#Obtain the coefficients
growth = (-4.4017e+03)*(1-exp(1-(-1.2934e-03)*nitrate))
#Get the predictions named as "growth"
Corn$growth = growth
#Save the data frame
write.csv(Corn,"D:/R/Homework/Assignment9/Corn.csv")
#Creat the function of measurement metrics
nummetrics = function(a,m)
{
  metrics = c(MAD=0,MSE=0,MAPE=0,MPSE=0,tMAD=0,P90=0,R2=0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs((a-m)/a))
  metrics["MPSE"] = mean(((a-m)/a)^2)
  metrics["tMAD"] = mean(abs(a-m),trim=0.05)
  metrics["P90"] = quantile(abs(a-m),probs = 0.9)
  SSE=sum((a-m)^2)
  SST=sum((a-mean(a))^2)
  metrics["R2"] = 1-(SSE/SST)
  return(metrics)
}
#Compare the two models using metrics
nummetrics(yield,linear)
nummetrics(yield,growth)
#Based on the result, the linear model is bettern tha  growth model.R2is larger and the other indicators are smaller.
detach(Corn)

#Question 2
#Import dataset and view it
Health <- read.csv("D:/R/Homework/Assignment9/Health.csv")
View(Health)
attach(Health)
#Create the likelihood function 
LLreg2 = function(a0,a1,a2,b0,b1,b2,b3)
{
  m=exp(a0+a1*numchron+a2*male)
  Pi=m/(1+m)
  lam=exp(b0+b1*numchron+b2*employed+b3*married)
  e=ifelse(ofp==0, Pi+(1-Pi)*dpois(0,lam), 
                   (1-Pi)*dpois(ofp,lam))  #Classify the two situations
  LLsum=sum(log(e))
  return(-1*LLsum)
}
#Use mle2 to optimize the result
res = mle2(minuslogl=LLreg2,start=list(a0=0,a1=0,a2=0,b0=0,b1=0,b2=0,b3=0)) 
summary(res)
detach(Health) 
#The result is a0=-1.1971337,a1=-0.5429613,a2=0.3734520,
#b0=1.6770421,b1=0.1495032,b2=0.0490204,b3=-0.0579778
#As b2 is starred only once in the result, employed is less important compared to numchron, married and male. 
  
#Question 3
#Import dataset and view it
spending <- read.csv("D:/R/Homework/Assignment9/spending.csv")
View(spending)
#Create the likelihood function based on the fact error is normally distributed
LLreg3 = function(l,a0,b1,sig1,sig2)
{
  x1 = spending$amount-a0
  x2 = spending$amount-a0-b1*(spending$age-l)
  LLsum = sum(ifelse(spending$age<=l,dnorm(x1,mean = 0,sd = sig1,log = T),
         dnorm(x2,mean = 0,sd = sig2,log = T)))
  return(-1*LLsum)
}
#Use mle2 to optimize the result
res = mle2(minuslogl = LLreg3,start = list(l=mean(spending$age),a0=mean(spending$amount),
                                           b1=0,sig1=sd(spending$amount),sig2=sd(spending$amount))) 
summary(res)  
#Obtain the coefficients and the result is as follows:
#As l is 45.20, there are two regression models with age less than 45.20 and larger than 45.20.
#standard deviation for the two models are 52.42 and 11.11 respectively.
#The intercept is 83.27 and the slope for the second model is 4.16.





