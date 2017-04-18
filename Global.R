#--------------------------------------------Author: Team Rocket------------------------------------------
#----------------------------------------------packages needed--------------------------------------------

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("googleVis")
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(choroplethr)
library(choroplethrMaps)

############################################################
####Make sure working directory refers to the data files####
############################################################

getwd()
setwd("C:/users/test/desktop/teamrocket/DATA")

############################################################
#####Make Sure to define proper path########################
############################################################
#-----------------------------------------------load data------------------------------------------------
bnb = read.csv("LA.csv")        #load the csv file  Los Angeles Airbnb data, name it as bnb
summ = read.csv("LA5.csv")      #load the csv file  summary Los angeles Airbnb data, name it as summ
LA <- read.csv("LA1.csv")       #load the csv file  the zip code associate with the listings, name it as LA
bubblesplot <- read.csv("LA2.csv")  #load the csv file containing the county summary information
zip_county <- read.csv("zip_county.csv") #load the csv file, zip county information

#-----------------------------------setting for Animation-----------------------------------------------
HOSTS_CONVERT = system('"C:/Program Files/ImageMagick-7.0.3-Q16/magick" -delay 200 Hosts*.png Hosts.gif')
#This command convert the files into other format as required. This team used to convert png into gif
###MAKE SURE MAGICK FILE IS INSTALLED IN THE SYSTEM AND CONTAINS AT THE SAME LOCATION
######MAGICK IS FREE TOOL WHICH IS SUPPORTED BY R TO CONVERT THE IMAGE FILES

AVGPRICE_CONVERT = system('"C:/Program Files/ImageMagick-7.0.3-Q16/magick" -delay 200 AvgPrice*.png AvgPrice.gif')
#This command convert the files into other format as required. This team used to convert png into gif
###MAKE SURE MAGICK FILE IS INSTALLED IN THE SYSTEM AND CONTAINS AT THE SAME LOCATION
######MAGICK IS FREE TOOL WHICH IS SUPPORTED BY R TO CONVERT THE IMAGE FILES

Price_Report = normalizePath(file.path(paste('AvgPrice',  '.gif', sep='')))
##THIS IS THE PATH WHERE GIF FILE IS STORED AFTER CONVERTED FROM PNG. MAKE SURE PATH IS THE WORKING DIRECTORY 

Hosts_Report = normalizePath(file.path(paste('Hosts', '.gif', sep='')))
##THIS IS THE PATH WHERE GIF FILE IS STORED AFTER CONVERTED FROM PNG. MAKE SURE PATH IS THE WORKING DIRECTORY 

LA_PIC = normalizePath(file.path(paste('Photo', '.gif', sep='')))
###THIS IS THE SAMPLE GIF JUST TO SHOW. WE CAN ADD MANY MORE DEPENDING UPON REQUIREMENT

LA_PIC1 = normalizePath(file.path(paste('Photo2', '.gif', sep='')))
###THIS IS THE SAMPLE GIF JUST TO SHOW. WE CAN ADD MANY MORE DEPENDING UPON REQUIREMENT


#--------------------------------setting for Googlevis Map---------------------------------------------------------------
latlong= paste(bnb$latitude, bnb$longitude, sep = ":")  #put latitude and longtitude into one column as format latitude:longitude
p1=gsub( ",", "", bnb$price)                            #change the format of price #first, replace ',' with ' ' 
p2=as.factor(p1)                                        #read the p1 as factor values
p3=as.numeric(sub('\\$','',as.character(p2)))           #change the factor value into numeric values
avai=round(100*((bnb$availability_365)/365),2)          #calculation of availability_365 column to see the rate of availability for each house in a year.
a1<-ifelse(avai>85,"High","Low")                        #create a new column to show if availability is over 85%, put "high," else, put "low"


bnb =cbind(bnb,latlong,a1,avai,p3)                     # Add latlong, a1, avai, and p3 into the bnb dataset

#create a new column "names" to put the information as a tooltip of the google map we are showing. This includes host name, house name, price, rate of availability per year, number of days that are available in a year.
names=paste("<b><font size='2'>Host:</font></b>",bnb$host_name,"<br>","<b><font size='2'>Name:</font></b>",bnb$name,"<br>","<b><font size='2'>Price:</font></b>",bnb$price,"<br>",bnb$a1,"Availability","<br>",paste(bnb$availability_365,"days/year","(",bnb$avai,"%)"))
bnb =cbind(bnb,names)                #put everything together