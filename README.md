# R-Shiny-Project
Interactive visualization of Airbnb Data using R Shiny
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


#-------------------------------Author: Team Rocket---------------------------------------
server <- function(input, output) {
  
  #----------------------------Bubble Plot Code--------------------------------------------------
  # The bubble plot is created for the four different county type,
  # with respect to price and time change. The color of the bubbles 
  # is based on the number of host. The size of the bubble is decided by the number of host. 
  filter_county = reactive({                                               #Filter function
    bubblesplot %>%
      filter(
    COUNTY %in% input$LA_county                                            # County type is added as filter in the bubble chart, gengerate subset based on county selected
      ) 
  })
  
  output$bubbleplot <- renderGvis({                                        # Render Gvis will give output in the interactive chart format
    bubbleplot= gvisBubbleChart(filter_county(),                           # gvisbubblechart is used for the bubble chart format
                              idvar = "COUNTY",                            # select which column subject to be analysed       
                              xvar = "year",                               # variable to be plotted on x-axis
                              yvar = "Avg_Price",                          # variable to be plotted on y-axis
                              colorvar = "Number.of.host",                 # varaible to  identifies bubbles in the same series
                              sizevar = "Number.of.host",                          # bubble size will vary based on the variable  selected
                              options=list(
                                height = "400", width = "1000",            # height and width setting
                                hAxis="{title:'YEAR', format:'####'}",     # An object with members to configure various horizontal axis elements. 
                                #4 Astricks shows the result value is in 4 digits. It is important otherwise R will take in scientific notations
                                vAxis="{format:'short',title:'PRICE'}",    # An object with members to configure various vertical axis elements
                                bubble="{textStyle:{color: 'none'}}")      # configure the visual properties of the bubbles. It is used in here to remove the labels from the bubble
                                
                              )                                            #An object that specifies the bubble text style,{color: <string>, fontName: <string>, fontSize: <number>}
    
    return(bubbleplot)                                                     # Renders the graph back to the ui to be displayed
  })
#-------------------------------Code for the Average Price by County(Heat Map)---------------------------------------------
  output$Zipcounty = renderPlot({
    
    # subset raw data set
    data <- select(LA, zipcode, price)
    
    # preprepare data
    #	Remove missing values in our data sets;
    data <- na.omit(data)
    # Remove rows that contain zipcodes that are not five-digit number
    data <- data[ ! data$zipcode %in% c("10068",
                                        "90005-3747", 
                                        "90034-2203",
                                        "90036-2514", 
                                        "90035-4475",
                                        "90039-2715",
                                        "90065.3819",
                                        "90403-2638",
                                        "91001-2243",
                                        "91604-3646",
                                        "CA91765",
                                        "Heart of Hollywood") , ]
    # Transform zipcode to numeric data type
    data <- transform(data, zipcode = as.numeric(as.character((zipcode))))
    
    # convert zipcode to fips code
    # website: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
    # the data set "Zip_county" has the fips code with zipcode
    
    # merge two data sets by zipcode
    new <- join(data, zip_county, by = "zipcode", type = "left", match = "first")
    
    # delete column zipcode
    new <- new[,c(2,3)]
    
    # calculate the average price of each county
    group <- ddply(new, .(COUNTY), summarize,  price=mean(price))
    
    # remove null value
    group <- na.omit(group)
    
    # create a list of county
    fips <- unique(group$COUNTY, incomparables = FALSE)
    
    # rename the columns
    # The data set that we provide to county_choropleth next step must have 
    # one column named "region" and one column named "value".
    names(group) <- c("region", "value")
    
    # plot
    county_choropleth(group,
                      title       = "Average Price by County",
                      legend      = "Price",
                      num_colors  = 1,      # represents a continuous scale
                      county_zoom = fips)   # the map only shows the five counties we have
    })

  #-----------------------------------------Motion Chart Code------------------------------------------
  # Filter  for the motion chart 
  filter_motionchart = reactive({                                           #use reactive expression whose result will change over time
    summ %>%
      filter(
        Room  ==input$laroom                                                # filter data that match input room type
      )
  })
  
  #Takes room type base filtered data as input to give dynamic visualization
  #motion chart is a dynamic chart to explore several indicators over time, 
  #create bubbleplot, bar charts and line charts based on all  available variables,
  #it picks up all the data in  filtermotion() to creates all the possible combination of charts 
  
  
  output$motionchart <- renderGvis({                                        #This will give result in the interactive map format                   
    #ID VARIABLE AND TIME VARIABLE MUST BE DEFINED. Rest all variable are optional. It is also avaliable in the charts
    motionch= gvisMotionChart(filter_motionchart(),idvar="Room",timevar="Year",yvar = "Number.of.Host.Year.", xvar = "Price", sizevar = "Number.of.Host")   # give analysis based on "Room", with time dimension based on "Year" 
    return(motionch)                                                        # Renders the graph back to the ui to be displayed
  })
  
#-----------------------------------------GGplot2 + Animation------------------------------------------
#--------WHEN RUN THE CODE, PLEASE BE PATIENT, THE PLOTS WILL TAKE A WHILE TO SHOW-----------------------
    output$gifaverprice <- renderImage({                                    #Render image from googleviz package is used to get output of an Image
    {(                                                                      #Average price with respect room type plot is made with respect to a particular year
      for (i in seq_along(summ$Room))                                       #Year variable is run in a loop to get the various plots with repect to time
      {
        LA7 = summ[summ$Room==summ$Room[i],]                      
        #Dimesions of the plot are defined down here
        a = ggplot(LA7, aes(x = Year, y = Price, height = .25, width = .25)) + geom_bar(stat = "identity",aes(fill = Number.of.Host))+scale_y_continuous(limits = c(0, 300))+ggtitle(summ$Room[i])
        ggsave(a,filename=paste("AvgPrice",summ$Room[i],".png",sep=""))     #Plot is then save in a PNG image file and name is given as AVGPRICE[YEAR].
      })
      ###IMPORTANTS LINK----SEE PATH FILE WHERE LIBRARY IS DEFINED####
      ###THIS IS COMMAND WHICH IS WRITTEN IN THE PATH FILE WHERE ALL THE PLOTS 
      AVGPRICE_CONVERT = system('"C:/Program Files/ImageMagick-7.0.3-Q16/magick" -delay 400 AvgPrice*.png AvgPrice.gif')
      AVGPRICE_CONVERT
      ###ARE SAVE AND THIS WILL CONVERT INTO GIF IMAGE           
    } ###IMPORTANTS LINK----SEE PATH FILE WHERE LIBRARY IS DEFINED####
    list(src = Price_Report,                                           #READ THE FILE WHERE GIF FILE IS SAVED AFTER GETTING CONVERT BY MAGICK COMMAND
         width=600,                                                    #Setting the dimension of the output gif file
         height=500 )
  }, deleteFile = FALSE)                                               #Choosing not to delete the image file
  
  output$gifnumber <- renderImage({                                    #Render image from googleviz package is used to get output of an Image
    {(                                                                 #Average price with respect room type plot is made with respect to a particular year
      for (i in seq_along(summ$Year))                                  #Year variable is run in a loop to get the various plots with repect to time
      {
        LA7 = summ[summ$Year==summ$Year[i],]
        #Dimesions of the plot are defined down here
        b = ggplot(LA7, aes(x = Room, y = Number.of.Host, height = .25, width = .25)) + geom_bar(stat = "identity",aes(fill = Room)) + scale_y_continuous(limits = c(0, 16000))+ggtitle(summ$Year[i])
        ggsave(b,filename=paste("Hosts",summ$Year[i],".png",sep=""))   #Plot is then save in a PNG image file and name is given as HOSTS[YEAR].
      })
      ###IMPORTANTS LINK----SEE PATH FILE WHERE LIBRARY IS DEFINED#### 
      ###THIS IS COMMAND WHICH IS WRITTEN IN THE PATH FILE WHERE ALL THE PLOTS 
      HOSTS_CONVERT = system('"C:/Program Files/ImageMagick-7.0.3-Q16/magick" -delay 200 Hosts*.png Hosts.gif')
      HOSTS_CONVERT     
      ###ARE SAVE AND THIS WILL CONVERT INTO GIF IMAGE
      ###IMPORTANTS LINK----SEE PATH FILE WHERE LIBRARY IS DEFINED#### 
    }
    list(src = Hosts_Report,                                          #READ THE FILE WHERE GIF FILE IS SAVED AFTER GETTING CONVERT BY MAGICK COMMAND
         contentType = 'image/gif',                                   #Setting the dimension of the output gif file
         width=600, 
         height=500
    )
  }, deleteFile = FALSE)                                              #Choosing not to delete the image file

#------------------------------------Googlevis map-----------------------------------------------------
#--------WHEN RUN THE CODE, PLEASE BE PATIENT, THE MAP WILL TAKE A WHILE TO SHOW-----------------------

# We created an interactive google map that shows all the airbnb houses located in LA. 
# Each point plotted by the latitide and longitude in the dataset.
# If click each point, users can see the information of each house on the tooltip. (interactive) 
# This tooltip was created in the Airbnb_LA code, added as a new column "names". 
# To show only the information that users want to see, we put filter for price, room type, and accommodates.
# With price filter, users can pick the range of price, with room type filter, users can choose 
# what kind of room type they want to stay. Here, we made it possible to pick multiple choices 
# With the accommodates filter(name as guests), users can pick how many guests are staying. 
# This is from the column "accommodates" in our dataset, which shows how many guests that each place can accommodate.


filteredgoogle=reactive({                         
  bnb %>%                                               # With the filters that we put, the outcome will show only the filtered ones. on the map
    filter( 
       p3>=input$price[1],                              # Price filter with the slider that users can change the range of the price.
       p3<=input$price[2], 
        room_type %in% input$buttons,                   #filter data based on room type selected, so users can pick what kinds of room type they want.
        accommodates==input$guests                      #filter data based on guests number selected, which means number of guests that can stay in each place.
    )                       
}) 

output$Map=renderGvis({                                 # Create the map with output function of shiny
  
  sites=gvisMap(filteredgoogle(),"latlong", "names",    # Gives google map with the filtered information, "latlong" is the latitude and longitude, and "names" is the tooltip.
                options=list(showTip=TRUE,              # Shows the tooltip "names" 
                             showLine=TRUE,             # Shows a Google Maps polyline through all the points.
                             enableScrollWheel=TRUE,    # Enable map to be zoomed in or out
                             mapType='normal',          # Map type 
                             useMapTypeControl=TRUE,width=600, height=400))   #size of map
  return(sites)
})


#---------------------------------Code for the display of GIF----------------------------------------------------------
output$Picture <- renderImage({             #THis is used to show a GIF for different pictures in the house
  list(src = LA_PIC,                        #THis read the GIF file
       width=550,                           #Set the dimensions
       height=400 )
}, deleteFile = FALSE) 

output$Picture2 <- renderImage({            #THis is used to show a GIF for different pictures in the house
  list(src = LA_PIC1,                       #THis read the GIF file
       width=550,                           #Set the dimensions
       height=400 )
}, deleteFile = FALSE)
}
#---------------------------------The END -------------------------------------------------------------


#-------------------------------Author: Team Rocket---------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Rocket dashboard"),    #comments are on the right side                # set dashboard header, input title "Rocket"
  dashboardSidebar(                                                                              # set dashboard sidebar
    sidebarMenu(                                                                                 # every menu item means a tab in sidebar
      menuItem("googleVis - Bubble Plot",tabName = "BubblePlot", icon = icon("spinner")),        # build menu,name it as "googleVis - Bubble Chart", use name "Bubble Chart" to link it to certain tableitem in below
      menuItem("googleVis - Motion Plot",tabName = "MotionPlot",icon = icon("area-chart")),      # build menu,name it as "googleVis - Motion Chart", use name "Motion Chart" to link it to certain tableitem in below
      menuItem("Imager-animation",       tabName = "imager",    icon = icon("tv")),              # build menu,name it as "Imager-animation", use name "imager" to link it to certain tableitem in below
      menuItem("Imager-animation",       tabName = "imager2",    icon = icon("tv")),             # build menu,name it as "Imager-animation", use name "imager2" to link it to certain tableitem in below
      menuItem("googleVis - Map",tabName = "Map",  icon = icon("map-marker"))                    # build menu,name it as "googleVis - Map", use name "Map" to link it to certain tableitem in below
    )                                                                                            # more ways of customizing icons can be found here. https://rstudio.github.io/shinydashboard/appearance.html#icons
  ),
  
dashboardBody(                                                                                   # fill dashboard body
tabItems(                                                                                        # set table items to put subsidiary table items in it
  # ------------------------------googlevis Bubble Plot------------------------------------------- 
  # produce a plot to show price and number of host  change with time variation 
  tabItem(tabName = "BubblePlot",                                                                # use name "BubblePlot" to connect tabitem with menu item set above
          fluidRow(box(                                                                          # set column width
                              title = "Select County", background = "teal",width = 4,         # set color title width and background color
                              checkboxGroupInput(inputId = "LA_county",label = "Options Avaliable",selected = "Long Beach", 
                                                 choices = list("Los Angeles"="Los Angeles","Orange"="Orange","Santo Ana"="Santo Ana","Long Beach" = "Long Beach"))
                              # InputID is to be interactive with the server script (specifically on filter in this case)
          ), box(offset = 1, plotOutput("Zipcounty", width = 400, height = 250))
          ),
          fluidRow(                                                                               #fluid Rows exist for the purpose of making sure their elements appear on the same line
            box(title = "Price vs Years in the selected county",
                solidHeader = TRUE, width = 12,background = "olive",htmlOutput("bubbleplot"))     # #set paramater for box, and define which output("bubbleplot") build in server file to show in this box
          )),
  
  
  #-------------------------------googlevis Motion plot------------------------------------------
  # This tab is for the motion chart showing interactive plots. 
  tabItem(tabName = "MotionPlot",                                                                     # use name "Motionplot" to connect tabitem with menu item set above
          fluidRow(                                                                 
            box(title = "Apartment type based visual (Motion Chart)",                                 # use box fuction to hold output in the main body
                background = "teal",  width = 8,solidHeader = TRUE,                                   # ser paramater value for box function
                htmlOutput("motionchart",height = 600)),                                              # tell ui to show  which output("motionchart") in this box
            column(4, box(                                                                            # set column width
              title = "Room types", background = "aqua", width = 12,                                  # set color title width and background color
              #input different room type and name this input as "laroom"
              checkboxGroupInput(inputId = "laroom",label = "Room Type",selected = "Shared Room", choices = list("Entire Home"="Entire Home","Private Room"="Private Room","Shared Room"="Shared Room"))
            )          
            )
          )
  ),
#------------------------------ggplot2 + animation -------------------------------
  tabItem(tabName = "imager",                                                                    # use name "imager" to connect tabitem with menu item set above
          fluidPage(                                                                             # use fluidpage to put box in
            # box in shinydashboard places visualization1 at a particular place in the output 
            box(title = "Average Price over the Years",solidHeader = TRUE,background = "teal",   # box in shinydashboard places visualization result at a particular place in the output 
                width = 12, height = 600, plotOutput("gifaverprice")))),                         # set width and height, use plotoutput for show gif, output name is the one defined in server file
  tabItem(tabName = "imager2",                                                                   # use"imager2" to connect tabitem with menu item set above
          fluidPage(                                                                             # use fluidpage to put box in
            box(title = "Number of Host over the Years",solidHeader = TRUE,background = "aqua",  # box in shinydashboard places visualization result at a particular place in the output 
                width = 12, height = 600, plotOutput("gifnumber")))),                            # set width and height, select plotoutput for gif, output name is the one defined in server file



#--------------------------------googlevis map-----------------------------------------------------
      tabItem(tabName = "Map",                                                                                # use"Map" to connect tabitem with menu item set above
              fluidPage(
        titlePanel("Los Angeles"),                                                                            # give a name to the title of panel
        sidebarLayout(
          sidebarPanel (                                                                                      # Create a sidebar panel to make inputs that user can make changes
            sliderInput(inputId = "price",label = "Price",                                                    # InputID is to be interactive with the server script (specifically on filter in this case)
                        min=0,max=500,animate=T,step = 5,value=c(20,370)),                                    # define the range of price and name this inputID as "price", activate animation and set step as 5
            checkboxGroupInput(inputId = "buttons",label = "Room Type",selected = "Private room", choices = list("Entire home/apt"="Entire home/apt","Private room"="Private room","Shared room"="Shared room")),
            #input different room type  and name this input as "buttons"
           
             selectInput(inputId = "guests",label="guests",choices = c(1:16))                                 #define  the range of guest number and name this input as "guests"
          ), mainPanel (htmlOutput("Map"))                                                                    # This is the google map we created with Airbnb houses plotted
        )
      ), 
      
      fluidRow(                                                                                              #This will show the GIF from the server file
        box(                                                                                                 #This will show the GIF from the server file
          plotOutput("Picture")),
        box(                                                                                                 #This will show the GIF from the server file
          plotOutput("Picture2")))                                                                
      )
    )
  )
)
#-------------------------------The End ---------------------------------------------------
