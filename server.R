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
