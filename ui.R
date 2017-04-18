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
