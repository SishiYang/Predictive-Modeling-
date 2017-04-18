# R-Shiny-Project
Interactive visualization of Airbnb Data using R Shiny
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
