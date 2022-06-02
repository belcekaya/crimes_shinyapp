
### installing d3heatmap and rcdimple packages
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("rstudio/d3heatmap")
#devtools::install_github("timelyportfolio/rcdimple")

# load the libraries
library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(leaflet)
library(gridExtra)
library(rgdal) # read json file
library(classInt)
library(RColorBrewer)
library(shiny)
library(tidyr)
library(highcharter)
library(dygraphs)
library(xts)
library(htmlwidgets)
library(ggplot2)
library(d3heatmap)
library(htmltools)
library(rcdimple)
library(DT)
library(waffle)
library(geojsonio)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)

## creating sub dataframes to visualize 
# main data
crimes <- read_csv("crime_data.csv")
# Convert to Date
crimes$date =  as.Date(crimes$date, format = "%Y-%m-%d")

## creating two forces dataframes for filtering their borough and categories
crimes_l <- filter(crimes, force=="City of London Police")
crimes_m <- filter(crimes, force=="Greater Manchester Police")

col_names <- c('borough','category')
crimes_l[,col_names] <- lapply(crimes_l[,col_names] , factor)
crimes_m[,col_names] <- lapply(crimes_m[,col_names] , factor)

# map data - top20 crimes
rpt_locs <- crimes %>% 
  group_by(force, long, lat, location, borough, category) %>%
  summarise(n = n()) %>%
  group_by(borough) %>%
  arrange(desc(n))%>% 
  slice(1:20)


# map data 2 - showing only Manchester boroughs
# Load the borough boundary vector layer  of Manchester only 
lsoa <- geojson_read("manchester_lsoa.geojson", what="sp")

# Load the crime data
map_df <- crimes %>% 
  filter(borough == "Manchester") %>% 
  group_by(category, lsoa, borough) %>%
  summarise(n = n()) %>% 
  rename(LSOA11CD = lsoa) %>% 
  as.data.frame()

map_df$category <- as.factor(map_df$category)

# Plot1 - Frequency of Violence by borough (in descending order)
df <- crimes %>% 
  filter(force == "Greater Manchester Police") %>% 
  group_by(borough, category) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

df$category <- as.factor(df$category)

# Plot2 - Frequency of crime by borough (in descending order)
df2 <- crimes %>% 
  filter(grepl('Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan', borough)) %>%
  filter(date == "2019-06-01" & category == "Violence and sexual offences" ) %>%  #date == "2019-06-01" &
  count(borough, sort = TRUE) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  select(borough, percent) %>% 
  spread(borough, percent)

# Plot3 - Bar chart
df_bar_L <- crimes %>% 
  filter(force=="City of London Police") %>%
  group_by(borough, category) %>%
  summarize(n = n())


#df_bar_L <- filter(df_bar, force=="City of London Police")
df_bar_L$borough <- as.factor(df_bar_L$borough)
 
# plot 4 dot chart - top 3 boroughs of one category
df_dot_L <- crimes %>% 
  filter(force=="City of London Police") %>%
  filter(borough != "City of London") %>%
  group_by(borough, category) %>%
  summarize(n = n())


# Plot5 - pie chart
df_pie_L <- crimes %>% 
  filter(borough=="City of London") %>%
  group_by(date, category) %>%
  summarize(n = n())


# Plot5 - Time series data
df_ts <- crimes %>% 
  filter(force=="City of London Police") %>%
  group_by(date,force, category) %>%
  summarise(n = n())

df_ts$category <- as.factor(df_ts$category)
###
df_ts_m <- crimes %>% 
  filter(force=="Greater Manchester Police") %>%
  group_by(date, category) %>%
  summarise(n = n())


################################ USER INTERFACE #############################

ui <- fluidPage(
  title = "Crime Data Analysis",
  #theme = shinytheme("united"),
  theme = "styles.css",
  
  
  titlePanel(div(strong("What are the most common", em("criminal record categories"), "of the two biggest urban areas of the United Kingdom?"), 
                 style = "margin: auto; width: 90%; text-align:justify; color:black; 
                   padding:15px; font-size:150%")
  ),
  
  fluidRow(p(em("Developed by"), strong(em("Belce Kaya")), em("and"),strong(em("Xingyi Li"))),style="text-align:center; font-family: times"),
  fluidRow(
    style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
    
    p("London is by far the largest urban agglomeration in the United Kingdom, 
      with an estimated population of 9.3 million people, more than three times as large as Manchester, which is
      the UK's second biggest urban agglomeration."),
    
    p("The crime rate in the United Kingdom was 79.52 per 1,000 people in 2021-2022. 
         The overall UK crime rate saw a 1.18% increase from 2020. Crime in Greater London urban area is the highest, moreover, crime in Greater Manchester has the second-highest incidence in England 
         and Wales after Greater London."),
    p("In this shiny app, we have tidied and merged the data from",a(strong(em("data.police.uk")),href="https://data.police.uk/"),"database and create the dataset 
         which contains 118,466 observations for City of London and Greater Manchester police forces. The data contains  
         date, location of the crime, borough, lsoa, category of the crime, longitude and latitude information.
         The crime records of City of London police force are from April, 2019 to March, 2022 and the records of
          Greater Manchester police force are from April, 2019 to June, 2019."),
    p("To see the crime analyses of the Greater London and Greater Manchester, please click on the corresponding tabs just below!")
    
  ),
  
  hr(style = "width: 97%"),
  
  navbarPage("UK Crime Analysis", id="nav",
             
      tabPanel("Greater London",
                      
            ## Introduction
            fluidRow(
              style="margin: auto; width: 90%; text-align:justify; color:white; background-color: #007BA7; padding:15px;
              border-radius:10px; font-size:130%",
                        
                p("The boroughs of the Greater London in the dataset are Camden, City of London, Islington, Southwark, Tower Hamlets, 
                Waltham Forest, Westminster, Hackney, Derbyshire Dales, Epsom and Ewell, Lambeth, Newham, Enfield,
                Haringey, Kensington and Chelsea, Tandridge. The category of crimes are Anti-social behaviour, Bicycle theft, Other theft,
                Shoplifting, Vehicle crime, Violence and sexual offences, Burglary, Public order, Theft from the person, 
                Criminal damage and arson, Drugs, Robbery, Other crime, Possession of weapons.")
                ),
                      
                br(),
                      
              # text - explanation of the maps
              fluidRow(
                style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
                        
                        h1("The most frequent crime rate locations of the boroughs of the Greater London"),
                        
                        
                        br(),
                        
                        p("In this interactive map, you can see the most frequent crime rate areas by selecting one borough of Greater London!
        In each borough, we have the exact locations that crime has been occurred. In this map, you can see the 
        top 20 highest frequent locations, which represent the most frequent crime rate areas. For some boroughs, there are less than 20 
        locations. The areas with red circle marks sorted by their frequency level."),
                        
                        p("IMPORTANT!! Please click on the red circles to see the detailed pop-up information about the frequency, location 
        and the category of the crime!")
                        
                      ),
                      
                      
              fluidRow(
                        style = "margin: auto; width: 85%",
                        
                        absolutePanel(
                          draggable = T, 
                          style="z-index:500; margin-left:30px; margin-top:200px; font-size:130%",
                          
                          selectInput("borough", "Select a borough:",
                                      choices = levels(crimes_l$borough),
                                      selected = "City of London"),
                          
                          actionButton("reset", "Reset zoom")),
                        leafletOutput("freq_map_L",height=600)
                        ),
                      
            br(),
                      
            # text - explanation of the maps
            fluidRow(
              style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
              
              h1("Analysis of Crime Data"),
              
              br(),
              
              p("To analyze the crime data more detail, you can click on either", strong("By Borough"), strong("By Category"),"or", strong("London Crimes"),"parts.
                In the By Borough part, you can visualize the most frequent crime types of the selected Borough in Greater London in a bar chart.
                In the By Category part, there are two visuals. First one is a bubble chart that shows the number of crimes occured in the selected category
                for the corresponding borough. When the number of crimes increases, bubbles go to the right side more. It is important to notice that
                London is the biggest district in the Greater London urban area, and in every category, as we have seen in the By Borough part that 
                London has the highest criminal records and has a very high criminal rate. Therefore, to see more clear the records of the other boroughs 
                except London, we are visualizing the Boroughs except the City of London. For detailed information about the criminal records of the City of London by date and category, 
                you can go to the", em("London Crimes"), "part."),
              
              p("Moreover, the second visual of the By Category part represents the time series of the number of criminal records in the Greater London, including all the boroughs.
                In the London Crimes part, you can choose a month and year between April 2019 to March 2022, and you can visualize the proportion of each crime type in the City
                of London in the selected month of the year!")
              
            ),
            
          
           # Panels with barplot and dotplot    
           div(
                  tabsetPanel(
                          
                        tabPanel(div("Crimes by Borough", style="color: black"),
                                   br(),
                                   
                            fluidRow(
                                     
                                column( width = 2, offset = 1,
                                             
                                      fixedRow(selectInput("borough2", "Select a borough:",
                                                           choices = levels(df_bar_L$borough), 
                                                           selected = "City of London")
                                             )),
                                     column(7, offset = 1,
                                            br(),
                                            highchartOutput("plot3",height = "500px"))
                            )),
                                   
                        tabPanel(div("Crimes by Category", style="color: black"),
                                            br(),
                                        fluidRow(
                                              
                                              column( width = 2, offset = 1,
                                                
                                                fixedRow(
                                                        
                                                  selectInput("category2", "Select a category:",
                                                                    choices = levels(df_ts$category),
                                                                    selected = "Robbery")
                                                  )),
                                              
                                              column(7,offset = 1,
                                                       fluidRow(
                                                         dimpleOutput('plot4',height = "250px")
                                                         ),
                                                       br(),
                                                       fluidRow(
                                                         div(h4(textOutput("title"), align = "center"), style = "color:black"),
                                                                br(),
                                                                dygraphOutput('plot5',height = "250px")
                                                               )
                                      
                                                      )
                                   
                                          )),
                        
                       tabPanel(div("London Crimes", style="color: black"),
                                      
                                br(),
                                fluidRow(
                                   
                                    column( width = 2, offset = 1,
                                           
                                          fixedRow(
                                            airDatepickerInput("Date",
                                                               label = "Choose a month and year:",
                                                               value = "2022-03-01",
                                                               maxDate = "2022-03-01",
                                                               minDate = "2019-04-01",
                                                               view = "months", #editing what the popup calendar shows when it opens
                                                               minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                                                               dateFormat = "yyyy-mm")
                                                
                                          )),
                                      
                                        column(7, offset = 1,
                                               br(),
                                               plotlyOutput("pie_london",height = "500px"))
                                            ))
                                  ),
                  
                          style = "margin: auto; width: 90%; font-size: 120%"
                    ),
           
           br(), br()
  
                                   
                  ), 
                          
                        
             tabPanel("Greater Manchester", 
                      
                      ## Introduction
                      fluidRow(
                        style="margin: auto; width: 90%; text-align:justify; color:white; background-color: #007BA7; padding:15px;
              border-radius:10px; font-size:130%",
                        
                        
                        p("The boroughs of Manchester in the dataset are Blackburn with Darwen, Bolton, Bury, Cheshire East,
          Manchester, Oldham, Rochdale, Salford,St. Helens, Stockport, Tameside, Trafford, Warrington, Wigan,
          Chorley, Rossendale, and West Lancashire. The category of crimes are Burglary, Criminal damage and arson, Public order, 
         Vehicle crime, Violence and sexual offences, Possession of weapons, Drugs, Bicycle theft, shoplifting, Robbery, 
         Theft form the person, other crime, other theft.")
                      ),
                      
                      # text - explanation of the maps
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
                        
                        h1("The most frequent crime rate locations of the boroughs of Great Manchester"),
                        
                        br(),
                        
                        p("In this part, we are going to visualize the crime data with interactive maps.
        In the first leaflet map, you can see the most frequent crime rate areas by selecting the borough in the Greater Manchester.
        The top 20 number of crime records locations are placed on the map. For example, we can see exactly 20 data points, 
        which represent the most frequent crime rate areas on the map of Manchester borough! 
        For some boroughs, as the number of crime records are less than 20,  we are seeing less data points such as Cheshire East. 
        Also, we are visualizing the areas with green circle marks sorted by their frequency level."),
                        
                        p("IMPORTANT!! Please click on the green circles to see the detailed information about the frequency, location 
        and the category of the crime!")
                        
                      ),
                      
                      fluidRow(
                        style = "margin: auto; width: 85%",
                        
                        absolutePanel(
                          draggable = T, 
                          style="z-index:500; margin-left:30px; margin-top:200px; font-size:130%",
                          
                          selectInput("borough_m", "Select a borough:",
                                      choices = levels(crimes_m$borough),
                                      selected = "Manchester"),
                          
                          actionButton("reset", "Reset zoom")),
                        leafletOutput("freq_map_M",height=600)
                        ),
                      
                      br(),
                      
                      
                      # text - explanation of the map
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
                        
                        p("In the second map below, we are only visualizing the crimes of Manchaster borough in May 2019. 
                        In this interactive map, we can see the selected crime category's rate per 1000 population! 
                        The crime rate is sorted and we can see on the legend that when it gets darker green, the crime rate for 
                          the population increases."),
                        
                        p("IMPORTANT!! The map takes some time to fully load. Thanks for your patience!! 
        Again, do not forget to click on the area for more detailed information!")
                        
                      ),
                      
                      br(),
                      
                      ## SECOND MAP UI ## 
                      
                      fluidRow(
                        column(7, offset = 1,
                               br(),
                               div(h4(textOutput("title_map2"), align = "center"), style = "color:black"),
                               div(h5(textOutput("period_map2"), align = "center"), style = "color:black"),
                               br())
                        ),
                      fluidRow(
                        column(7, offset = 1,
                               leafletOutput("map2", height="530"),
                               br(),
                               actionButton("reset_button", "Reset view")),
                        column(3,
                               uiOutput("category_map2", align = "left"))
                        ),
                      
                      #### Crime Categories - sum ###
                      ## PLOT 1 - UI
                      br(),
                      
                      # text
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
                        
                        p("After analyzing the data on interactive maps, on the below, we can see the crime frequences during this 3-month period.
                          As Manchester is the biggest part of the Greater Manchester, therefore, it is not a surprise that this borough has
                          the highest value for all the categories! In this visual we can find the answer of this question: 
                          In which boroughs the selected category occured the most?")
                      ),
                      
                      br(),
                      
                      fluidRow(
                        
                        column( width = 2, offset = 1,
                                
                                fixedRow(
                                  
                                  selectInput("category3", "Select a category:",
                                              choices = levels(df$category),
                                              selected = "Violence and sexual offences")
                                )

                            )
                        ),
                        
                        fluidRow(
                          style = "margin: auto; width: 90%",
                          plotOutput('plot1')
                      ),
                      
                      br(), br(), br(),
                      
                      # text
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
                        
                        p("We can see that Violence and sexual offences has been observed 6657 times in Manchester, 3244 times in Bolton
                          and 2850 times in Oldham. Moreover, drug crime has been experienced the most in Manchester, 721 times, and 
                          the second high area in this category was Rochdale with 151 records. Also, robbery has been recorded 725 times
                          in Manchester, 143 times in Salford and 141 times in Oldham."),
                        br(),
                        p("We can conclude from the categories that highest values for many boroughs has been observed for the category", 
                          strong("Violence and sexual offences."),p("We can see on the above waffle plot the borough level for 
                          the most occured crime category with its proportion to the overall crime rate for each borough."))
                      ),
                      
                      fluidRow(
                        style = "margin: auto; width: 85%",
                        plotOutput('plot2')
                      ),
                      
                      #text
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:120%",
                        
                        h1("Greater Manchester crime indencies and trends"),
                        
                        br(),
                        
                        p("In this part, we are going to toggle between different types of crime for three months (April, May and June) 
                        of the year 2019. In the trend visual, we can see how much the amount of each crime differs between the 3 months."),
                        
                      ),
                      fluidRow(
                        style = "margin: auto; width: 65%; height:80%",
                        plotlyOutput("crimeTrend")
                        ),
                      
                      #text
                      fluidRow(
                        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:120%",
                        
                        p("In the trend, we can see the evaluation of each crime category from April to June 2019.
                          As seen on the trend, the Violence and sexual offences category has the highest frequency among all
                          the categories.This crime type occured highest in May. 
                          The second most common crime type is Anti-social behaviour. The trend of this crime type is quite flat, 
                          there are not significant changes between observed months. For the other crime types, there are not
                          crucial up and down trends during the 3-month period as well. The third most recorded crime type
                          is Public order."),
                        
                      ),
                      
                      br(), br()
                      
                      
             )
             
  )
)
  
  
  
############### SERVER #####################
  
server <- function(input, output, session) {
    
    ## FIRST MAPS ##
    
    output$freq_map_L <- renderLeaflet({
      
      rpt_locs_L <- rpt_locs %>%
                      filter(force == "City of London Police") %>%
                      filter(borough == input$borough)
      leaflet(rpt_locs_L) %>%
        addProviderTiles("CartoDB.Positron") %>% 
        addCircleMarkers(lng=~long, lat=~lat, 
                         fillColor = "white", color = "red",  
                         radius = ~n*0.1, # this may need to be controlled e.g. radius = ~n*0.1
                         popup = ~ paste0("<strong>Frequency: </strong>", rpt_locs_L$n,
                                                  "<br><strong>Location: </strong>", rpt_locs_L$location,
                                                  "<br><strong>Category: </strong>", rpt_locs_L$category)
                         ) 
      
    })
    
    output$freq_map_M <- renderLeaflet({
      rpt_locs_M <- rpt_locs %>%
                       filter(force == "Greater Manchester Police") %>%
                       filter(borough == input$borough_m)
      leaflet(rpt_locs_M) %>%
        addProviderTiles("CartoDB.Positron") %>% 
        addCircleMarkers(lng=~long, lat=~lat, 
                         fillColor = "white", color = "green",  
                         radius = ~n*0.1, # this may need to be controlled e.g. radius = ~n*0.1
                         popup = ~paste0("<strong>Frequency: </strong>", rpt_locs_M$n,
                                         "<br><strong>Location: </strong>", rpt_locs_M$location,
                                         "<br><strong>Category: </strong>", rpt_locs_M$category)
        ) 
      
    })
    
    ## MAP 2
    output$category_map2 <- renderUI({
      radioButtons("category", "Select a crime category:",
                   choices = levels(map_df$category),
                   selected = "Robbery")
    })  
    
    selected_map2 <- reactive({
      subset(map_df,
             category==input$category)
    })
    
    output$title_map2 <- renderText({
      req(input$category)
      paste0(input$category, " offences by LSOA in Manchester")
    })
    
    output$period_map2 <- renderText({
      req(input$category)
      paste("during May 2019")
    })
    
    ## ssetting the initial long and latitude
    lat <- 53.442788
    lng <- -2.244708
    zoom <- 11
    
    output$map2 <- renderLeaflet({
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lat = lat, lng = lng, zoom = zoom)
    })
    
    observe({
      
      lsoa@data <- left_join(lsoa@data, selected_map2())
      lsoa$rate <- round((lsoa$n / lsoa$pop_All.Ag) * 1000, 1)
      
      qpal <- colorQuantile("YlGn", lsoa$rate, n = 5, na.color = "#bdbdbd")
      
      popup <- paste0("<strong>LSOA: </strong>",
                      lsoa$LSOA11CD,
                      "<br><strong>Category: </strong>",
                      lsoa$category,
                      "<br><strong>Rate: </strong>",
                      lsoa$rate)
      
      leafletProxy("map2", data = lsoa) %>%
        addProviderTiles("CartoDB.Positron") %>% 
        clearShapes() %>% 
        clearControls() %>% 
        addPolygons(data = lsoa, fillColor = ~qpal(rate), fillOpacity = 0.7, 
                    color = "white", weight = 2, popup = popup) %>%
        leaflet::addLegend(pal = qpal, values = ~rate, opacity = 0.7,
                           position = 'bottomright', 
                           title = paste0(input$category, "<br>", " per 1,000 population"))
    })
    
    observe({
      input$reset_button
      leafletProxy("map2") %>% setView(lat = lat, lng = lng, zoom = zoom)
    }) 
    
    
    ## PLOT1
    output$plot1 <- renderPlot({ 
      # order the data frame
      df <- df %>%
        filter(category == input$category3) 
        #filter(date == input$dateInput)
      
      ggplot(df, aes(x = reorder(borough, n), y = n)) + 
        geom_point(size = 12, stat = "identity", color = "black") + 
        geom_text(aes(label = n, fontface = "bold"), color = "white", size = 4) + 
        coord_flip() + 
        theme_minimal(base_size = 20) + 
        xlab("") + ylab("") + 
        ggtitle(paste0("Total Crime Frequencies")) +
        scale_y_continuous(limits=c(0,max(df$n)))
      
    })
    
    #  waffle plot
    output$plot2 <- renderPlot({ 
      
      waffle(df2, rows = 4, size = 2, 
             colors=(RColorBrewer::brewer.pal(n=10,"Set3")),
             title="Borough level Violence and sexual offence as a proportion of total Violence and sexual offence, 06/2019",
             legend_pos = "bottom")
      
    })
    
    ### BAR PLOT ###
    
    output$plot3 <- renderHighchart({
      
      # order the data frame
      df_bar_plt <- df_bar_L %>%
        filter(borough == input$borough2)
      
      df_bar_plt <- df_bar_plt[order(-df_bar_plt$n),]
      
      # plotting
      hc <- highchart() %>%
        hc_title(text = paste0("Crimes in ", input$borough2)) %>% 
        hc_xAxis(categories = as.list(df_bar_plt$category)) %>% 
        hc_add_series(name = "Number of crimes", data = df_bar_plt$n, type = "bar", showInLegend = TRUE) %>% 
        hc_yAxis(title = list(text = "")) %>% 
        hc_exporting(enabled = TRUE)
      hc %>% hc_add_theme(hc_theme_538())
      
    })
    
    ### DOT PLOT ###
    selected_dot <- reactive({
      df <- filter(df_dot_L, category==input$category2)
    })
    
    output$plot4 <- renderDimple({
      selected_dot() %>%
        dimple(borough ~ n, type = "bubble", height = 500, width = "100%") %>%
        xAxis(type = "addMeasureAxis", showGridlines = F, title = "Number of crimes") %>%
        yAxis(type = "addCategoryAxis", orderRule = "Frequency", showGridlines = F, title = "") %>%
        default_colors(c("#7a0177", "#7a0177")) %>%
        add_title(html = paste0("<div style='text-align:center;width:100%'>
                                <b style = 'font-size:100%;'>", input$category2, " ", paste('offences (except London)'),
                                "</div>"))
    })
    
    ### Time series ###
    df_ts_L <- filter(df_ts, force=="City of London Police")
    selected_ts <- reactive({df_ts_L %>% 
        filter(category == input$category2)})
    
    output$title <- renderText({
      paste0(input$category2, " offences by date")
    })
    
    output$plot5 <- renderDygraph({
      crime_xts <- xts(selected_ts()$n, order.by = selected_ts()$date, frequency = 12) #as.Date(selected_ts()$date, format = "%Y/%m/%d")
      
      dygraph(crime_xts, ylab = "Frequency") %>% 
        dySeries(label = "Crimes", color = "#3182bd", fillGraph = TRUE, strokeWidth = 3, drawPoints = TRUE, pointSize = 6) %>% 
        dyOptions(includeZero = TRUE, drawGrid = FALSE,
                  axisLineWidth = 2, axisLabelFontSize = 12) %>% 
        dyLegend(show = "follow") %>% 
        dyCSS("dygraph.css")
      
    })
    
    # pie chart london
    
    output$pie_london <- renderPlotly({
      df_pie_L <- df_pie_L %>% 
                    filter(date == input$Date) 
      
      fig <- plot_ly(df_pie_L, labels = ~category, values = ~n, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste( 'Number of crimes: ', n),
                     marker = list(#colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE)
      fig <- fig %>% layout(title = paste('London Crimes by Categories in the date of',input$Date),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig

      
    })
    #crime trend manchester
    
    output$crimeTrend <- renderPlotly({

      # chart
      p <- ggplot(df_ts_m) +
        geom_line(aes(x = date, y = n, group = category, colour = category), size = 2) +
        #scale_x_discrete(labels = c('April','May','June')) +
        scale_colour_manual(values = rainbow(14)) +
        theme(text = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(angle = 60, hjust = 1)) +
        labs(title = "2019 Monthly crime in the Greater Manchester",
             colour = "Crime type") +
        xlab("") + ylab("")
      
      # Turn it interactive with ggplotly
      p <- ggplotly(p)
      p
      
    })
    
    
}
  
  
###### RUNNING THE APP #######
  
shinyApp(ui, server)
  