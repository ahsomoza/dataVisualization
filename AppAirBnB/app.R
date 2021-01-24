library(shiny)
library(leaflet)
library(stringr) # For str_remove_all function
library(dplyr)
library(htmltools)
library(ggplot2)
library(plyr)
library(plotly)
library(rgdal)
library(DescTools) # For mode

data <- read.csv("./data/listings.csv")

# https://github.com/codeforamerica/click_that_hood/blob/master/public/data/madrid.geojson
neighbourhoods <- rgdal::readOGR("./data/madrid.geojson")

# https://github.com/codeforamerica/click_that_hood/blob/master/public/data/madrid-districts.geojson
neighbourhoods_groups <- rgdal::readOGR("./data/madrid-districts.geojson")

pal <- colorFactor(c("blue", "red","purple", "green"), domain = c("Entire home/apt", "Hotel room", "Private room", "Shared room"))

############################## GEOJSON MODIFICATIONS (7 districs are different) #####################
# In the geojson with the districts we can find that some districts have different names in the dataset
neighbourhoods_groups$name <- replace(
  neighbourhoods_groups$name, 
  c(5, 6, 7, 8, 9, 19, 20), 
  c("Chamartín", "Tetuán", "Chamberí", "Fuencarral - El Pardo", "Moncloa - Aravaca", "Vicálvaro", "San Blas - Canillejas")
)

############################## DATASET MODIFICATIONS (Comparations between neighbourhoods) ####################################
# The variable price comes with a dolar in the first position and also with ","
# We are interested in computing the average of the price per neighbourhood,
# so price should be a numeric type like double
data$price <- lapply(data$price, substring, first=2) # Delete $
data$price <- lapply(data$price, str_remove_all, pattern=",") # Delete ,
data$price <- as.double(data$price)
# Also we will compare between neighbouthood/districts the average of other variables 
data$accommodates <- as.integer(data$accommodates)
data$availability_365 <- as.double(data$availability_365)
data$review_scores_location <- as.double(data$review_scores_location)
data$review_scores_value <- as.double(data$review_scores_value)

# Now, we have to compute the mean value per neighbourhood
vars <- c("price", "availability_365", "review_scores_location", "review_scores_value")
variables_data <- aggregate(data[, vars], list(Neighbourhood=data$neighbourhood_cleansed), mean, na.rm=TRUE)
variables_data[,vars] <- lapply(variables_data[, vars], round, digits=2) # Round to 2 digits
names(variables_data) <- c("Neighbourhood", "Price", "Availability", "ReviewScoresPerLocation", "ReviewScores")

# For the variable accommodates we are not interested in computing the average, but the mode
accom <- aggregate(data[, "accommodates"], list(Neighbourhood=data$neighbourhood_cleansed), Mode, na.rm=TRUE)
names(accom) <- c("Neighbourhood", "Accommodates")
accom$Accommodates <- lapply(accom$Accommodates, function(x) { x <- max(as.vector(x))})
variables_data <- merge(variables_data, accom, by = 'Neighbourhood')

# Finally we combine the geojson with the relevant information of the dataset
MapNeighbourhood <- merge(neighbourhoods, variables_data, by.x = 'name', by.y = 'Neighbourhood')

# We repeat these steps for districts
variables_data <- aggregate(data[, vars], list(Neighbourhood=data$neighbourhood_group_cleansed), mean, na.rm=TRUE)
variables_data[,vars] <- lapply(variables_data[, vars], round, digits=2)
names(variables_data) <- c("District", "Price", "Availability", "ReviewScoresPerLocation", "ReviewScores")
accom <- aggregate(data[, "accommodates"], list(Neighbourhood=data$neighbourhood_group_cleansed), Mode, na.rm=TRUE)
names(accom) <- c("District", "Accommodates")
accom$Accommodates <- lapply(accom$Accommodates, function(x) { x <- max(as.vector(x))})
variables_data <- merge(variables_data, accom, by = 'District')
MapDistricts <- merge(neighbourhoods_groups, variables_data, by.x = 'name', by.y = 'District')
############################## HOST DATA MANIPULATION ####################################

host <- dplyr::select(data,c("host_id","host_name","host_is_superhost", "host_since"))
host <- subset(host, host_is_superhost!="")
host$host_is_superhost <- as.factor(host$host_is_superhost)
levels(host$host_is_superhost) <- c("False", "True")

host$host_since<-as.Date(host$host_since)
host_number_listings<- host %>% dplyr::count(host_id,host_name,host_is_superhost, host_since ) 
host_number_listings<- host_number_listings[order(host_number_listings$n,decreasing=TRUE),]
#host_number_listings$host_is_superhost<- as.factor(host_number_listings$host_is_superhost)
host_number_listings$keyed<- paste(host_number_listings$host_name,host_number_listings$host_id,sep="/")
host_number_listings$keyed <- factor(host_number_listings$keyed, levels = host_number_listings$keyed[order(host_number_listings$n)])

df <- read.csv("data/calendarMeanPrice.csv")
df <- df[,c("date", "price")]
df$date <- as.Date(df$date, "%Y-%m-%d")
df$dayOfWeek <- strftime(df$date, "%u")
df$dayOfYear <-strftime(df$date, "%j")
df$weekOfYear <- strftime(df$date, "%W")
df$year <-strftime(df$date, "%Y")
df$month <-strftime(df$date, "%m")
df$monthYear<- paste(df$year, "-", df$month)
df$weekYear <- paste(df$year, "-", df$weekOfYear)

############################## UI ####################################
# Choices for drop-downs
varsInputNeighbourhood <- c(
  "Select a variable" = "Select a variable",
  "Price" = "Price",
  "Accomodates" = "Accomodates",
  "Availability" = "Availability",
  "Score per location" = "ReviewScoresPerLocation",
  "Total score" = "ReviewScores"
)
initialSelection <- c(
  "About" = "About",
  "Geographical distribution of the listings" = "MapIndividuals",
  "Neighbourhoods differences" = "MapNeighbourhood",
  "Districs differences" = "MapDistrict",
  "Temporal Analysis"="Temp",
  "Host Analysis"="Hosts")

hostOption <- c(
  "Total" = "1979-01-01",
  "During 2020" = "2020-01-01",
  "Between 2016 and 2020" = "2016-01-02",
  "Before 2016" = "2016-01-01")
district <- levels(as.factor(data$neighbourhood_group_cleansed))

ui <- fluidPage(
  titlePanel("Madrid AirBNB"),
  mainPanel(
    sidebarPanel(
      selectInput(
        "chosenMap", "Select the visualization",
        initialSelection)
      , width = 100),
    
    # Only show this panel if the map is MapNeighbourhood
    conditionalPanel(
      condition = "input.chosenMap == 'About'",
      h5("The city of Madrid is one of the cities that concentrates a large and 
         varied listing of short-term Airbnb rentals, and we are interested in 
         analysing the data related to it. With the development of this visual 
         analytics app, we intend to generate a system that helps an analyst to 
         face the analysis of the problem generated by Airbnb in the cities and 
         therefore gain new knowledge in order to make any conclusion of how the 
         app is really being used in the city. ", align="justify"),
      h5("By using the public available data of the listings of Airbnb in the 
      city of Madrid, we intend to answer some questions: "),
      h5("- How are the different offers distributed across the city?"),  
      h5("- Do we find differences between the different neighbourhoods/districts in the city?"),
      h5("- Is there any variation in the offers across time? Seasonality found? "),
      h5("- What can be learnt about the different hosts? (Their number of listings , since when they are host, superhost or not"),
      h1(""),
      h5("This project is made by:"),
      h5("- Donís Ebri, Pablo"),
      h5("- Gómez-Carpintero García, Teresa"),
      h5("- Herranz Somoza, Alberto")
    ),

    # Only show this panel if the map is MapNeighbourhood
    conditionalPanel(
      condition = "input.chosenMap == 'MapNeighbourhood'",
      leafletOutput("mymap"),
      selectInput("var1", label = "Choose a variable to display",
                  choices = varsInputNeighbourhood,
                  selected = "Select a variable")
    ),
    
    # Only show this panel if the map is MapDistrict
    conditionalPanel(
      condition = "input.chosenMap == 'MapDistrict'",
      leafletOutput("mymapGroup"),
      selectInput("var2", label = "Choose a variable to display",
                  choices = varsInputNeighbourhood,
                  selected = "Select a variable")
    ),
    
    # Only show this panel if the map is MapIndividuals
    conditionalPanel(
      condition="input.chosenMap == 'MapIndividuals'",
      leafletOutput("mymapIndividual"),
      selectInput("rtype", 
                  label="Select a room type",
                  choices=c("Select an option", "Entire home/apt", "Hotel room", "Private room", "Shared room"),
                  selected = "Select an option"),
      selectInput("dist", 
                  label="Select a district",
                  choices=c("All",district),
                  selected = "All")
    ),
    
    conditionalPanel(
      condition="input.chosenMap == 'Hosts'",
      sliderInput("hostDate", label = h5("Select the date range to filter by the 
                                         date the host begin to use the app:"), min = as.Date("2009-04-16"), 
                  max = Sys.Date(), value = c(as.Date("2009-04-16"), Sys.Date())),
      fluidRow(
        splitLayout(cellWidths = c("65%", "35%"), plotOutput("plotHost"), plotOutput("plotHostDonut"))
      )
    ),
    conditionalPanel(
      condition="input.chosenMap == 'Temp'",
      sidebarPanel(
        fluidRow(
          column(4,
                 selectInput("dateRange", 
                             label = "Choose a date range to show",
                             choices = list("By day", 
                                            "By week",
                                            "By month"),
                             selected = "By month")
          ),
          column(4,
                dateRangeInput("dates", label="Select date range to analyze",   start  = "2018-01-01",
                              end    = "2021-12-30", min    = "2017-03-06",
                              max    = "2021-12-30")),
          column(4,
                 selectInput("dateForm", 
                             label = "Choose a date type to group by",
                             choices = list("By day of week", 
                                            "By week of year",
                                            "By day of year"),
                             selected = "By day of week"))
          )
        , width = 100),      
      fluidRow(
        mainPanel(
          splitLayout(cellWidths = c("55%", "45%"), 
              plotlyOutput("plot"),
              plotlyOutput("plot2")
            )
        , width=95)
      )
    ), width = "98%")
)

############################## SERVER ####################################
server <- function(input, output, session) {
  
############################ NEIGHBOURHOODS/DISTRICTS #####################
  output$mymap <- renderLeaflet({
    leaflet(MapNeighbourhood) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "blue",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      )
  })
  
  output$mymapGroup <- renderLeaflet({
    leaflet(MapDistricts) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "blue",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      )
  })
  
  observe({
    #colorBy <- input$var
    mapSel <- input$chosenMap
    if(mapSel=="MapNeighbourhood"){
      mapSelected <- MapNeighbourhood
      mapObject <- "mymap"
      textPopup <- "Neighbourhood: "
      colorBy <- input$var1
    }else{
      mapSelected <- MapDistricts
      mapObject <- "mymapGroup"
      textPopup <- "District: "
      colorBy <- input$var2
    }
    
    if(colorBy == "Select a variable"){
      leafletProxy(mapObject) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = mapSelected, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = "blue",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste( "<b>"
                                   , textPopup
                                   , "</b>"
                                   , mapSelected$name))
      
    }
    else if(colorBy == "Price"){
      colorData <- mapSelected[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      
      leafletProxy(mapObject) %>%
        clearShapes() %>%
        addPolygons(data = mapSelected, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = pal(colorData),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste( "<b>Neighbourhood: </b>"
                                   ,  mapSelected$name
                                   , "<br>"
                                   , "<b>"
                                   , colorBy
                                   ,": </b> $"
                                   ,  mapSelected[[colorBy]]
                                   , "/night <br>"
                                   
                    )) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
      
    }
    else if(colorBy == "Accomodates"){
      
      factpal <- colorFactor(c("red","blue","green","purple","orange","yellow"), domain = c(1,2,3,4,5,6))
      
      leafletProxy(mapObject) %>%
        clearShapes() %>%
        addPolygons(data = mapSelected, weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    color = factpal(mapSelected$Accommodates),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste( "<b>Neighbourhood: </b>"
                                   ,  mapSelected$name
                                   , "<br>"
                                   , "<b>"
                                   , colorBy
                                   ,": </b>"
                                   ,  mapSelected$Accommodates
                                   , " <br>"
                                   
                    )) %>%
        addLegend("bottomleft", pal=factpal, values=c(1,2,3,4,5,6), title=colorBy,
                  layerId="colorLegend")
      
    }
    else{
      colorData <- mapSelected[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      
      leafletProxy(mapObject) %>%
        clearShapes() %>%
        addPolygons(data = mapSelected, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = pal(colorData),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste( "<b>Neighbourhood: </b>"
                                   ,  mapSelected$name
                                   , "<br>"
                                   , "<b>"
                                   , colorBy
                                   ,": </b> "
                                   ,  mapSelected[[colorBy]]
                                   , "<br>"
                                   
                    )) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
      
    }
  })
  
  ############################ MAP INDIVIDUALS #####################
  output$mymapIndividual <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron,
                                   options = providerTileOptions(noWrap = TRUE)
    ) %>%
      addCircleMarkers(
        lng = data$longitude, 
        lat=data$latitude,
        radius = 1,
        opacity = 1,
        color = pal(data$room_type),
        popup = paste( "<a href='"
                       , data$listing_url
                       , "' target='_blank'>"
                       , data$name
                       , "</a>"
                       , "<br>"
                       , "<b>Score: </b>"
                       ,  data$review_scores_rating 
                       , "/100"
                       , "<br>"
                       , "<b>Zone: </b>"
                       ,  data$neighbourhood_cleansed 
                       , "<br>"
                       , "<b>Accomodates: </b>"
                       ,  data$accommodates 
                       , "<br>"
                       , "<b>Min Nights: </b>"
                       ,  data$minimum_nights 
                       , "<br>"
                       , "<b>Price: </b>"
                       ,  data$price
                       , "/night"
        )
      ) %>%
      addLegend("bottomleft",pal=pal,values=c("Entire home/apt", "Hotel room", "Private room", "Shared room"),title="Room type")
  })
  
  observe({
    if(input$rtype=="Select an option"){
      filas <- data
      if(input$dist != "All"){
        filas <- subset(data, subset=neighbourhood_group_cleansed==input$dist)
      }
      leafletProxy("mymapIndividual") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = filas$longitude, 
          lat = filas$latitude,
          radius = 1,
          opacity = 1,
          color = pal(filas$room_type),
          popup = paste( "<a href='"
                         , filas$listing_url 
                         , "' target='_blank'>"
                         , filas$name
                         , "</a>"
                         , "<br>"
                         , "<b>Score: </b>"
                         ,  filas$review_scores_rating
                         , "/100"
                         , "<br>"
                         , "<b>Zone: </b>"
                         ,  filas$neighbourhood_cleansed
                         , "<br>"
                         , "<b>Accomodates: </b>"
                         ,  filas$accommodates   
                         , "<br>"
                         , "<b>Min Nights: </b>"
                         ,  filas$minimum_nights
                         , "<br>"
                         , "<b>Price: </b>"
                         ,  filas$price
                         , "/night"
          )
        )%>%
        addLegend("bottomleft",pal=pal,values=c("Entire home/apt", "Hotel room", "Private room", "Shared room"),title="Room type")
    }
    else{
      filas <- subset(data, subset=room_type==input$rtype)
      if(input$dist != "All"){
        filas <- subset(filas, subset=neighbourhood_group_cleansed==input$dist)
      }
      leafletProxy("mymapIndividual") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = filas$longitude, 
          lat = filas$latitude,
          radius = 1,
          opacity = 1,
          color = pal(input$rtype),
          popup = paste( "<a href='"
                         , filas$listing_url 
                         , "' target='_blank'>"
                         , filas$name
                         , "</a>"
                         , "<br>"
                         , "<b>Score: </b>"
                         ,  filas$review_scores_rating 
                         , "/100"
                         , "<br>"
                         , "<b>Zone: </b>"
                         ,  filas$neighbourhood_cleansed
                         , "<br>"
                         , "<b>Accomodates: </b>"
                         ,  filas$accommodates 
                         , "<br>"
                         , "<b>Min Nights: </b>"
                         ,  filas$minimum_nights
                         , "<br>"
                         , "<b>Price: </b>"
                         ,  filas$price
                         , "/night"
          )
        )%>%
        addLegend("bottomleft",pal=pal,values=c("Entire home/apt", "Hotel room", "Private room", "Shared room"),title="Room type")
      
    }
  })
  ############################ HOST ANALYSIS #######
 
    hd <- reactive({
      req(input$hostDate)
      hostDt <- as.vector(input$hostDate)
      sub <- subset(host_number_listings, host_since<=hostDt[2] & host_since>=hostDt[1])
      ret <- head(sub,30)
      })
    
    output$plotHost <- renderPlot({
        ggplot(hd(),aes(x=n, y=keyed, fill=host_is_superhost)) +
            geom_col() +
            theme(axis.text.x=element_text(angle=45, hjust=1)) +
            labs(y="Host identifier", x = "Number of listings")
    })

    hdr <- reactive({
      req(input$hostDate)
      hostDt <- as.vector(input$hostDate)
      sub <- subset(host_number_listings, host_since<=hostDt[2] & host_since>=hostDt[1])
      superhost <- sub %>% dplyr::count(host_is_superhost)
      # Compute percentages
      superhost$fraction = superhost$n / sum(superhost$n)
      # Compute the cumulative percentages (top of each rectangle)
      superhost$ymax = cumsum(superhost$fraction)
      # Compute the bottom of each rectangle
      superhost$ymin = c(0, head(superhost$ymax, n=-1))
      superhost
    })
    
    # Make the plot
    output$plotHostDonut <- renderPlot({
      ggplot(hdr(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=host_is_superhost)) +
        geom_rect() +
        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
        xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
    })
    

  ############################ TEMPORAL ANALYSIS #####################
    output$plot <- renderPlotly({
      if(input$dateRange=="By day"){
        dfT <- subset(df, date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
        dfT$dayOfWeek <- as.numeric(dfT$dayOfWeek)
        ggplot(dfT, aes(x=date, y=price, fill=dayOfWeek)) +
          ggtitle("Price by date")+
          geom_bar(stat = "identity")+
          theme_minimal() +
          xlab("Date")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(dfT$price),max(dfT$price)))
      }else if (input$dateRange=="By week"){
        dfT <- subset(df, date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
        df2 <- aggregate(dfT$price, list(dfT$weekYear), FUN=mean) 
        df2$date <- df2$Group.1
        df2$price <- as.numeric(df2$x)
        df2 <- df2[,c("date", "price")]
        ggplot(df2, aes(x=date, y=price))+
          geom_bar(stat = "identity", fill="steelblue")+
          ggtitle("Price by date")+
          theme_minimal() +
          theme(axis.ticks.x=element_blank(),
                axis.text.x =element_blank()) +
          xlab("Week")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(df2$price),max(df2$price)))
      }else if (input$dateRange=="By month"){
        dfT <- subset(df, date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
        df2 <- aggregate(dfT$price, list(dfT$monthYear), FUN=mean) 
        df2$date <- df2$Group.1
        df2$price <- as.numeric(df2$x)
        df2 <- df2[,c("date", "price")]
        ggplot(df2, aes(x=date, y=price)) +
          ggtitle("Price by date")+
          theme_minimal()+
          theme(axis.ticks.x=element_blank(),
                axis.text.x =element_blank()) +
          geom_bar(stat = "identity", fill="steelblue")+
          xlab("Month")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(df2$price),max(df2$price)))
      }
    })
    output$plot2 <- renderPlotly({
      dfT <- subset(df, date > as.Date(input$dates[1]) & date < as.Date(input$dates[2]))
      if(input$dateForm=="By day of week"){
        dfT$dayOfWeek <- as.numeric( dfT$dayOfWeek)
        df2 <- aggregate(dfT$price, list(dfT$dayOfWeek), FUN=mean) 
        df2$day <- df2$Group.1
        df2$price <- as.numeric(df2$x)
        df2 <- df2[,c("day", "price")]
        ggplot(df2, aes(x=day, y=price))+
          geom_bar(stat = "identity", fill="steelblue")+
          ggtitle("Average price")+
          theme_minimal() +
          xlab("Day of week")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(df2$price),max(df2$price)))+ 
          scale_x_continuous(breaks=1:7, labels =c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      }else if (input$dateForm=="By day of year"){
        df2 <- aggregate(dfT$price, list(dfT$dayOfYear), FUN=mean) 
        df2$day <- df2$Group.1
        df2$price <- as.numeric(df2$x)
        df2 <- df2[,c("day", "price")]
        ggplot(df2, aes(x=day, y=price))+
          geom_bar(stat = "identity", fill="steelblue")+
          ggtitle("Average price")+
          theme_minimal() +
          theme(axis.ticks.x=element_blank(),
                axis.text.x =element_blank()) +
          xlab("Day of year")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(df2$price),max(df2$price)))
      }else if (input$dateForm=="By week of year"){
        df2 <- aggregate(dfT$price, list(dfT$weekOfYear), FUN=mean) 
        df2$week <- df2$Group.1
        df2$price <- as.numeric(df2$x)
        df2 <- df2[,c("week", "price")]
        ggplot(df2, aes(x=week, y=price))+
          ggtitle("Average price")+
          geom_bar(stat = "identity", fill="steelblue")+
          theme_minimal() +
          theme(axis.ticks.x=element_blank(),
                axis.text.x =element_blank()) +
          xlab("Week of year")+
          ylab("Price ($)")+
          coord_cartesian(ylim=c(min(df2$price),max(df2$price)))
      }
    })
}

shinyApp(ui, server)
