library(readr)
library(tidyverse)
library(magrittr)
library(maps)
library(mapdata)
library(Hmisc)
library(leaflet)
library(htmlwidgets)
library(lattice)
library(dplyr)
library(shiny)
library(ggmap)

Street_Clean  <- read.csv("Trash .csv") %>% select(-PublicWorksDistrict)

Street_Clean %<>% separate(col = Location, into = c("Lat","lon"),sep = ",")
Street_Clean$Lat <- gsub(pattern = "[(]",replacement = "",x=Street_Clean$Lat)
Street_Clean$Lat <- as.numeric(Street_Clean$Lat)
Street_Clean$lon <- gsub(pattern = "[)]",replacement = "",x=Street_Clean$lon)
Street_Clean$lon <- as.numeric(Street_Clean$lon)
Street_Clean <- na.omit(Street_Clean)

Street_Clean_Monday <- filter(Street_Clean, Trash == "M" & Recycling == "M")
dat1 <- Street_Clean_Monday[1:50,]
Street_Clean_T <- filter(Street_Clean, Trash == "T" & Recycling == "T")
dat2 <- Street_Clean_T[1:50,]
Street_Clean_W <- filter(Street_Clean, Trash == "W" & Recycling == "W")
dat3 <- Street_Clean_W[1:50,]
Street_Clean_TH <- filter(Street_Clean, Trash == "TH" & Recycling == "TH")
dat4 <- Street_Clean_TH[1:50,]
Street_Clean_F <- filter(Street_Clean, Trash == "F" & Recycling == "F")
dat5 <- Street_Clean_F[1:50,]

data_final <- rbind(dat1,dat2,dat3,dat4,dat5)
####################################################

ui <- fluidPage(

    # Application title
    titlePanel("Different day of trash and recycling in Boston"),
    sidebarLayout(
        sidebarPanel(
            selectInput("day", "Select a day", sort(unique(data_final$Trash)))
        ),

        mainPanel(
           leafletOutput("Plot", width= 2000, height = 1000)
        )
    )
)


server <- function(input, output,session) {

    
output$Plot<- renderLeaflet({
        bounds <- map("state", c('Massachusetts'), fill=TRUE, plot=FALSE)
        icons <- awesomeIcons(
            icon = 'disc',
            iconColor = 'blue',
            library = 'ion',
            markerColor = 'yellow',
            squareMarker = TRUE
)
       data_used <- data_final %>% filter( Trash == input$day)
        leaflet(data = data_used) %>%
            setView(mean(data_used$lon)+0.3,mean(data_used$Lat),zoom = 11) %>%
            addProviderTiles("CartoDB.Positron", group = "Map") %>%
            addAwesomeMarkers(~lon, ~Lat, label = ~Address, group = "Trash", icon = icons) %>%
            addPolygons(data = bounds, group="States", weight=2, fillOpacity = 0) %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("Map"),
                overlayGroups = c("Trash", "States"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

