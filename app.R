
library(ggmap)
library(maptools)
library(maps)
library(leaflet)
library(shiny)

type_maps <- c(Cylindrical="cylindrical", Mercator="mercator", Sinusoidal = "sinusoidal", Gnomonic = "gnomonic" , Rrectangular=  "rectangular") 

#######################################################################

## Building Shiny UI 

ui <- fluidPage(

    titlePanel("Different types of Maps"),

     selectInput("maps", "Select a type of maps", choices = type_maps),
    
        
          mainPanel(
            plotOutput(outputId = "Plot")
        )
    )



server <- function(input, output, session) {

  mapWorld <- map_data("world")
  
  mp1 <- ggplot(mapWorld, aes(x=long, y=lat, group=group))+
    geom_polygon(fill="white", color="black") +
    coord_map(xlim=c(-180,180), ylim=c(-60, 90))
    output$Plot <- renderPlot({
      mp2 <- mp1 + coord_map(input$maps ,xlim=c(-180,180), ylim=c(-60, 90))
       mp2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

