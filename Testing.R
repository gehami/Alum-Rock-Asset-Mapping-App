library(shiny)
library(leaflet)
library(dplyr)
# Create a small dataframe
visits = data.frame(City=c("Bangalore", "Delhi", "Chennai", "Bangalore"), 
                    lat=c(12.972442, 28.644800, 13.067439, 12.972442), 
                    lon=c(77.580643, 77.216721, 80.237617, 77.580643), 
                    Year=c("2007", "2008", "2009", "2010")) 

visits$Year = as.Date(visits$Year, format="%Y")

# define ui with slider and animation control for time
ui <- bootstrapPage(
  tags$head(tags$style(
    HTML('
         #input_date_control {background-color: black;}
         #year {background-color: black;}')
    )),
  
  leafletOutput("mymap", height = 600),
  absolutePanel(
    top = 25, right = 40, width = 200, draggable = TRUE,
    id = "input_date_control", class = "panel panel-default",
    sliderInput(inputId = "year", label = "Year", 
                min = min(visits$Year), 
                max = max(visits$Year),
                value = min(visits$Year), step = 365,
                animate=T)
    
  )
  )

server <- function(input, output, session) {
  current <- reactive({
    visits %>% 
      filter(Year == input$year)
  })
  
  past <- reactive({
    visits %>%
      filter(Year <= input$year)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = ~lon,
                 lat = ~lat,
                 data = current()) %>%
      addMarkers(lng = ~lon,
                 lat = ~lat,
                 data = past()) %>%
      addPolylines(lng = ~lon,
                   lat = ~lat,
                   data = past())
  })
}

shinyApp(ui, server)