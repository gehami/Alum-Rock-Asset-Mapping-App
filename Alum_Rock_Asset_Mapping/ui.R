#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
######## Function for sampling a dataset #######

sample_dat = function(dat, rows = 10){
  return(dat[sample(1:nrow(dat), rows),])
}

install_and_load = function(library){
  if(!is.character(library)){
    message("Please type library name as a character value")
    return(NULL)
  }
  if(!require(library, character.only = T)){
    install.packages(library)
    require(library, character.only = T)
  }
  return(NULL)
}



#opens library if installed, if not, installs it (character input) 
install_and_load = function(library){
  if(!is.character(library)){
    message("Please type library name as a character value")
    return(NULL)
  }
  if(!require(library, character.only = T)){
    install.packages(library)
    require(library, character.only = T)
  }
  return(NULL)
}


######## Loading in dataset --> sl (services and locations), sa (school addresses)##############
sl = readRDS('new school services and locations.rds')
sa = readRDS('new school lat lng locations.rds')

sa$num_services = 0
for(n in  1 : nrow(sa)){
  sa$num_services[n] = nrow(sl[[n]]$services)
}


######## Setting up Leaflet Map ############
install_and_load('magrittr')
install_and_load('leaflet')

map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.84, lat = 37.34, zoom = 12) #%>%

service_map = map %>%
  leaflet::addCircleMarkers(lng = sa$lng, lat = sa$lat, radius = sa$num_services, label = sa$school, layerId = sa$school)


#### UI #######

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Map of Services in Alum Rock"),
  tags$head(tags$style("#target_school{color: black;
                       font-size: 20px;
                       font-style: bold;
                       }
                       #schools{font-size: 16px;}"
  )
  ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3('Things you can do:'),
      h5('1) Select a school from the options below'),
      h5('2) Select a school by clicking on the school on the map'),
      h5('3) See what services are offered at the selected school in the table below'),
     uiOutput('attempt')

      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leaflet::leafletOutput('map'),
      br(),
      textOutput('target_school'),
      br(),
      dataTableOutput('service_table')
    )
  )

))
