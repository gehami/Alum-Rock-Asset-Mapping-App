#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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
sl = readRDS('school services and locations.rds')
sa = readRDS('school lat lng locations.rds')

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
  setView(lng = -121.84, lat = 37.34, zoom = 13) #%>%

service_map = map %>%
  leaflet::addCircleMarkers(lng = sa$lng, lat = sa$lat, radius = sa$num_services, label = sa$school, layerId = sa$school)


######## more stuff ########


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Map of Services at target four schools"),
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
        # radioButtons('schools', "Select Schools", choices = sa$school),
        uiOutput('attempt')
        # uiOutput('checkboxes'),
        # uiOutput('services')
        
        
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
   # fluidRow(column(width = 11, 
   #                 textOutput('target_school'),
   #                 br(),
   #                 dataTableOutput('service_table'),
   #                 offset = 0.5
   #                 )
   #   
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- leaflet::renderLeaflet(service_map)
   
   output$attempt <- renderUI({
     radioButtons('schools', "Select a School", choices = sa$school, selected = sa$school[2])
   })
   
   # output$checkboxes <- renderUI({
   #   #schools = sa$school
   #   lapply(sa$school, function(school) {
   #     checkboxInput(school, school, value = F)
   # 
   #   })
   # })
   output$schoolnames <- renderUI({
     print()
   })
   
   observeEvent(input$schools,{
     # print(input$schools)
     # print(which(sa$school %in% input$schools))
     sl_selected = data.frame(sl[which(sa$school %in% input$schools)][[1]]$services, stringsAsFactors = F)
     sa_selected = sa[which(sa$school %in% input$schools),]
     # print(sl_selected[,1])
     output$service_table = renderDataTable(sl_selected[,c('Organization', 'Tier', 'Qualifies', 'Description')],
                                            options = list(
                                              autoWidth = TRUE
                                            ))
     leafletProxy('map') %>% addCircleMarkers(lng = sa_selected$lng, lat = sa_selected$lat,
                                              radius = sa_selected$num_services,
                                              color = 'green', layerId = 'accent', label = input$schools)
     output$target_school = renderText(paste0('Showing services provided at: ', input$schools))
   })
   
   observeEvent(input$map_marker_click,{
     click = input$map_marker_click$id
     if(is.null(click)) return()
     if(click == 'accent'){
       print('accent')
       return()
     }
     school = click
     # sl_selected = data.frame(sl[which(sa$school %in% school)][[1]]$services, stringsAsFactors = F)
     # sa_selected = sa[which(sa$school %in% school),]
     # print(sl_selected[,1])
     # output$service_table = renderDataTable(sl_selected[,c('Organization', 'Tier', 'Qualifies', 'Description')],
     #                                        options = list(
     #                                          autoWidth = TRUE
     #                                        ))
     # leafletProxy('map') %>% addCircleMarkers(lng = sa_selected$lng, lat = sa_selected$lat,
     #                                          radius = sa_selected$num_services*2,
     #                                          color = 'green', layerId = 'accent', label = school)
     output$attempt <-  renderUI({
       radioButtons('schools', "Select Schools", choices = sa$school, selected = school)
     })
     print(click)
   })

     
     # for(school in as.character(sa[,1])){
     #   checkboxInput(school, school, value = F)
     #   conditionalPanel(
     #     condition = paste0('input.',school,'== true'),
     #     h3(school)
     #   )

   
}

# Run the application 
shinyApp(ui = ui, server = server)

