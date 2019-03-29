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




######## Random points around a point in a circle ############

# lat = 37.37487
# lng = -121.843
# num_points = 38
# radius = .001

get_circle_points = function(lat, lng, num_points, radius = .001){
  pi_points = seq(0, 2*pi, length.out = num_points + 1) %>% head(-1)
  x_cords = cos(pi_points) * radius + lng
  y_cords = sin(pi_points) * radius + lat
  print(num_points)
  return(data.frame(lng = x_cords, lat = y_cords))
}

######## Constants ############

RADIUS = .02
MAX_ZOOM = 16
INITIAL_ZOOM = 12

######## Loading in dataset --> sl (services and locations), sa (school addresses)##############
sl = readRDS('new school services and locations.rds')
sa = readRDS('new school lat lng locations.rds')

########### removing those which are not schools - sa and sl ###############

school_inds = grep("School|elementary|Middle|Academy", sa$school, ignore.case = TRUE) # as of 3-29-2019, this successfully gets all of the schools and nothing else

sl = sl[school_inds]
sa = sa[school_inds,]


######### adding num_services to sa - sa ############
sa$num_services = 0
for(n in  1 : nrow(sa)){
  sa$num_services[n] = nrow(sl[[n]]$services)
}


######## Setting up Leaflet Map ############
install_and_load('magrittr')
install_and_load('leaflet')
install_and_load('htmltools')

map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.84, lat = 37.34, zoom = INITIAL_ZOOM) #%>%

service_map = map %>%
  leaflet::addCircleMarkers(lng = sa$lng, lat = sa$lat, radius = sa$num_services, label = sa$school, group = 'general_school_markers', layerId = sa$school)

######## Server #######


library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  ###MAke a reactive element that stores the previous clicked school. Then replaces that with the new one to keep it only one check box.
  current_school = reactiveVal(sa$school[2])
  
  
  output$map <- leaflet::renderLeaflet(service_map)
  
  output$attempt <- renderUI({
    checkboxGroupInput('schools', "Select a School", choices = sa$school, selected = NULL)
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
    # print(which(sa$school %in% input$schools))
    if(length(input$schools) == 0){
      sa_selected = sa
      output$service_table = NULL #renderDataTable(sl_selected[,c('Organization', 'Tier', 'Qualifies', 'Description')],
                                   #          options = list(
                                    #           autoWidth = TRUE
                                     #        ))
      leafletProxy('map') %>% removeMarker(layerId ='accent') %>% clearGroup(group = 'service_markers') %>% addCircleMarkers(lng = sa$lng, lat = sa$lat, radius = sa$num_services, label = sa$school, layerId = sa$school,
                                                                                   group = 'general_school_markers')
      output$target_school = renderText(paste0('Select a school to display services', input$schools))
      return()
    }
    if(length(input$schools) > 1){
      new_school = input$schools[which(!(input$schools %in% current_school()))][1]
      output$attempt <- renderUI(checkboxGroupInput('schools', "Select Schools", choices = sa$school, selected = new_school))
      current_school(new_school)
      return()
    }

    sl_selected = data.frame(sl[which(sa$school %in% input$schools)][[1]]$services, stringsAsFactors = F)
    sa_selected = sa[which(sa$school %in% input$schools),]
    # print(sa_selected)
    # print(sl_selected[,1])
    # print(sl_selected[,1])
    output$service_table = renderDataTable(sl_selected[,c('Organization', 'Tier', 'Qualifies', 'Description')],
                                           options = list(
                                             autoWidth = TRUE
                                           ))
    service_dots = get_circle_points(lat = sa_selected$lat, lng = sa_selected$lng, num_points = sa_selected$num_services, radius = RADIUS)# * 10^(input$map_zoom - INITIAL_ZOOM))
    service_dot_labels = paste(sl_selected$Organization, gsub('([[:print:]]{50}[^[:space:]]*[[:space:]])', 
                                                              '\\1<br/>', (sl_selected$Description)), sep = '<br/>') #replace this with 50 characters and then up to the next space
    
    leafletProxy('map') %>% clearGroup(group = 'general_school_markers') %>% clearGroup(group = 'service_markers') %>% addCircleMarkers(lng = sa_selected$lng, lat = sa_selected$lat,
                                             radius = sa_selected$num_services,
                                             color = 'green', layerId = 'accent', label = input$schools) %>% 
      addMarkers(lng = service_dots$lng, lat = service_dots$lat, label = lapply(service_dot_labels, HTML), group = 'service_markers')
    output$target_school = renderText(paste0('Showing services provided at: ', input$schools))

  }, ignoreNULL = FALSE)
  
  observeEvent(input$map_marker_click,{
    click = input$map_marker_click$id
    if(is.null(click)) return()
    if(click == 'accent'){
      print('accent')
      output$attempt <- renderUI(checkboxGroupInput('schools', "Select Schools", choices = sa$school, selected = NULL))
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
      checkboxGroupInput('schools', "Select Schools", choices = sa$school, selected = school)
    })
    print(click)
  })
  
  
  # for(school in as.character(sa[,1])){
  #   checkboxInput(school, school, value = F)
  #   conditionalPanel(
  #     condition = paste0('input.',school,'== true'),
  #     h3(school)
  #   )
  
  
})
