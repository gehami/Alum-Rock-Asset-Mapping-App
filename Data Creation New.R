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


######## Loading in dataset ##############

install_and_load('readxl')

svcs = readxl::read_excel('SLS  ARUSD Community Resources 2.20.19.xlsx')


######### Adding coordinate variables (lat and lon) - svcs ##############

coords = svcs$Coordinates
lat = as.numeric(gsub('[^[:print:]]', '', gsub('([^0-9]+)([0-9]+)(\\.)([0-9]+)([^0-9]+)([[:print:]]*)', '\\2\\3\\4', coords)))
lon = as.numeric(gsub('[^[:print:]]', '', gsub('([^0-9]+)([0-9]+)(\\.)([0-9]+)([^0-9]+)([0-9]+\\.[0-9]+)([[:print:]]*)', '-\\6', coords)))

svcs$lon = lon
svcs$lat = lat

#for appearance, let's wiggle the lat and lon coordinates for the renaissance academies
ren_academies = grep('Renaissance', svcs$`Schools where services are provided`) #since there are only two academies, and they are all spelled correctly, we can jiggle both a little bit and it's all good
ren_1 = which(svcs$`Schools where services are provided` == unique(svcs$`Schools where services are provided`[ren_academies])[1])
ren_2 = which(svcs$`Schools where services are provided` == unique(svcs$`Schools where services are provided`[ren_academies])[2])

svcs$lon[ren_1] = svcs$lon[ren_1] + sd(lon)/10
svcs$lon[ren_2] = svcs$lon[ren_2] + sd(lon)/10


####### Cleaning up school names #########
svcs$`Schools where services are provided` = gsub('[^[:alpha:]]+$', '', gsub('^[^[:alpha:]]+', '', gsub('[^[:print:]]', '', svcs$`Schools where services are provided`)))

school_names = unique(svcs$`Schools where services are provided`)[order(unique(svcs$`Schools where services are provided`))]

#marking the name of each schoool with a unique lat/lon pairing
unique_school_inds = which(!duplicated(paste0(svcs$lat, svcs$lon)))
school_to_lat_lon = data.frame(school = svcs$`Schools where services are provided`[unique_school_inds], lon = svcs$lon[unique_school_inds], lat = svcs$lat[unique_school_inds], stringsAsFactors = FALSE)
school_to_lat_lon$id = paste0(school_to_lat_lon$lat, school_to_lat_lon$lon)

svcs$id = paste0(svcs$lat, svcs$lon)
svcs$school = NA
for(i in seq_along(school_to_lat_lon$id)){
  svcs$school[svcs$id == school_to_lat_lon$id[i]] = school_to_lat_lon$school[i]
}

undupl_school_inds = which(!duplicated(svcs$school))
school_addresses = data.frame(school = svcs$school[undupl_school_inds], lat = svcs$lat[undupl_school_inds], lon = svcs$lon[undupl_school_inds], stringsAsFactors = FALSE)


###### Making a list element for each school, within each list will be each service --> school_list ###########

#for testing
# dat = svcs
# school_addresses = school_addresses
# school = school_addresses[1,1]

fill_school = function(school, dat, school_addresses){
  school_dat = dat[dat$school %in% school,]
  service_dat = school_dat[,c('Name of Organization', 
                              'Service Category',
                              'Service Type',
                              'Tier of help',
                              'Number of slots available',
                              'Who qualifies?',
                              'Address',
                              'Areas of Service',
                              'Description/Scope of Service')]
  
  colnames(service_dat) = c('Organization',
                            'Category of Service',
                            'Service Provided',
                            'Tier',
                            'Slots',
                            'Qualifies',
                            'Address',
                            'Areas of Service',
                            'Description')
  lat_lng = school_addresses[school_addresses$school == school,2:3]
  
  ret_school = list(
    school = school,
    location = lat_lng,
    services = service_dat
  )
  return(ret_school)
}

school_list = list()
i = 1
for(school in unique(svcs$school)){
  school_list[[i]] = fill_school(school, svcs, school_addresses)
  i = i + 1
}



####### Saving data --> file = 'school services and locations.rds' and file = 'school lat lng locations.rds'  ##############

colnames(school_addresses) = c('school', 'lat', 'lng')

saveRDS(school_list, file = 'Alum_Rock_Asset_Mapping/new school services and locations.rds')
saveRDS(school_addresses, file = 'Alum_Rock_Asset_Mapping/new school lat lng locations.rds')





