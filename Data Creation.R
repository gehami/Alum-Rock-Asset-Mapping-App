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

four_schools = data.frame(readxl::read_excel('Community Resources - Alum Rock.xlsx', sheet = 'Master'), stringsAsFactors = F)


######## cleaning up the school column --> four_schools, new_school_names ###########

# print(four_schools$Schools.where.services.are.provided)

new_school_names = gsub('(\r)', '', gsub('(\n)', '', four_schools$Schools.where.services.are.provided))

new_school_names[grep('Lee Mathson School', new_school_names)] = "Lee Mathson Middle School"
new_school_names[grep('Cassell Elementary ', new_school_names)] = "Cassell Elementary"
new_school_names[grep('Alum Rock', new_school_names)] = "Alum Rock Unified School District (all)"


four_schools$school = new_school_names

four_schools = four_schools[!is.na(four_schools$school),]
four_schools = four_schools[four_schools$school != 'Foxdale Area, Overfelt, 10th and Williams, Roundatble, Edenvale, Roosevelt Park, Plata Independence Area.',]

# four_schools = four_schools[four_schools$school %in% c('Arbuckle', 'Aptitud Community Academy at Goss', 'Lee Mathson Middle School', 'Hubbard'),]

####### Adding lat and lon of each school --> school_addresses ##############
install_and_load('magrittr')
school_addresses = data.frame(school = unique(four_schools$school), lat = 0, lng = 0) 
school_addresses = school_addresses[order(school_addresses$school),]


#adding in the new Lat and Lon here:
lat_and_lng = c(37.345214, -121.812784, #adelante
                37.368932, -121.833936, #Alum Rock
                37.349931, -121.831957, #aptitud
                37.344171, -121.840271, #arbuckle
                37.344979, -121.829435, #Cassell
                37.349315, -121.845277, #Cesar Chavez
                37.337329, -121.829920, #Fischer
                37.368090, -121.815085, #Cureton/Horace
                37.339812, -121.836680, #Dorsa
                37.330528, -121.838722, #Hubbard
                37.370360, -121.814489, #Joseph George
                37.350812, -121.851733, #Learning in an Urban Community with High Achievement
                37.351161, -121.842283, #Lee Mathson
                37.377349, -121.823753, #Linda Vista
                37.360130, -121.825057, #Lyndale
                37.336016, -121.827458, #Meyer
                37.381833, -121.833151, #Milard McCollam Elementary
                37.341563, -121.815768, #Ocala STEAM Academy
                37.375758, -121.843651, #Painter Elementary
                37.337774, -121.829647, #Renaissance Academy at Fischer
                37.351349, -121.842329, #Renaissance Academy at Mathson
                37.349835, -121.823529, #Ryan Elementary
                37.350812, -121.852000, #San Antonio Elementary
                37.374990, -121.843076 #Sheppard Middle
                )


#hubbard: 37.330552, -121.838732
#Aptitud: 37.349987, -121.831931
#Lee Mathson: 37.351204, -121.842329
#Arbuckle: 37.344217, -121.840252

school_addresses$lat = lat_and_lng[seq(from = 1, to = (length(lat_and_lng) - 1), by = 2)]
school_addresses$lng = lat_and_lng[seq(from = 2, to = (length(lat_and_lng)), by = 2)]


###### Making a list element for each school, within each list will be each service --> school_list ###########

#for testing
# dat = four_schools
# school_addresses = school_addresses
# school = new_school_names[1]

fill_school = function(school, dat, school_addresses){
  school_dat = dat[dat$school %in% school,]
  service_dat = school_dat[,c('Name.of.Organization', 
                              'Service.Category',
                              'Services.Provided',
                              'Tier.of.help',
                              'Number.of.slots.available',
                              'Who.qualifies.',
                              'Address',
                              'Areas.of.Service',
                              'Description.Scope.of.Service')]
  
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
for(school in unique(four_schools$school)){
  school_list[[i]] = fill_school(school, four_schools, school_addresses)
  i = i + 1
}










###### Making a quick table for the school name, lat lon, and number of services ###########




####### Saving data --> file = 'school services and locations.rds' and file = 'school lat lng locations.rds'  ##############

saveRDS(school_list, file = 'school services and locations.rds')
saveRDS(school_addresses, file = 'school lat lng locations.rds')


