install.packages('sf')
library(sf)

#Downloads the line shapefile from the cogcc website
link = 'https://cogcc.state.co.us/documents/data/downloads/gis/DIRECTIONAL_LINES_SHP.ZIP'
destination = paste0(getwd(), '/', basename(link))
download.file(url=link, destfile = destination, mode='wb')
unzip(destination, exdir=getwd())
unlink(destination)

#Downloads the county shapefile
link = 'https://storage.googleapis.com/co-publicdata/lm_cnty.zip'
destination = paste0(getwd(), '/', basename(link))``
download.file(url=link, destfile = destination, mode='wb')
unzip(destination, exdir=getwd())
unlink(destination)

#Lets read in the shapefiles as sf objects
wells = st_read(paste0(getwd(), '/Directional_Lines.shp'))
counties = st_read(paste0(getwd(), '/lm_cnty.shp'))

#Transform them to Colorado State Plane Central
epsg = 26754
wells = st_transform(wells, epsg)
counties = st_transform(counties, epsg)

#There's alot of wells in here, let's just filter to Horizontal wells in Wattenberg
wells = wells[wells$Deviation=='Horizontal' & !is.na(wells$Deviation), ]
wells = wells[wells$Field_Name=='WATTENBERG', ]

#Let's see what a few of these look like
plot(wells$geometry[1:5])

#Cool, looks like they're entire surveys.  Let's simplify them with a 20' tolerance.
wells = st_simplify(wells, dTolerance = 20)

#Let's see what it looks like
plot(wells$geometry, col='red')
plot(counties$geometry, add=TRUE)

#How about we count the number of offset wells within a mile of any well.
#First, let's make buffer polygons around each well
buffers = st_buffer(wells, dist=2640)
plot(buffers$geometry[1], border='green')
plot(wells$geometry[1], add=TRUE)

#Looks good.  Let's now get a list of the wells that intersect each buffer
ints = st_intersects(buffers, wells)
plot(buffers$geometry[1], border='green')
plot(wells$geometry[ints[[1]]], add=TRUE)

#Nice, now we can count the number of wells like this
wells$offsets = do.call(c, lapply(ints, length))
plot(wells[,'offsets'])

#How about we get the total length of wells actually within each buffer?
wInt = st_intersection(buffers[1,'geometry'], wells[ints[[1]],])
wInt$intersected_length = as.numeric(st_length(wInt))
total_length = sum(wInt$intersected_length)
plot(buffers$geometry[1], border='green', main = paste0('Total Length: ', round(total_length,0)))
plot(wells$geometry[ints[[1]]], add=TRUE)
plot(wInt$geometry, add=TRUE, col='red')

#Let's loop that to do it for all wells.
#Not particularly fast but it gets the job done.
#I usually speed this up by just using two point well sticks, sometimes using parallel
getIntLength = function(buffer, wellSubset){
  wInt = st_intersection(buffer, wellSubset)
  wInt$intersected_length = as.numeric(st_length(wInt))
  total_length = sum(wInt$intersected_length)
}
lengths = lapply(1:nrow(buffers), function(i) getIntLength(buffers[i,], wells[ints[[i]],]))
lengths = do.call(c, lengths)
wells$offset_length = lengths
plot(wells[,'offset_length'])

longest_well_index = which.max(st_length(wells))
plot(wells[longest_well_index, 'geometry'])

buffer = st_buffer(wells[longest_well_index,], dist=5280)
plot(buffer$geometry)
plot(wells[longest_well_index,'geometry'], add=TRUE, col='red')

longest_well_coords = st_coordinates(wells[longest_well_index,])
max_x = max(longest_well_coords[,1])
min_x = longest_well_coords[which.max(longest_well_coords[,2]),1]
min_y = min(longest_well_coords[,2])
max_y = max(longest_well_coords[,2])
x = c(max_x + 660, min_x + 660, max_x+1320, min_x+1320, max_x+1980, min_x+1980)
y = c(min_y, max_y, min_y, max_y, min_y, max_y)
id = rep(1:(length(x)/2), each = 2)
coords = data.frame(x,y,id)
g = lapply(unique(id), function(i) st_linestring(as.matrix(coords[coords$id==i,c('x','y')])))
g = st_sf('geometry' = st_sfc(g))
st_crs(g) = st_crs(wells)
buffer = st_buffer(wells[longest_well_index,], dist=5280)
plot(buffer$geometry)
plot(wells[longest_well_index,'geometry'], add=TRUE, col='red')
plot(g$geometry, add=TRUE, col='purple')


g[,names(wells)[!(names(wells) %in% 'geometry')]]=NA
wells = sf:::rbind.sf(wells, g)

coords = matrix(x,y,ncol=2,byrow=FALSE)

ints = st_intersects(buffer, wells)
ints

plot(buffer$geometry)
plot(wells[ints[[1]], 'geometry'], add=TRUE, col='green')
plot(wells[longest_well_index,'geometry'], add=TRUE, col='red')

wellInts = st_intersection(buffer$geometry, wells[ints[[1]],])
plot(buffer$geometry)
plot(wells[ints[[1]], 'geometry'], add=TRUE, col='green')
plot(wells[longest_well_index,'geometry'], add=TRUE, col='red')
plot(wellInts$geometry, add=TRUE, col='blue')

junk=1
stuff=2