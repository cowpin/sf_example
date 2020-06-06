install.packages('sf')
library(sf)

#Downloads the line shapefile from the cogcc website
link = 'https://cogcc.state.co.us/documents/data/downloads/gis/DIRECTIONAL_LINES_SHP.ZIP'
destination = paste0(getwd(), '/', basename(link))
download.file(url=link, destfile = destination, mode='wb')
unzip(destination, exdir=getwd())
unlink(destination)

#Downloads the county shapefile
link = 'https://www2.census.gov/geo/tiger/TIGER2016/COUSUB/tl_2016_08_cousub.zip'
destination = paste0(getwd(), '/', basename(link))
download.file(url=link, destfile = destination, mode='wb')
unzip(destination, exdir=getwd())
unlink(destination)

#Lets read in the shapefiles as sf objects
sticks = st_read('C:/Users/donal/Documents/R/sf example/Directional_Lines.shp')
counties = st_read('C:/Users/donal/Documents/R/sf example/tl_2016_08_cousub.shp')
