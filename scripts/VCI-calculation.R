#import required libraries
library(sf)
library(raster)
library(here)
library(exactextractr)

#list files (in this case raster TIFFs)
grids <- list.files(here("out","tif","ndvi-Feb-June"), pattern =
                      "*.tif$")
grids
#check the number of files in the raster list (grids)
length <- length(grids)
length
#read-in the polygon shapefile
poly<- st_read(here("data","shp","Djibouti-adm2.shp"))

#create a raster stack
s <- stack(paste0(here("out","tif","ndvi-Feb-June","/"), grids))
s
#------------cALCULATE MIN-MAX STATS
min_ndvi<- s %>% min(na.rm = TRUE)
plot(min_ndvi)
max_ndvi<- s %>% max(na.rm = TRUE)
plot(max_ndvi)

summary(max_ndvi)
summary(min_ndvi)

#-------------- VCI Calculation--------------------
vci_stack= ((s-min_ndvi)/(max_ndvi-min_ndvi))*100
names(vci_stack)
new_names <- paste0("vci_", 2003:2021)
names(vci_stack) <-  new_names
plot(vci_stack<10)
a<-vci_stack<35
writeRaster(a[[1]], 'vci_22less10.tif')

#----------------- Extract VCI -----------
start <- Sys.time() # time the computation
#extract raster cell count (sum) within each polygon area (poly)
for (i in 1:length(grids)){
  ex <- raster::extract(vci_stack, poly, fun=mean, na.rm=TRUE, df=TRUE)
}
# Calc time to execute
end <- Sys.time()
difftime(end,start)

#write to a data frame
df <- data.frame(ex)
# Add shapefile field from the polygon's name
df$name=poly$ADM2_NAME

#write to a CSV file
write.csv(df, file = "VCI_zonal stats.csv")

