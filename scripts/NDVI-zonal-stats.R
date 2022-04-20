#import required libraries
library(sf)
library(raster)
library(exactextractr)

# for parallel processing
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
# always use one core less, that handles the overhead tasks like copying etc. Otherwise your PC might crash
UseCores <- detectCores() -1

#Register CoreCluster
# With makeCluster() and registerDoParallel() you register your cores for the parallelisation.
cl    <- makeCluster(UseCores)
registerDoParallel(cl)


#list files (in this case raster TIFFs)
grids <- list.files("D:/shared_vm/soil_moisture_Senegal/data/raw/wrsi/test/", pattern = "*.tif$")
grids
#check the number of files in the raster list (grids)
length <- length(grids)
length
#read-in the polygon shapefile
poly <- st_read("D:/shared_vm/soil_moisture_Senegal/data/shp/vul_poly/vuln.shp")

#create a raster stack
s <- stack(paste0("D:/shared_vm/soil_moisture_Senegal/data/raw/wrsi/test/", grids))
s

##### Use foreach loop and %dopar% command
# ****Notes***** -The syntax inside the brackets differ from the conventional for loop where you 
# would write for (i in 1:length(grids)), here you write foreach(i=1:length(stack_list)). 
# %dopar% which stand for "do parallel".
#In the end we close the parallelisation by calling stopCluster().

start <- Sys.time() # time the computation
#extract raster cell count (sum) within each polygon area (poly)
foreach(i=1:length(grids)) %dopar% {
  library(raster) 
  library(sf)
  ex <- extract(s, poly, fun=mean, na.rm=TRUE, df=TRUE)
  
  df <- data.frame(ex)
  # Add shapefile field from the polygon's name
  df$name=poly$Area
  
  #write to a CSV file
  write.csv(df, file = "D:/shared_vm/soil_moisture_Senegal/data/results/atest_wrsi_2007-2019.csv")
  
  # Calculate the time to execute the script
  end <- Sys.time()
  difftime(end,start)
}

#end cluster
stopCluster(cl)

