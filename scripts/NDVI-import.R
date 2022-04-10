#import NDVI
library(here)
library(raster)
library(tidyverse)
library(rgdal)
library(rasterVis)
library(lubridate)
library(dplyr)


#Load shp + use this to seclect polygon to plot
shp<- readOGR(here("data","shp","Djibouti-adm2.shp"))
inshp <- inshp[inshp$ADM2_NAME %in% c( "Randa","Tadjourah"), ]
plot(shp)

# list ndvi files
#files_in <- fs::dir_ls(here("data","ndvi"))
# create dataframe of the file path and get the filename from the path
#dates12 <- data.frame(layer = files_in)
#ndvi_file<- dates12 %>%
#mutate(rast_img = basename(files_in))

###-----Method 2
#TO avoid the following error add raster:: before the fn eg   raster::crop(s, raster::extent(cal_mask))
#Error in (function (classes, fdef, mtable)  :  unable to find an inherited method for function 'crop' for signature '"RasterStack"
flist <- list.files(here("data","ndvi"),full.names = FALSE,  pattern = "*.tif$")
head(flist)
flist_path <- list.files(here("data","ndvi"),full.names = TRUE,  pattern = "*.tif$")
head(flist_path)
# "DJ_MODAPE10_2002-07-21.tif" "DJ_MODAPE10_2002-08-01.tif" "DJ_MODAPE10_2002-08-11.tif"

# Convert file list to df and converT to date format
df_ndvi <- data.frame(img_path =flist_path,img_date = flist)
df_ndvi$date_img <- gsub("DJ_MODAPE10_", " ", df_ndvi$img_date)
df_ndvi$date_img <- gsub(".tif", " ", df_ndvi$date_img)
df_ndvi[['date_img']] <- as.POSIXct(df_ndvi[['date_img']],format = "%Y-%m-%d")


# Season definition Feb- October - Divided into 2 parts: Feb-June (main) & July - October.
# classify the data into seasons
df_ndvi_2 <- df_ndvi %>% 
  mutate(
    year = format(date_img, "%Y"),
    month = months(date_img, abbreviate = TRUE),
    day = format(date_img, "%d"),
    season_full = case_when(
      month %in% c('Feb','Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep','Oct') ~ 'feb_Oct'),
    season = case_when(
      month %in% c('Feb','Mar','Apr', 'May', 'Jun') ~ 'main season',
      month %in% c('Jul', 'Aug', 'Sep','Oct') ~ 'minor season'))
  #%>%
# filter(year == 2007 & month == "Jan" | month == "Feb" )
df_ndvi_2$year <- as.integer(df_ndvi_2$year)

###### Method 2
# temp_data %>% #   mutate( season = case_when(month %in%  9:11 ~ "Fall", month %in%  c(12, 1, 2)  ~ "Winter",
#       month %in%  3:5  ~ "Spring",  TRUE ~ "Summer"))

###### Create files List and raster stack for each year ###### 

######------------- Create files for each year------------ ###### 
#feb_jun_2002 <- df_ndvi_2 %>% filter(year == 2002 & season == "main season")
feb_jun_2003 <- df_ndvi_2 %>% filter(year == 2003 & season == "main season")
feb_jun_2004 <- df_ndvi_2 %>% filter(year == 2004 & season == "main season")
feb_jun_2005 <- df_ndvi_2 %>% filter(year == 2005 & season == "main season")
feb_jun_2006 <- df_ndvi_2 %>% filter(year == 2006 & season == "main season")
feb_jun_2007 <- df_ndvi_2 %>% filter(year == 2007 & season == "main season")
feb_jun_2008 <- df_ndvi_2 %>% filter(year == 2008 & season == "main season")
feb_jun_2009 <- df_ndvi_2 %>% filter(year == 2009 & season == "main season")
feb_jun_2010 <- df_ndvi_2 %>% filter(year == 2010 & season == "main season")
feb_jun_2011 <- df_ndvi_2 %>% filter(year == 2011 & season == "main season")
feb_jun_2012 <- df_ndvi_2 %>% filter(year == 2012 & season == "main season")
feb_jun_2013 <- df_ndvi_2 %>% filter(year == 2013 & season == "main season")
feb_jun_2014 <- df_ndvi_2 %>% filter(year == 2014 & season == "main season")
feb_jun_2015 <- df_ndvi_2 %>% filter(year == 2015 & season == "main season")
feb_jun_2016 <- df_ndvi_2 %>% filter(year == 2016 & season == "main season")
feb_jun_2017 <- df_ndvi_2 %>% filter(year == 2017 & season == "main season")
feb_jun_2018 <- df_ndvi_2 %>% filter(year == 2018 & season == "main season")
feb_jun_2019 <- df_ndvi_2 %>% filter(year == 2019 & season == "main season")
feb_jun_2020 <- df_ndvi_2 %>% filter(year == 2020 & season == "main season")
feb_jun_2021 <- df_ndvi_2 %>% filter(year == 2021 & season == "main season")

# ---------- Create raster stack-------------------
wd <- here("data","ndvi","/")

#r2002<- stack(paste0(wd, feb_jun_2002$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2003<- stack(paste0(wd, feb_jun_2003$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2004<- stack(paste0(wd, feb_jun_2004$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2005<- stack(paste0(wd, feb_jun_2005$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2006<- stack(paste0(wd, feb_jun_2006$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2007<- stack(paste0(wd, feb_jun_2007$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2008<- stack(paste0(wd, feb_jun_2008$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2009<- stack(paste0(wd, feb_jun_2009$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2010<- stack(paste0(wd, feb_jun_2010$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2011<- stack(paste0(wd, feb_jun_2011$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2012<- stack(paste0(wd, feb_jun_2012$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2013<- stack(paste0(wd, feb_jun_2013$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2014<- stack(paste0(wd, feb_jun_2014$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2015<- stack(paste0(wd, feb_jun_2015$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2016<- stack(paste0(wd, feb_jun_2016$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2017<- stack(paste0(wd, feb_jun_2017$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2018<- stack(paste0(wd, feb_jun_2018$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2019<- stack(paste0(wd, feb_jun_2019$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2020<- stack(paste0(wd, feb_jun_2020$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
r2021<- stack(paste0(wd, feb_jun_2021$img_date)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)

# TEST

# Seasonal NDVI raster stack ,  Set pixels outside the mask to null
season_ndvi <- stack(r2003,r2004,r2005,r2006, r2007,r2008,r2009,r2010,r2011,r2012,r2013,r2014,r2015,r2016,r2017,r2018,r2019,r2020,r2021)
#season_ndvi <- stack(r2002,r2003,r2004,r2005,r2006, r2007,r2008,r2009,r2010,r2011,r2012,r2013,r2014,r2015,r2016,r2017,r2018,r2019,r2020,r2021)
season_ndvi[season_ndvi > 0.1] <- NA
#new_names <- paste0("ndvi_", 2002:2021)
names(season_ndvi)
new_names <- paste0("ndvi_", 2003:2021)
names(season_ndvi) <-  new_names
#levelplot(season_ndvi)
summary(season_ndvi)

# Calculate average
season_avg <- max(season_ndvi,na.rm = TRUE)
season_avg[season_avg > 0.1]<- NA # Set pixels outside the mask to null

#-------------------------- print image stats
summary(season_ndvi)
summary(season_avg)

#-------------------- Save the seasonal avg and the longterm avg ndvi ------------------------#
#-------------------- Save the seasonal avg and the longterm avg ndvi ------------------------#
raster::writeRaster(season_avg, filename= here("out", "tif","NDVI_max_Feb_Jun_2003_2021.tif"), format="GTiff", bylayer=TRUE, overwrite=TRUE)
# Save raster brick
ndvi_filenames <- paste0(new_names,".tif")
for(i in 1:length(ndvi_filenames)) {
  single_band <- raster(season_ndvi, layer = i)
  writeRaster(single_band,ndvi_filenames[i], overwrite=TRUE)
}

# Method 2 Use the purrr map method
library(purrr)
ndvi_filenames <- paste0(new_names,".tif")
layers_list <- map(1:length(ndvi_filenames), ~raster(season_ndvi, layer = .))
# use walk2 rather than map2 because we don't want the output of writeRaster
walk2(layers_list, ndvi_filenames, writeRaster,overwrite=TRUE)


ndvi_major_minor_season <-split(df_ndvi_2,list(df_ndvi_2$year,df_ndvi_2$season, df_ndvi_2$img_path))
head(ndvi_major_minor_season)
ndvi_full_season <-split(df_ndvi_2,list(df_ndvi_2$year,df_ndvi_2$season_full, df_ndvi_2$img_path))

linelist_split <- df_ndvi_2 %>% 
  group_split(year,season_full)
linelist_split[2]

# extract group_keys() as a dataframe
groupings <- df_ndvi_2 %>% 
  group_by(year, season_full) %>%       
  group_keys()

# Combine into one name value 
names(linelist_split) <- groupings %>% 
  mutate(across(everything(), replace_na, "Missing")) %>%  # replace NA with "Missing" in all columns
  unite("combined", sep = "-") %>%                         # Unite all column values into one
  setNames(NULL) %>% 
  as_vector() %>% 
  as.list()
groupings      # show unique groupings

## Export as Excel sheets
linelist_split %>% 
  writexl::write_xlsx(path = here("out", "File_path_df.xlsx"))

########----------- Filter the list------------
z=linelist_split %>% lapply(., "[", , 'img_path')
names(z)[lapply(z, names) == "2006-Missing"]




l[lapply(l, length) > 0]



#--------------Get the colum with the img path---------------------------
z=linelist_split %>% lapply(., "[", , 'img_path')
z
  #$`2022-feb_Oct`
# A tibble: 4 x 1
# img_path                                                                                                        
# <chr>                                                                                                           
#   1 C:/Users/Farai.marumbwa/OneDrive - World Food Programme/ARC_WORK/TASKS-TSD and RD/NDVI_analysis_Djibouti/data/n~
#   2 C:/Users/Farai.marumbwa/OneDrive - World Food Programme/ARC_WORK/TASKS-TSD and RD/NDVI_analysis_Djibouti/data/n~
#   $`2022-Missing`
# # A tibble: 3 x 1
# img_path                                                                                                        
# <chr>                                                                                                           
#   1 C:/Users/Farai.marumbwa/OneDrive - World Food Programme/ARC_WORK/TASKS-TSD and RD/NDVI_analysis_Djibouti/data/n~

#--------------create raster stack based on the new list---------------------------
stack(z[[1]][[1]])
# class      : RasterStack 
# dimensions : 301, 301, 90601, 10  (nrow, ncol, ncell, nlayers)
# resolution : 0.01, 0.01  (x, y)
# extent     : 40.99, 44, 9.99, 13  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
#ap(rnorm, n = 10)
# names      : DJ_MODAPE10_2002.07.21, DJ_MODAPE10_2002.08.01, DJ_MODAPE10_2002.08.11, DJ_MODAPE10_2002.08.21, DJ_MODAPE10_2002.09.01, DJ_MODAPE10_2002.09.11, DJ_MODAPE10_2002.09.21, DJ_MODAPE10_2002.10.01, DJ_MODAPE10_2002.10.11, DJ_MODAPE10_2002.10.21 

# --------only save one raster
b=list()
i=1
for (i in 1:20){
 b= stack(z[[i]][[i]])
 i=i+1
}
#-- this only saves the first raster stack
bb=lapply(z, function(x) { x["ind"] <- NULL; x })%>%
 
  
bb%>% map(function(x) stack(x))


addTen <- function(.x) {
  return(stack(.x))
}
addTen(bb)

library(tidyverse)
map(.x = bb, 
    .f = addTen)

#The map() function below iterates addTen() across all entries of the vector, .x = c(1, 4, 7), and returns the output as a list

library(tidyverse)
map(.x = c(1, 4, 7), 
    .f = addTen)


#------------------ Select the first 27 items of the first col
a=lapply(linelist_split, "[", 1:27, 1)
b=as.data.frame(a)  
write.csv(b,file="b.csv")
#----------Convert to df-------------
#purrr::map_df(list_of_lists, tibble::as_tibble)



#create a raster stack and mask and calculate seasonal NDVI
# calculate the seasonal totals #sum_2021 <- calc(r2021, function(x) sum(x, na.rm = TRUE))
wd<-paste0(here("data","ndvi"),"/")
f_2007 <- df_ndvi_2 %>% filter(year == 2007 & season== "main season" )
r2007v<- stack(paste0(wd, f_2007$img)) %>% raster::crop(raster::extent(inshp)) %>% raster::mask(shp) %>% mean(na.rm = TRUE)
plot(r2007v)

f_2010 <- df_ndvi_2 %>% filter(year == 2010 & season== "main season" )

# Function to filter and create raster stack from df / split raster into list and create stack
stack_main_season <- function(yr, seas){
  f_2010 <- df_ndvi_2 %>% filter(year == yr & season== seas )
  rstack <- stack(f_2010$img)
  return(rstack)
}
a=stack_main_season(2007, "main season")
plot(a)






# Processing list
paste0(wd,ndvi_full_season[[5]]$img)

 for (i in 1:length(ndvi_full_season)){
   print(ndvi_full_season[i])
    }

###- attempt 1 only saving the
tstList <- list()
tstStack <- stack()
i <- 1
for (i in 1:length(ndvi_full_season)){
  sa <- ndvi_full_season[[i]]$img
  tstStack <- stack(sa)
  tstList[[i]] <- tstStack
  i <- i+1  
}
#----------- Plot image from list
z=tstList[[7]] %>% raster::crop(raster::extent(inshp)) %>%
            raster::mask(shp) %>% sum(na.rm = TRUE) %>% plot()
                          
mapply(plot,tstList)








#-----------------Attempt 2----- working - need to conver this to brick------------
stack.list <- list()
for (i in 1:length(ndvi_full_season)){
  s <- ndvi_full_season[[i]]$img
  print(s)
  stack.list[[i]] <- stack(s)
}



#---------@Examples@
stack.list <- list()
for (i in 1:length(list_dirs)){
  s <- list.files(path=list_dirs[i], pattern = "cool", recursive=F, full.names = TRUE)
  stack.list[[i]] <- stack(s)
}

stack.list <- list()
for (y in 2000:2019){
  for (m in 1:12){
    # paste0(y,"_",sprintf("%02d", m)) returnes YEAR_MONTH
    tstStack <- stack(mrlist[grep(paste0(y,"_",sprintf("%02d", m)), names(mrlist))] )
    tstList[[i]] <- tstStack
    i <- i+1  
  } 
}


desired_length <- 10 # or whatever length you want
empty_vec <- rep(NA, desired_length)


# 2. stack rasters of the same month
tstList <- list()
tstStack <- stack()
i <- 1
for (y in 2000:2019){
  for (m in 1:12){
    # paste0(y,"_",sprintf("%02d", m)) returnes YEAR_MONTH
    tstStack <- stack(mrlist[grep(paste0(y,"_",sprintf("%02d", m)), names(mrlist))] )
    tstList[[i]] <- tstStack
    i <- i+1  
  } 
}









r2008<- stack(paste0(wd, f_2008$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2009<- stack(paste0(wd, f_2009$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2010<- stack(paste0(wd, f_2010$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2011<- stack(paste0(wd, f_2011$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2012<- stack(paste0(wd, f_2012$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2013<- stack(paste0(wd, f_2013$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2014<- stack(paste0(wd, f_2014$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2015<- stack(paste0(wd, f_2015$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2016<- stack(paste0(wd, f_2016$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2017<- stack(paste0(wd, f_2017$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2018<- stack(paste0(wd, f_2018$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2019<- stack(paste0(wd, f_2019$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
r2020<- stack(paste0(wd, f_2020$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)
#r2021<- stack(paste0(wd, f_2021$layer)) %>% raster::crop(raster::extent(shp)) %>% raster::mask(shp) %>% sum(na.rm = TRUE)

# Seasonal totals raster stack ,  Set pixels outside the mask to null
season_totals <- stack(r2001,r2002,r2003,r2004,r2005,r2006, r2007,r2008,r2009,r2010,r2011,r2012,r2013,r2014,r2015,r2016,r2017,r2018,r2019,r2020)
season_totals[season_totals < 1] <- NA
new_names <- paste0("RFE2_", 2001:2020)
names(season_totals) <-  new_names
levelplot(season_totals)

# Calculate average
season_avg <- mean(season_totals,na.rm = TRUE)
season_avg[season_avg < 1]<- NA # Set pixels outside the mask to null
summary(season_avg)

# Save the seasonal totals and the longterm avg rainfall
# raster::writeRaster(season_avg, filename="D:/temp/Biniam_rainfall/arc2/ARC2_avg_2001_2020.tif", format="GTiff", bylayer=TRUE, overwrite=TRUE)
# raster::writeRaster(season_totals, filename="D:/temp/Biniam_rainfall/arc2/ARC2_sum.tif", format="GTiff", bylayer=TRUE, overwrite=TRUE)

raster::writeRaster(season_avg, filename="D:/temp/Biniam_rainfall/rfe2/RFE2_avg_2001_2020.tif", format="GTiff", bylayer=TRUE, overwrite=TRUE)
raster::writeRaster(season_totals, filename="D:/temp/Biniam_rainfall/rfe2/RFE2_sum.tif", format="GTiff", bylayer=TRUE, overwrite=TRUE)



