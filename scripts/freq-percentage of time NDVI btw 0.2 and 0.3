library(raster)
# create wrsi raster stack
wrsi_path <- here('data','ndvi')
wrsi_list <- list.files(wrsi_path,full.names = FALSE,pattern = "*.tif$")
wrsi_stack <- stack(paste0(path = wrsi_path, wrsi_list))
names(wrsi_stack)
names(wrsi_stack)=gsub("wrsi_","w_",names(wrsi_stack))
names(wrsi_stack)

# Replace values >= 0 and <= 10 to 200 
# (Does not need to be 100. A very large number above max wrsi vals should be good)
season_ndvi_freq <- stack(r2003,r2004,r2005,r2006, r2007,r2008,r2009,r2010,r2011,r2012,r2013,r2014,r2015,r2016,r2017,r2018,r2019,r2020,r2021)
#
season_ndvi_freq[season_ndvi_freq >= 0.1 & season_ndvi_freq <= 0.2] <- 200
plot(season_ndvi_freq)
# Replace other values to 0
season_ndvi_freq[season_ndvi_freq < 200] <- 0
plot(season_ndvi_freq)
# Replace 200 to 1
season_ndvi_freq[season_ndvi_freq == 200] <- 1
plot(season_ndvi_freq)

# Calculate the percentage of severe wet
wrsi_stack_perc <- mean(season_ndvi_freq) * 100
plot(wrsi_stack_perc, main = "Frequency of WRSI<10 (%)")
summary(wrsi_stack_perc)

# Save the results
writeRaster(wrsi_stack_perc, filename = "D:/shared_vm/soil_moisture_Senegal/data/results/Perc_time_wrsi_less_10.tif", overwrite = TRUE)




#Create a data frame from the raster values
df_wrsi_ras <- cbind.data.frame(values(wrsi_stack))
df_wrsi_ras
# Remove rows with N/As
df_wrsi_ras<-df_wrsi_ras[complete.cases(df_wrsi_ras), ]

write.csv(df_wrsi_ras, file = "D:/shared_vm/soil_moisture_Senegal/data/results/test_dry_spell.csv")

