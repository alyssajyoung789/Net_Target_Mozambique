

# Mozambique- NET TARGET: extraction of population and RFE data, merging of datasets ------------------------------------------------
# Author: Alyssa Young
# Date Created: 8-2-20
# Date Last Modified: 8-21-2020
#  ------------------------------------------------------------------------

# Load packages
library("spDataLarge")
library(dplyr)
library(GADMTools)
library(tmap)
library(sf)
#library(exactextractr)
library(maptools)
library(raster)
library(sp)
library(readxl)
library(readr)
library(foreign)   ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(reshape2)  ; library(hablar)
library(tidyr)     
library (viridis)
library(data.table)
library(forecast)  ; library(MASS)
library(tseries)   ; library(scales)
library(tsModel)   ; library(extrafont)
library(lmtest)    ; library(tidyverse)
library(stargazer) ; library(RColorBrewer)
library(readxl)    ; library(olsrr)
library(Hmisc)
library(MASS)
library(ggplot2)
library(dplyr)
library(devEMF)
library(padr)
library(zoo)
library(tidyverse)
library(naniar)
library(GGally)
library(sf)
library(cartogram)
library(mgcv)
library(BAMMtools)
library(stringr)

require(raster)
require(rgdal)
require(sp)
require(data.table)
require(RCurl)
require(R.utils)
require(gdalUtils)
require(parallel)
library(stringr)

## get data GADM function not working for some reason- have manually dowloaded necessry shapefiles
## loading shapefiles and verying adminstrative unit levels
MZ_adm1 <- st_read(dsn= "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\Mozambique_adm1_2000-2020.shp")
MZ_adm2 <- st_read(dsn= "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\Mozambique_adm2_2000-2020.shp")
MZ_adm0 <- st_read (dsn= "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\gadm36_MOZ_0.shp")
plot (MZ_adm2) ### adminsitrative unit (district) at which we will develop prioritization scores, n=148
plot (MZ_adm1)

## setting district "shapefile"' as spatial dataframe
MZ_adm2.df <- as.data.frame(MZ_adm2) 
MZ_adm2.df <- MZ_adm2.df[-c(1)]

## loading COVID-19 case data (at provincial NOT district leve)
MOZ_MASTER <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\MOZ_MASTER_2.csv")

## merging COVID-19 case data to spatial data frame featuring province and district name
MZ_adm2.spdf <- merge(MZ_adm2, MOZ_MASTER, by="AREA_NAME", all = TRUE)
MZ_adm2.df2 <- as.data.frame(MZ_adm2.spdf)

## exporting new data set as excel file
library(writexl)
library(WriteXLS)
write_xlsx(MZ_adm2.spdf,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\MOZ_MASTER.xlsx")


## importing population density raster layer
## Source: Facebook Connectivity Lab and Center for International Earth Science Information Network - CIESIN - Columbia University. 2016. 
##High Resolution Settlement Layer (HRSL). Source imagery for HRSL © 2016 DigitalGlobe"

MOZ_ppp<- raster(x = "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\population_moz_2019-07-01.tif")
plot(MOZ_ppp)
#class      : RasterLayer 
#dimensions : 58944, 38208, 2252132352  (nrow, ncol, ncell)
#resolution : 0.0002777778, 0.0002777778  (x, y)
#extent     : 30.22403, 40.83736, -26.87069, -10.49736  (xmin, xmax, ymin, ymax)
#crs        : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
#source     : C:/Users/Alyssa/OneDrive/Desktop/Malaria Consortium/Mozambique data/population_moz_2019-07-01.tif 
#names      : population_moz_2019.07.01 

MOZ_dist_pop <- st_as_sf(MZ_adm2)
MOZ_dist_pop $sum_pop_density <- exact_extract(MOZ_ppp, MOZ_dist_pop, 'sum')
library("writexl")
write_xlsx(MOZ_dist_pop,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\MOZ_admin2_pop_2020.xlsx")

MOZ_pop <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\MOZ_admin2_pop_2020.csv")
## merging population data with master set
library (tidyverse)

# getting column names, renaming variables, dropping extraneous observation (airport) and joining sets
colnames (MOZ_pop)
names (MOZ_pop) [names(MOZ_pop) == "ï..Adm_2_Name"] <- "Adm_2_Name"
MOZ_pop <- MOZ_pop [-c (149)]
MOZ_MASTER2 <- left_join(MOZ_MASTER, MOZ_pop,by = c("Adm_2_Name" = "Adm_2_Name"))

## extracting monthly rainfall data
require("RCurl") 
if(interactive() && url.exists("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0//africa_monthly/tifs/")) {
  url = "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0//africa_monthly/tifs/"
  CHIRPSFilePaths = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  
  # Deal with newlines as \n or \r\n. (BDR)
  CHIRPSFilePaths = paste(url, strsplit(CHIRPSFilePaths, "\r*\n")[[1]], sep = "")
}
# Make a data table with the components of the path in the
CHIRPSFiles.dt <- data.table(FullPath = CHIRPSFilePaths)
CHIRPSFiles.dt[, filename := substr(FullPath, 76, 999)]
CHIRPSFiles.dt[, year := as.numeric(substr(FullPath, 89, 93)),]
CHIRPSFiles.dt[, month := as.numeric(substr(FullPath, 94, 96)),]


#CHIRPSFiles.dt[, pentad := as.numeric(substr(FullPath, 96, 96)),]
setkey(CHIRPSFiles.dt,year,month)


# Subset the data to only the month and years of interest (could go further and subset months within years)
CHIRPSFiles.dt <- subset(CHIRPSFiles.dt, year %in% 2015:2020 & month %in% 1:12)
CHIRPSFilePaths <- CHIRPSFiles.dt[,FullPath] 


# Function Unzip and read as rasters from any path on the CHIRPS FTP server
unzipToRaster <- function(path) {
  assign(paste(substr(path, 77, 999),"_temp", sep = ""), tempfile())
  download.file(path,paste(substr(path, 77, 999),"_temp", sep = ""))
  ras <- raster(gunzip(paste(substr(path, 77, 999),"_temp", sep = ""),substr(path, 77, 999), temporary = T, overwrite = T))
  unlink(paste(substr(path, 77, 999),"_temp", sep = ""))
  return(ras)
}
## Download

system.time({RFRasterList <- lapply(CHIRPSFilePaths, unzipToRaster)})

RFstack <- stack(RFRasterList)
plot (RFstack)

###Extracting mean rainfall by relevant months (November-April) to department (admin 1)
MOZ_RFE<- st_as_sf(MZ_adm2)

MOZ_RFE$mean_jul19_prec <- exact_extract(RFRasterList[[55]], MOZ_RFE, 'mean')
MOZ_RFE$mean_aug19_prec <- exact_extract(RFRasterList[[56]], MOZ_RFE, 'mean')
MOZ_RFE$mean_sep19_prec <- exact_extract(RFRasterList[[57]], MOZ_RFE, 'mean')
MOZ_RFE$mean_oct19_prec <- exact_extract(RFRasterList[[58]], MOZ_RFE, 'mean')
MOZ_RFE$mean_nov19_prec <- exact_extract(RFRasterList[[59]], MOZ_RFE, 'mean')
MOZ_RFE$mean_dec19_prec <- exact_extract(RFRasterList[[60]], MOZ_RFE, 'mean')
MOZ_RFE$mean_jan20_prec <- exact_extract(RFRasterList[[61]], MOZ_RFE, 'mean')
MOZ_RFE$mean_feb20_prec <- exact_extract(RFRasterList[[62]], MOZ_RFE, 'mean')
MOZ_RFE$mean_mar20_prec <- exact_extract(RFRasterList[[63]], MOZ_RFE, 'mean')
MOZ_RFE$mean_apr20_prec <- exact_extract(RFRasterList[[64]], MOZ_RFE, 'mean')
MOZ_RFE$mean_may20_prec <- exact_extract(RFRasterList[[65]], MOZ_RFE, 'mean')
MOZ_RFE$mean_jun20_prec <- exact_extract(RFRasterList[[66]], MOZ_RFE, 'mean')

write_xlsx(MOZ_RFE,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Mozambique data\\MOZ_RFE.xlsx")

##Mapping districts with populations that experienced disruptive events

##verifying variable class
class (MZ_adm2.spdf$Flooding_cyclone_reported_2020)
MZ_adm2.spdf$Flooding_cyclone_reported_2020_factor <- as.factor (MZ_adm2.spdf$Flooding_cyclone_reported_2020)
table(MZ_adm2.spdf$Flooding_cyclone_reported_2020_factor, useNA = "always")

tm_shape(MZ_adm0) + tm_polygons("GID_0") + 

MZ_adm2.spdf$Flooding_cyclone_reported_2020_factor <- as.factor(MZ_adm2.spdf$Flooding_cyclone_reported_2020)
  
tm_shape(MZ_adm2) + tm_fill(col="grey80") + tm_shape(MZ_adm2.spdf) + tm_fill("Flooding_cyclone_reported_2020", title = "Districts experiencing disruptive events", n=2, style = "cat", palette = "Blues") + 
  tm_scale_bar() + tm_compass(position = c("left","top")) +  tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

MOZ_HRP <-tm_shape(MZ_adm1) + tm_fill(col="grey80") + tm_shape(MZ_adm2.spdf) + tm_fill("Disruptive_event_any_2020", title = "HRP Districts", n=2, style = "cat",  palette = "Purples", labels = c("No reported HRPs", "HRP region")) + 
  tm_scale_bar() + tm_compass(position = c("left","top")) +  tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

tmap_save(MOZ_HRP, "MOZ HRP.png", width =6.78, height=5, units ='in', asp = 0)

