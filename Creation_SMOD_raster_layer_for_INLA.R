#####################################
##-----------------------------------
##--SMOD (Settlement Model)
##-----------------------------------
#####################################

# https://ghsl.jrc.ec.europa.eu/download.php?ds=smod

SMOD.MOZ.1 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_10.tif")
SMOD.MOZ.2 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_11.tif")
SMOD.MOZ.3 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_12.tif")
SMOD.MOZ.4 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_10.tif")
SMOD.MOZ.5 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_11.tif")
SMOD.MOZ.6 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_12.tif")
SMOD.MOZ.7 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMOD/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_22_10.tif")

SMOD.MOZ.raster <- raster::mosaic(SMOD.MOZ.1, SMOD.MOZ.2, SMOD.MOZ.3, SMOD.MOZ.4, SMOD.MOZ.5, SMOD.MOZ.6, SMOD.MOZ.7, fun=mean)
rm(SMOD.MOZ.1, SMOD.MOZ.2, SMOD.MOZ.3, SMOD.MOZ.4, SMOD.MOZ.5, SMOD.MOZ.6, SMOD.MOZ.7)

newproj <- "+proj=longlat +datum=WGS84"
SMOD.MOZ.raster <- projectRaster(SMOD.MOZ.raster, crs=newproj)

##--mask and crop
SMOD.MOZ.raster <- mask(x = SMOD.MOZ.raster, mask = adm1.spdf)
SMOD.MOZ.raster <- crop(x = SMOD.MOZ.raster, y = extent(adm1.spdf))

# plot raster layer for built population 
p <- malariaAtlas::autoplot_MAPraster(SMOD.MOZ.raster,shp_df=adm1.spdf, printed=F)
full_plot <- p[[1]] + 
    scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0,start=1.5,r=-1.0,hue=1.5,n=16)), 
                         name="Urbanicity") + 
    ggtitle("SMOD") +
    theme(axis.text=element_blank(),
          panel.border=element_rect(fill=NA, color="white"))
print(full_plot)
ggsave(filename ="C:/Users/Alyssa/OneDrive/Desktop/Malaria Consortium/Mozambique data/INLA Covariate Layers/SMOD.png", width =13.35, height=7.5, units ='in', dpi =320)
rm(p, full_plot)

#---------------------------------------------------------------------------#
# extract raster value by DHS cluster points (2km buffer for urban / 10km buffer for rural)
rasValue.U<-extract(SMOD.MOZ.raster,Urban.MOZ_clust.df,buffer=2000,fun=mean,na.rm=TRUE)
rasValue.R<-extract(SMOD.MOZ.raster,Rural.MOZ_clust.df,buffer=10000,fun=mean,na.rm=TRUE)

# combine raster values with DHS cluster points and class as data.frame
combinePointValue.U=as.data.frame(cbind(Urban.MOZ_clust.df,rasValue.U))
combinePointValue.R=as.data.frame(cbind(Rural.MOZ_clust.df,rasValue.R))

U.SMOD<-as.data.frame(combinePointValue.U[,c(4,11,20:22)])
R.SMOD<-as.data.frame(combinePointValue.R[,c(4,11,20:22)])

# set column name
colnames(U.SMOD)[3] <- "SMOD_2015"
colnames(R.SMOD)[3] <- "SMOD_2015"

# rbind urban/rural data frames for each survey year
SMOD_2015<-rbind(U.SMOD,R.SMOD)

##--snap SMOD.NGA.raster to same dim/res/extent as ImpHsng_2015.raster
#SMOD_2015.raster <- projectRaster(SMOD.NGA.raster, ImpHsng_2015.raster, method='bilinear')

# remove objects that won't be saved
# retain objects needed for creation of subsequent covariate raster layers
rm(newproj,combinePointValue.R,combinePointValue.U,R.SMOD,rasValue.R,rasValue.U,U.SMOD)

#####################################
##-----------------------------------
##--Educational attainment for women of reproductive age
##-----------------------------------
#####################################

# http://ghdx.healthdata.org/record/ihme-data/africa-educational-attainment-geospatial-estimates-2000-2015

# load educational attainment file as raster layer: dowloaded mean education attainment for years 2014-2018, women of reproductive age 
EducAttnmnt_2014_2018.raster <- raster("C:/Users/Alyssa/OneDrive/Desktop/Malaria Consortium/Mozambique data/INLA Covariate Layers/ED_AT_F_15_49.TIF")
EducAttnmnt_2014_2018.raster <- mask(x = EducAttnmnt_2014_2018.raster, mask = adm1.spdf) # mask
EducAttnmnt_2014_2018.raster <- crop(x = EducAttnmnt_2014_2018.raster, y = extent(adm1.spdf)) # crop

# plot raster layer for Educational Attainment 
p <- malariaAtlas::autoplot_MAPraster(EducAttnmnt_2014_2018.raster,shp_df=adm1.spdf, printed=F)
full_plot <- p[[1]] + 
    scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0,start=1.5,r=-1.0,hue=1.5,n=16)), 
                         name="Mean Years of Education") + 
    ggtitle("Educational Attainment: Women 15-49yrs") +
    theme(axis.text=element_blank(),
          panel.border=element_rect(fill=NA, color="white"))
print(full_plot)
ggsave(filename ="C:/Users/Alyssa/OneDrive/Desktop/Malaria Consortium/Mozambique data/INLA Covariate Layers/EducAttnmnt.png", width =13.35, height=7.5, units ='in', dpi =320)
rm(p, full_plot)

#---------------------------------------------------------------------------#
# extract raster value by DHS cluster points (2km buffer for urban / 10km buffer for rural)
rasValue.U<-extract(EducAttnmnt_2014_2018.raster,Urban.MOZ_clust.df,buffer=2000,fun=mean,na.rm=TRUE)
rasValue.R<-extract(EducAttnmnt_2014_2018.raster,Rural.MOZ_clust.df,buffer=10000,fun=mean,na.rm=TRUE)

# combine raster values with DHS cluster points and class as data.frame
combinePointValue.U=as.data.frame(cbind(Urban.MOZ_clust.df,rasValue.U))
combinePointValue.R=as.data.frame(cbind(Rural.MOZ_clust.df,rasValue.R))

U.EducAttnmnt<-as.data.frame(combinePointValue.U[,c(4,11,20:22)])
R.EducAttnmnt<-as.data.frame(combinePointValue.R[,c(4,11,20:22)])

# set column name
colnames(U.EducAttnmnt)[3] <- "EducAttnmnt_2014_2018"
colnames(R.EducAttnmnt)[3] <- "EducAttnmnt_2014_2018"

# rbind urban/rural data frames for each survey year
EducAttnmnt_2014<-rbind(U.EducAttnmnt,R.EducAttnmnt)

##--snap EducAttnmnt_2014.raster to same dim/res/extent as ImpHsng_2015.raster
#EducAttnmnt_2014.raster <- projectRaster(EducAttnmnt_2014.raster, ImpHsng_2015.raster, method='bilinear')

# remove objects that won't be saved
# retain objects needed for creation of subsequent covariate raster layers
rm(combinePointValue.R,combinePointValue.U,R.EducAttnmnt,rasValue.R,rasValue.U,U.EducAttnmnt)
