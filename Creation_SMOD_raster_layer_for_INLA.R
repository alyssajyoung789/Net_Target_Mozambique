#####################################
##-----------------------------------
##--SMOD (Settlement Model)
##-----------------------------------
#####################################

# https://ghsl.jrc.ec.europa.eu/download.php?ds=smod

SMOD.MOZ.1 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/SMODGHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_10.tif")
SMOD.MOZ.2 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_11.tif")
SMOD.MOZ.3 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_20_12.tif")
SMOD.MOZ.4 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_10.tif")
SMOD.MOZ.5 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_11.tif")
SMOD.MOZ.6 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_21_12.tif")
SMOD.MOZ.7 <- raster("C:/Users/admin/Box/WAH Work/Mozambique/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_22_10.tif")

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