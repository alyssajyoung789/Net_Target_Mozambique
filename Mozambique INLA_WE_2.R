##--------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------##
##--Malaria Consortium - Mozambique INLA-SPDE
##--Matt Worges (original author)
##--Modifed by Will Eaton
##--10.7.2020
##--------------------------------------------------------------------------------##
##--------------------------------------------------------------------------------##

lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
library(readstata13)
library(geoR)
library(dplyr)
library(sp)
library(rgdal)
library(leaflet)
library(viridis)
library(sf)
library(INLA)
library(tmap)
library(data.table)
library(regclass)
library(exactextractr)
library(writexl)
require(raster)
library(foreign)

# read in 2018 Uganda MIS Stata or Excel file
# DHS2018 <- readstata13::read.dta13("D:/TRMD Files/Nigeria INLA/Data/NigeriaDHS_2018.dta")
# DHS2018 <- readstata13::read.dta13("C:/Users/admin/Box/WAH Work/Mozambique/DHS/MZHR7AFL.dta")
# input Stata file
DHS2018 <- read.dta("C:/Users/admin/Box/WAH Work/Mozambique/DHS/MZHR7AFL.dta")
# DHS2018_xls <- read_excel("C:/Users/mworges/Desktop/Nigeria/NGA_DHS_2018.xlsx")
# DHS2018<-DHS2018_xls

# read in covariate raster layers
load("C:/Users/admin/Box/WAH Work/Mozambique/Alyssa Repository/Net_Target_Mozambique/INLA/MOZ_environment.RData")

##--create columns for households that own at least 1 net
##--hv227 = HH has bed net for sleeping
DHS2018$atleast1Net[DHS2018$hv227=="yes"]<-1
DHS2018$atleast1Net[DHS2018$hv227=="no"]<-0
unique(DHS2018$atleast1Net)
table(DHS2018$hv227, DHS2018$atleast1Net)

##--get the total number of reported bed nets in the HH
#**************************************************************************
# Question for Matt and Alyssa: do I need to change "hmlidx_1" or "..._2"?
# confirm: Is this a stata variable name
# *************************************************************************
options(max.print=2000)
grep("hmlidx_1", colnames(DHS2018)) #column index 1809 # WE: note I obtain value "1809" when running this line
grep("hmlidx_7", colnames(DHS2018)) #column index 1815 # WE: note I obtain value "1815" when running this line
DHS2018$NetCount <- apply(!is.na(DHS2018[c(1809:1815)]), 1, sum) #sum integers across columns
DHS2018$NetCountNA <- apply(is.na(DHS2018[c(1809:1815)]), 1, sum) #sum NAs across columns

sapply(list(DHS2018$hmlidx_1,DHS2018$hmlidx_2,DHS2018$hmlidx_3,DHS2018$hmlidx_4,DHS2018$hmlidx_5,DHS2018$hmlidx_6,DHS2018$hmlidx_7,DHS2018$NetCount,DHS2018$NetCountNA,DHS2018$atleast1Net), head, n = 10)

##--number of HHs with at least one net per every two people (net to person ratio >= 0.5)
DHS2018$NetPerRatio<-DHS2018$NetCount/DHS2018$hv009 #ratio
DHS2018$NetperTwoPpl[DHS2018$NetPerRatio>=0.5] <- 1
DHS2018$NetperTwoPpl[DHS2018$NetPerRatio>=0 & DHS2018$NetPerRatio<0.5] <- 0

sapply(list(DHS2018$NetCount,DHS2018$hv009,DHS2018$NetPerRatio,DHS2018$NetperTwoPpl), head, n=10)

# Test comment

##--get the total number of nets that were **used** in the HH
##--hml21_X = did someone sleep under the bed net last night
DHS2018$netuse_1[DHS2018$hml21_1=="yes"]<-1
DHS2018$netuse_2[DHS2018$hml21_2=="yes"]<-1
DHS2018$netuse_3[DHS2018$hml21_3=="yes"]<-1
DHS2018$netuse_4[DHS2018$hml21_4=="yes"]<-1
DHS2018$netuse_5[DHS2018$hml21_5=="yes"]<-1
DHS2018$netuse_6[DHS2018$hml21_6=="yes"]<-1
DHS2018$netuse_7[DHS2018$hml21_7=="yes"]<-1

DHS2018$netuse_1[DHS2018$hml21_1=="no"]<-0
DHS2018$netuse_2[DHS2018$hml21_2=="no"]<-0
DHS2018$netuse_3[DHS2018$hml21_3=="no"]<-0
DHS2018$netuse_4[DHS2018$hml21_4=="no"]<-0
DHS2018$netuse_5[DHS2018$hml21_5=="no"]<-0
DHS2018$netuse_6[DHS2018$hml21_6=="no"]<-0
DHS2018$netuse_7[DHS2018$hml21_7=="no"]<-0

DHS2018$net_used <- rowSums(subset(DHS2018, select = c(paste0("netuse_",1:7))), na.rm = T)

##--any bed net use
DHS2018$AnyNetUse[DHS2018$atleast1Net==1 & DHS2018$net_used>0]<-1
DHS2018$AnyNetUse[!is.na(DHS2018$NetCount) & DHS2018$net_used==0]<-0

sapply(list(DHS2018$atleast1Net,DHS2018$NetCount,DHS2018$NetCountNA,DHS2018$net_used,DHS2018$AnyNetUse), head, n = 20)

# Population access to ITNs
# Numerator: De facto household population who could sleep under an ITN if each ITN in # the household were used by up to two people
# Denominator: De facto household population
DHS2018$hv013[DHS2018$hv013==0]<-DHS2018$hv009
table(DHS2018$hv013)

DHS2018$Num_Net<-DHS2018$NetCount*2
DHS2018$PopAccess<-DHS2018$Num_Net/DHS2018$hv013
DHS2018$PopAccess[DHS2018$PopAccess>1]<-1
summary(DHS2018$PopAccess)

sapply(list(DHS2018$Num_Net,DHS2018$hv013,DHS2018$PopAccess), head, n = 100)

DHS2018$PopAccess_binary[DHS2018$PopAccess==1]<-1
DHS2018$PopAccess_binary[DHS2018$PopAccess<1]<-0
table(DHS2018$PopAccess_binary)

#--------------------------------------------------------------------------#
# get ADM0 boundaries (for clipping when using tmap)
adm0_NGA <- sf::st_read(dsn ="C:/Users/mworges/Desktop/Nigeria/ShapeFiles/nga_admbnda_adm0_osgof_20190417.shp")

# get DHS geospatial covariates shape file
GPS_2018 <- st_read(dsn ="C:/Users/mworges/Desktop/Nigeria/NGGE7AFL.shp")
GPS_2018 <- GPS_2018[!(GPS_2018$LATNUM==0),] # remove records

##--merge data frames with raster-to-buffer values
GPS_2018 <- merge(GPS_2018,TravTime[,c("DHSCLUST","TravTime")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

GPS_2018 <- merge(GPS_2018,Population_2015[,c("DHSCLUST","Population_2015")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

GPS_2018 <- merge(GPS_2018,Built_2014[,c("DHSCLUST","Built_2014")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

GPS_2018 <- merge(GPS_2018,ImpHsng_2015[,c("DHSCLUST","ImpHsng_2015")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

GPS_2018 <- merge(GPS_2018,SMOD_2015[,c("DHSCLUST","SMOD_2015")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

GPS_2018 <- merge(GPS_2018,EducAttnmnt_2014[,c("DHSCLUST","EducAttnmnt_2014")],by.x="DHSCLUST",by.y="DHSCLUST",all.x = TRUE)

# merge MIS columns of interest with geospatial covariates shape file
GPS_2018 <- merge(GPS_2018,DHS2018[,c("hv021","AnyNetUse","atleast1Net","NetperTwoPpl","PopAccess_binary","PopAccess")],by.x="DHSCLUST",by.y="hv021",all.x = TRUE)

# set as data frame
GPS_2018.df <- as.data.frame(GPS_2018)

#------------------------------------------------------------------------------------------------
# assessing multi-collinearity between potential fixed effect covariates
# not survey set!
# any net use
colnames(GPS_2018.df)
AnyNetUse<-glm(AnyNetUse~TravTime+SMOD_2015+EducAttnmnt_2014,data = GPS_2018.df, family = binomial)

summary(AnyNetUse)
exp(cbind(OR = coef(AnyNetUse), confint(AnyNetUse)))

VIF_2018<-as.data.table(VIF(AnyNetUse))
VIF_2018$VIFequivalent<-(VIF_2018$V1)^2
VIF_2018

# net per 2 people
NetperTwoPpl<-glm(NetperTwoPpl~TravTime+SMOD_2015+EducAttnmnt_2014,data = GPS_2018.df, family = binomial)

summary(NetperTwoPpl)
exp(cbind(OR = coef(NetperTwoPpl), confint(NetperTwoPpl)))

VIF_2018<-as.data.table(VIF(NetperTwoPpl))
VIF_2018$VIFequivalent<-(VIF_2018$V1)^2
VIF_2018
#------------------------------------------------------------------------------------------------

# summarize by GPS points (clusters) - roll up net use column to proportions at clusters
GPS.2018.tbl <- group_by(GPS_2018.df, LONGNUM, LATNUM) %>%
  summarize(
    total = n(),
    PopAccCnt = sum(PopAccess_binary),
    NetUse = sum(AnyNetUse),
    NetAvlblty = sum(NetperTwoPpl),
    prev_PopAccCnt = PopAccCnt / total,
    PopAcc = mean(PopAccess),
    prev_NU = NetUse / total,
    prev_NA = NetAvlblty / total,
    SMOD_2015 = mean(SMOD_2015),
    TravTime = mean(TravTime),
    EducAttnmnt_2014 = mean(EducAttnmnt_2014))

GPS.2018.tbl <- GPS.2018.tbl[complete.cases(GPS.2018.tbl[ , 1:12]),]

##--set raster layers to 5km by 5km
SMOD_2015.raster <- resample(SMOD_2015.raster, TravTime.raster, method='ngb')
SMOD.agg <- aggregate(SMOD_2015.raster, fact = 5, fun = mean, na.rm=TRUE)
TravTime.agg <- aggregate(TravTime.raster, fact = 5, fun = mean, na.rm=TRUE)
EducAttn.agg <- resample(EducAttnmnt_2014.raster, TravTime.agg, method='bilinear')

##--mask and crop raster files to specific region
SMOD.agg <- mask(x = SMOD.agg, mask = adm0_NGA)
SMOD.agg <- crop(x = SMOD.agg, y = extent(adm0_NGA))
TravTime.agg <- mask(x = TravTime.agg, mask = adm0_NGA)
TravTime.agg <- crop(x = TravTime.agg, y = extent(adm0_NGA))
EducAttn.agg <- mask(x = EducAttn.agg, mask = adm0_NGA)
EducAttn.agg <- crop(x = EducAttn.agg, y = extent(adm0_NGA))

##--set raster up to have coordinates alongside values in an SPDF
SMOD_2015.spdf <- rasterToPoints(SMOD.agg, spatial=TRUE)
TravTime.spdf <- rasterToPoints(TravTime.agg, spatial=TRUE)
EducAttnmnt_2014.spdf <- rasterToPoints(EducAttn.agg, spatial=TRUE)

##--reproject sp object
geo.prj <- "+proj=longlat +datum=WGS84" 
SMOD_2015.spdf <- spTransform(SMOD_2015.spdf, CRS(geo.prj))
TravTime.spdf <- spTransform(TravTime.spdf, CRS(geo.prj))
EducAttnmnt_2014.spdf <- spTransform(EducAttnmnt_2014.spdf, CRS(geo.prj))

##--assign coordinates to @data slot
SMOD_2015.spdf@data <- data.frame(SMOD_2015.spdf@data,
                                  long=coordinates(SMOD_2015.spdf)[,1],
                                  lat=coordinates(SMOD_2015.spdf)[,2])
TravTime.spdf@data <- data.frame(TravTime.spdf@data,
                                     long=coordinates(TravTime.spdf)[,1],
                                     lat=coordinates(TravTime.spdf)[,2])
EducAttnmnt_2014.spdf@data <- data.frame(EducAttnmnt_2014.spdf@data,
                                long=coordinates(EducAttnmnt_2014.spdf)[,1],
                                lat=coordinates(EducAttnmnt_2014.spdf)[,2])

##--check length of each transformed raster file
length(SMOD_2015.spdf)
length(TravTime.spdf)
length(EducAttnmnt_2014.spdf)

##--convert SPDF to data frame
A1 <- as.data.frame(SMOD_2015.spdf)
A2 <- as.data.frame(TravTime.spdf)
A3 <- as.data.frame(EducAttnmnt_2014.spdf)

##--create unique ID by concatenating lat/long values
A1$ID <- paste0(A1$lat,A1$long)
length(unique(A1$ID))
A2$ID <- paste0(A2$lat,A2$long)
length(unique(A2$ID))
A3$ID <- paste0(A3$lat,A3$long)
length(unique(A3$ID))

##--merge the covariate data frames into a single file
Covs_2018 <- merge(A3,A1[,c("ID","layer")],by.x="ID",by.y="ID",all.x = TRUE)
Covs_2018 <- merge(Covs_2018,A2[,c("ID","layer")],by.x="ID",by.y="ID",all.x = TRUE)

##--remove unnecessary columns
Covs_2018<-Covs_2018[,-c(1,5,6)]

##--rename column headings to match existing variable names
colnames(Covs_2018)[colnames(Covs_2018)=="layer.x"] <- "SMOD_2015"
colnames(Covs_2018)[colnames(Covs_2018)=="layer.y"] <- "TravTime"
colnames(Covs_2018)[colnames(Covs_2018)=="EducAttnmt_FemRepAge_2014"] <- "EducAttnmnt_2014"
colnames(Covs_2018)[colnames(Covs_2018)=="lat"] <- "LATNUM"
colnames(Covs_2018)[colnames(Covs_2018)=="long"] <- "LONGNUM"

##--rearrange Covs_2018 data frame
Covs_2018 <- as.data.frame(Covs_2018 %>%
                             dplyr::select("LONGNUM","LATNUM",everything()))

##--remove NAs from the Covs_2018 columns
Covs_2018 <- Covs_2018[complete.cases(Covs_2018[ , 3:5]),]

##--convert to matrix
Covs_2018.mtrx <- as.matrix(Covs_2018)

##--INLA for ANY NET USE
##--create and plot mesh
coo.2018 <- cbind(GPS.2018.tbl$LONGNUM, GPS.2018.tbl$LATNUM) #get coordinates
mesh.2018 <- inla.mesh.2d(loc = coo.2018, max.edge = c(0.3, 5), cutoff = 0.015)
mesh.2018$n #number of vertices
plot(mesh.2018)
points(coo.2018, col = "red")

spde.2018 <- inla.spde2.matern(mesh = mesh.2018, alpha = 2, constr = TRUE)
indexs.2018 <- inla.spde.make.index("s", spde.2018$n.spde)
lengths(indexs.2018)
A.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coo.2018) #A matrix for est stack
coop.2018 <- Covs_2018.mtrx[, c("LONGNUM", "LATNUM")]
Ap.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coop.2018) #A matrix for pred stack

# stack for estimation
stk.e.2018 <- inla.stack(
  tag = "est",
  data = list(y = GPS.2018.tbl$NetUse, numtrials = GPS.2018.tbl$total),
  A = list(1, A.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = GPS.2018.tbl$SMOD_2015,
                            TRAV = GPS.2018.tbl$TravTime,
                            EDUC = GPS.2018.tbl$EducAttnmnt_2014), s = indexs.2018))

# stack for prediction
stk.p.2018 <- inla.stack(
  tag = "pred",
  data = list(y = NA, numtrials = NA),
  A = list(1, Ap.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = Covs_2018.mtrx[, 4],
                            TRAV = Covs_2018.mtrx[, 5],
                            EDUC = Covs_2018.mtrx[, 3]), s = indexs.2018))

stk.full.2018 <- inla.stack(stk.e.2018, stk.p.2018)

formula.2018 <- y ~ 0 + b0 + SMOD + TRAV + EDUC + f(s, model = spde.2018)

NetUse_res.2018 <- inla(formula.2018,
                     family = "binomial", Ntrials = numtrials,
                     control.family = list(link = "logit"),
                     verbose = TRUE,
                     data = inla.stack.data(stk.full.2018),
                     control.predictor = list(
                       compute = TRUE, link = 1,
                       A = inla.stack.A(stk.full.2018)))

index.2018 <- inla.stack.index(stack = stk.full.2018, tag = "pred")$data
prev_mean.2018 <- NetUse_res.2018$summary.fitted.values[index.2018, "mean"]
prev_ll.2018 <- NetUse_res.2018$summary.fitted.values[index.2018, "0.025quant"]
prev_ul.2018 <- NetUse_res.2018$summary.fitted.values[index.2018, "0.975quant"]

NetUse_mean.raster <- rasterize(
  x = coop.2018, y = TravTime.agg, field = prev_mean.2018,
  fun = mean
)

(NGA_NetUse<-tm_shape(adm0_NGA) +
  tm_polygons(col="blue") + 
  tm_shape(adm0_NGA) +
  tm_borders(col="black",lwd=2) + 
  tm_shape(NetUse_mean.raster) +
  tm_raster(style="cont",palette ="viridis") +
  tm_layout(title="",
            title.size=1.5,
            title.position=c("center","TOP"),
            legend.outside=TRUE,
            legend.title.size=2,
            legend.text.size=1.5,
            legend.show=TRUE,
            legend.height=2,
            frame=FALSE))

save(NetUse_res.2018,NetUse_mean.raster,NGA_NetUse,stk.full.2018,file="C:/Users/mworges/Desktop/Nigeria/NGA_NetUse.RData")

##--INLA for NET AVAILABILITY
##--create and plot mesh
coo.2018 <- cbind(GPS.2018.tbl$LONGNUM, GPS.2018.tbl$LATNUM) #get coordinates
mesh.2018 <- inla.mesh.2d(loc = coo.2018, max.edge = c(0.3, 5), cutoff = 0.015)
mesh.2018$n #number of vertices
plot(mesh.2018)
points(coo.2018, col = "red")

spde.2018 <- inla.spde2.matern(mesh = mesh.2018, alpha = 2, constr = TRUE)
indexs.2018 <- inla.spde.make.index("s", spde.2018$n.spde)
lengths(indexs.2018)
A.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coo.2018) #A matrix for est stack
coop.2018 <- Covs_2018.mtrx[, c("LONGNUM", "LATNUM")]
Ap.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coop.2018) #A matrix for pred stack

# stack for estimation
stk.e.2018 <- inla.stack(
  tag = "est",
  data = list(y = GPS.2018.tbl$NetAvlblty, numtrials = GPS.2018.tbl$total),
  A = list(1, A.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = GPS.2018.tbl$SMOD_2015,
                            TRAV = GPS.2018.tbl$TravTime,
                            EDUC = GPS.2018.tbl$EducAttnmnt_2014), s = indexs.2018))

# stack for prediction
stk.p.2018 <- inla.stack(
  tag = "pred",
  data = list(y = NA, numtrials = NA),
  A = list(1, Ap.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = Covs_2018.mtrx[, 4],
                            TRAV = Covs_2018.mtrx[, 5],
                            EDUC = Covs_2018.mtrx[, 3]), s = indexs.2018))

stk.full.2018 <- inla.stack(stk.e.2018, stk.p.2018)

formula.2018 <- y ~ 0 + b0 + SMOD + TRAV + EDUC + f(s, model = spde.2018)

NetAvlblty_res.2018 <- inla(formula.2018,
                        family = "binomial", Ntrials = numtrials,
                        control.family = list(link = "logit"),
                        verbose = TRUE,
                        data = inla.stack.data(stk.full.2018),
                        control.predictor = list(
                          compute = TRUE, link = 1,
                          A = inla.stack.A(stk.full.2018)))

index.2018 <- inla.stack.index(stack = stk.full.2018, tag = "pred")$data
prev_mean.2018 <- NetAvlblty_res.2018$summary.fitted.values[index.2018, "mean"]
prev_ll.2018 <- NetAvlblty_res.2018$summary.fitted.values[index.2018, "0.025quant"]
prev_ul.2018 <- NetAvlblty_res.2018$summary.fitted.values[index.2018, "0.975quant"]

NetAvlblty_mean.raster <- rasterize(
  x = coop.2018, y = TravTime.agg, field = prev_mean.2018,
  fun = mean
)

(NGA_NetAvlblty<-tm_shape(adm0_NGA) +
  tm_polygons(col="blue") + 
  tm_shape(adm0_NGA) +
  tm_borders(col="black",lwd=2) + 
  tm_shape(NetAvlblty_mean.raster) +
  tm_raster(style="cont", palette ="viridis") +
  tm_layout(title="",
            title.size=1.5,
            title.position=c("center","TOP"),
            legend.outside=TRUE,
            legend.title.size=2,
            legend.text.size=1.5,
            legend.show=TRUE,
            legend.height=2,
            frame=FALSE))

save(NetAvlblty_res.2018,NetAvlblty_mean.raster,NGA_NetAvlblty,stk.full.2018,file="C:/Users/mworges/Desktop/Nigeria/NGA_NetAvlblty.RData")

par(mfrow=c(1,2))
hist(GPS.2018.tbl$NetUse)
hist(GPS.2018.tbl$NetAvlblty)
par(mfrow=c(1,1))

##--INLA for Population Access to Nets
##--create and plot mesh
coo.2018 <- cbind(GPS.2018.tbl$LONGNUM, GPS.2018.tbl$LATNUM) #get coordinates
mesh.2018 <- inla.mesh.2d(loc = coo.2018, max.edge = c(0.3, 5), cutoff = 0.015)
mesh.2018$n #number of vertices
plot(mesh.2018)
points(coo.2018, col = "red")

spde.2018 <- inla.spde2.matern(mesh = mesh.2018, alpha = 2, constr = TRUE)
indexs.2018 <- inla.spde.make.index("s", spde.2018$n.spde)
lengths(indexs.2018)
A.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coo.2018) #A matrix for est stack
coop.2018 <- Covs_2018.mtrx[, c("LONGNUM", "LATNUM")]
Ap.2018 <- inla.spde.make.A(mesh = mesh.2018, loc = coop.2018) #A matrix for pred stack

# stack for estimation
stk.e.2018 <- inla.stack(
  tag = "est",
  data = list(y = GPS.2018.tbl$PopAcc),
  A = list(1, A.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = GPS.2018.tbl$SMOD_2015,
                            TRAV = GPS.2018.tbl$TravTime,
                            EDUC = GPS.2018.tbl$EducAttnmnt_2014), s = indexs.2018))

# stack for prediction
stk.p.2018 <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap.2018),
  effects = list(data.frame(b0 = 1, 
                            SMOD = Covs_2018.mtrx[, 4],
                            TRAV = Covs_2018.mtrx[, 5],
                            EDUC = Covs_2018.mtrx[, 3]), s = indexs.2018))

stk.full.2018 <- inla.stack(stk.e.2018, stk.p.2018)

formula.2018 <- y ~ 0 + b0 + SMOD + TRAV + EDUC + f(s, model = spde.2018)

PopAcc_res.2018 <- inla(formula.2018,
                        family = "gaussian",
                        control.family = list(link = "identity"),
                        verbose = TRUE,
                        data = inla.stack.data(stk.full.2018),
                        control.predictor = list(
                          compute = TRUE, link = 1,
                          A = inla.stack.A(stk.full.2018)))

index.2018 <- inla.stack.index(stack = stk.full.2018, tag = "pred")$data
prev_mean.2018 <- PopAcc_res.2018$summary.fitted.values[index.2018, "mean"]
prev_ll.2018 <- PopAcc_res.2018$summary.fitted.values[index.2018, "0.025quant"]
prev_ul.2018 <- PopAcc_res.2018$summary.fitted.values[index.2018, "0.975quant"]

PopAcc_mean.raster <- rasterize(
  x = coop.2018, y = TravTime.agg, field = prev_mean.2018,
  fun = mean
)

(NGA_PopAcc<-tm_shape(adm0_NGA) +
    tm_polygons(col="blue") + 
    tm_shape(adm0_NGA) +
    tm_borders(col="black",lwd=2) + 
    tm_shape(PopAcc_mean.raster) +
    tm_raster(style="cont",palette ="viridis") +
    tm_layout(title="",
              title.size=1.5,
              title.position=c("center","TOP"),
              legend.outside=TRUE,
              legend.title.size=2,
              legend.text.size=1.5,
              legend.show=TRUE,
              legend.height=2,
              frame=FALSE))

save(NetUse_res.2018,NetUse_mean.raster,NGA_NetUse,stk.full.2018,file="C:/Users/mworges/Desktop/Nigeria/NGA_NetUse.RData")

