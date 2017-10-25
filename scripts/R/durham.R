options(max.print = .Machine$integer.max)
rm(list=ls())

# installs packages
list.of.packages <- c("ggplot2", "rgeos", "Rcpp", "ggmap", "stringr", "raster", "sp","zoo", "geosphere", "ggproto")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
require(rgdal)
require(ggmap)
require(ggplot2)
require(stringr)
require(raster)
require(sp)
require(geosphere)
require(zoo)
require(rgeos)

# READ THE GLOSSY BUCKTHORN DATA
# REMOVE UNECESSARY ENTRIES
glossy_data = read.csv('../../resources/glossy.csv', header = TRUE, sep = ",")
glossy_data = glossy_data[glossy_data$Observation_.Date  !="", ]
glossy_data = glossy_data[glossy_data$Coordinates !="", ]

ret = str_split_fixed(glossy_data$Observation_.Date, " ", 3)
glossy_data$Observation_.Date_.Day = as.numeric(ret[, 1])
glossy_data$Observation_.Date_.Month = ret[, 2]
glossy_data$Observation_.Date_.Year = as.numeric(ret[, 3])

ret <- str_split_fixed(glossy_data$Coordinates,",",2)
glossy_data$Coordinates_.Latitude <- as.double(ret[,1])
glossy_data$Coordinates_.Longitude <- as.double(ret[,2])

# Remove unecessary columns

glossy_data$Reporter <- NULL
glossy_data$Updated_.By <- NULL
glossy_data$Collector <- NULL
glossy_data$Report_.Ownership <- NULL
glossy_data$Florida_.Reporter <- NULL
glossy_data$Common_.Name <- NULL
glossy_data$Scientific_.Name <- NULL
glossy_data$Weed_.Watch_.I.D <- NULL
glossy_data$Herbarium <- NULL
glossy_data$Herbarium_.Record <- NULL
glossy_data$Host <- NULL
glossy_data$Host_.Phenology <- NULL
glossy_data$Host_.Damage <- NULL
glossy_data$Host_.Name <- NULL
glossy_data$Life_.Status <- NULL
glossy_data$Muncipality_.Name <- NULL
glossy_data$Conservation_.Area_.Name <- NULL
glossy_data$Conservation_.Area <- NULL
glossy_data$Lake..Bay_.Name <- NULL
glossy_data$Lake..Bay_.Type <- NULL
glossy_data$Creek_.Name <- NULL
glossy_data$Creek_.Type <- NULL
glossy_data$Alberta_.Jurisdiction <- NULL
glossy_data$Fisheries_.Management_.Zone_.I.D <- NULL
glossy_data$Access <- NULL
glossy_data$U.R.L <- NULL
glossy_data$Reporter_.Affiliation <- NULL
glossy_data$G.P.S_.Model <- NULL
glossy_data$Other_.Surveyors <- NULL
glossy_data$Project_.Name <- NULL
glossy_data$Project_.Method <- NULL
glossy_data$Project_.Discussion <- NULL
glossy_data$Project_.Description <- NULL
glossy_data$Course <- NULL
glossy_data$Study_.Type <- NULL
glossy_data$Speed_.In_.Meters_.Per_.Second <- NULL
glossy_data$Comments <- NULL
glossy_data$Surveyor <- NULL
glossy_data$Other_.Surveyors <- NULL
glossy_data$Agent_.Source <- NULL
glossy_data$First_.Nation_.Name <- NULL
glossy_data$Federal_.Lands_.Name <- NULL
glossy_data$Federal_.Lands_.Type <- NULL
glossy_data$Visit_.Type <- NULL
glossy_data$Quantity <- NULL
glossy_data$glossy_data_.Type <- NULL
glossy_data$Florida_.Location_.I.D <- NULL
glossy_data$Conservation_.Authority_.Name <- NULL
glossy_data$Conservation_.Area_.I.D <- NULL
glossy_data$Conservation_.Reserves_.Name <- NULL
glossy_data$River_.Name <- NULL
glossy_data$River_.Type <- NULL
glossy_data$Provincial_.Park_.Name <- NULL
glossy_data$Sighting_.Waterbody_.I.D <- NULL
glossy_data$M.N.R_.District_.Boundaries_.Name <- NULL
glossy_data$Provincial_.Park_.Name <- NULL
glossy_data$method <- NULL
glossy_data$C.W.M.A_.Name <- NULL
glossy_data$Datum <- NULL
glossy_data$A.K.E.P.I.C_.Original_.Site <- NULL
glossy_data$A.K.E.P.I.C_.Site <- NULL
glossy_data$Reviewer <- NULL
glossy_data$Reviewer_.Identifier <- NULL
glossy_data$Treatment_.Comments <- NULL
glossy_data$F.W.C..Observation..I.D <- NULL
glossy_data$Observation_.Identifier <- NULL
glossy_data$Nativity <- NULL
glossy_data$City <- NULL
glossy_data$Reviewer_.Identifier <- NULL
glossy_data$Local_.Ownership <- NULL
glossy_data$Locality <- NULL
glossy_data$Sex <- NULL
glossy_data$Voucher <- NULL
glossy_data$Disturbance_.I.D <- NULL
glossy_data$Date_.Uncertainty <- NULL
glossy_data$Date_.Reviewed <- NULL
glossy_data$Control_.Action <- NULL
glossy_data$Site <- NULL
glossy_data$Site_.Name <- NULL
glossy_data$Year_.Accuracy <- NULL
glossy_data$F.I.P.S_codes <- NULL
glossy_data$National_.Ownership <- NULL
glossy_data$National_.Forest <- NULL
glossy_data$Original_.Reported_.Subject <- NULL
glossy_data$Forest_.Service_.Region <- NULL
glossy_data$Ranger_.District <- NULL
glossy_data$Source_.Name <- NULL
glossy_data$Source_.Type <- NULL
glossy_data$Date_.Entered <- NULL
glossy_data$Date_.Updated <- NULL
glossy_data$Location <- NULL
glossy_data$Habitat <- NULL
glossy_data$Number_.Of_.Traps <- NULL
glossy_data$Trap_.Type <- NULL
glossy_data$Management_.Type <- NULL
glossy_data$Disturbance <- NULL
glossy_data$Distrubance_.I.D <- NULL
glossy_data$Distrubance <- NULL
glossy_data$Disturbance_.Age <- NULL
glossy_data$Disturbance_.Type <- NULL
glossy_data$Disturbanve_.Severity <- NULL
glossy_data$Aggressiveness <- NULL
glossy_data$Satellite <- NULL
glossy_data$Reference <- NULL
glossy_data$Population_.Size <- NULL
glossy_data$Largest_.Observed <- NULL
glossy_data$Smallest_.Observed <- NULL
glossy_data$Water_.Clarity <- NULL
glossy_data$Water_.Velocity <- NULL
glossy_data$Source_.I.D <- NULL
glossy_data$Nutrient_.Level <- NULL
glossy_data$Plants_.Treated <- NULL
glossy_data$Target_.Range <- NULL
glossy_data$Target_.Name <- NULL
glossy_data$Target_.Count <- NULL
glossy_data$Stop_.Time <- NULL
glossy_data$Start_.Time <- NULL
glossy_data$Maturity <- NULL
glossy_data$Patch_.Type <- NULL
glossy_data$Collection_.Time <- NULL
glossy_data$Confidence <- NULL
glossy_data$Treatment_.Area <- NULL

# Remap months to numbers

glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "January")] <- 1
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "February")] <- 2
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "March")] <- 3
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "April")] <- 4
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "May")] <- 5
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "June")] <- 6
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "July")] <- 7
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "August")] <- 8
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "September")] <- 9
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "October")] <- 10
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "November")] <- 11
glossy_data$Observation_.Date_.Month[which(glossy_data$Observation_.Date_.Month == "December")] <- 12
glossy_data$Observation_.Date_.Month <- as.numeric(glossy_data$Observation_.Date_.Month)
glossy_data <- glossy_data[order(glossy_data$Observation_.Date_.Year),]

# EXTRACT COORDINATES
coor_ <- data.frame("longitude" = glossy_data$Coordinates_.Longitude, "latitude" = glossy_data$Coordinates_.Latitude)
coor_ <- coor_[order(-coor_$longitude, coor_$latitude),  ]

pol1 <- chull(as.matrix(coor_))
pol1 <- Polygon(as.matrix(coor_[pol1, ]))

#Polygon around NH and MAINE glossy population
# -73.80615234125003 45.48513871959699
# -74.15771484125003 41.56816442093237
# -75.49804687250003 42.01049756503889
# -75.14648437250003 39.86966235384007
# -69.71923827875003 41.18895670533026
# -66.75292968500003 44.80324484552226
# -67.76367187250003 45.79239431452086
# -70.64208984125003 45.26905723017684
nh_coords <- data.frame("longitude"=c(-73.80615234125003, -74.15771484125003, -75.49804687250003, -75.14648437250003, -69.71923827875003, -66.75292968500003, -67.76367187250003, -69.98291015375003, -70.64208984125003 ),
                        "latitude"=c(45.48513871959699, 42.01049756503889, 42.01049756503889, 39.86966235384007, 41.18895670533026, 44.80324484552226, 45.79239431452086, 44.34935530251231, 45.26905723017684))
# nh_coords <- nh_coords[order(-nh_coords$longitude, nh_coords$latitude),  ]
#nh_polygon <- chull(as.matrix(nh_coords))
nh_polygon <- Polygon(as.matrix(nh_coords))
nh_bounding_box = SpatialPolygons(list(Polygons(list(nh_polygon), ID="a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# GET THE GLOSSY DATA THAT FALL IN THE POLYGON
glossy_coords <- coor_[coor_$longitude >= min(nh_coords$longitude) &
                        coor_$longitude <= max(nh_coords$longitude) &
                        coor_$latitude >= min(nh_coords$latitude) &
                        coor_$latitude <= max(nh_coords$latitude), ]
glossy_coords <- na.omit(glossy_coords)
glossy_spatial_points <- SpatialPoints(glossy_coords, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# PLOT THE POLYGON AND POINTS
map <- get_map(location = 'united states', zoom = 3, maptype = "terrain", source = 'google', color = 'color')
ggmap(map) + 
   geom_point(data = nh_coords, aes(x = as.numeric(nh_coords$longitude), y = as.numeric(nh_coords$latitude)), 
              fill = "green", alpha = 0.8, size = 0.0001, shape = 21) +
   geom_point(data = glossy_coords, aes(x = as.numeric(glossy_coords$longitude), y = as.numeric(glossy_coords$latitude)), 
              fill = "red", alpha = 0.8, size = 0.001, shape = 21) +
   geom_polygon(aes(x = coordinates(nh_polygon)[,1], y = coordinates(nh_polygon)[,2]), data = nh_polygon, alpha = 0.5) +
   guides(fill = FALSE, alpha = FALSE, size = FALSE)

# GET UNIQUE YEARS IN CASE THEY ARE NEEDED
years = unique(glossy_data$Observation_.Date_.Year)

#CREATE A SPATIAL POLYGON FOR THE BOUNDING BOX
Ps1 = SpatialPolygons(list(Polygons(list(nh_polygon), ID = "a")), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# ZONE 13 CONTAINS THE DESIRED LOCATION
# If already created the data structure, set load to TRUE
# and load it instead of loading all the individual files
load <- FALSE
bio_13 <- stack()
var_names = c()

data.path <- "../../resources"

if(load == FALSE){
  zone_13_tmean <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio1_13.tif", sep = ''))
  zone_13_mdr <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio2_13.tif",sep=''))
  zone_13_iso <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio3_13.tif",sep=''))
  zone_13_ts <- raster(paste(data.path,  "/clim_zones/bio_13_tif/bio4_13.tif",sep=''))
  zone_13_tmax_month <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio5_13.tif",sep=''))
  zone_13_tmin_month <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio6_13.tif",sep=''))
  zone_13_tar <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio7_13.tif",sep=''))
  zone_13_tmean_wettest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio8_13.tif",sep=''))
  zone_13_tmean_driest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio9_13.tif",sep=''))
  zone_13_tmean_warmest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio10_13.tif",sep=''))
  zone_13_tmean_coldest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio11_13.tif",sep=''))
  zone_13_prec_an <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio12_13.tif",sep=''))
  zone_13_prec_wettest_month <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio13_13.tif",sep=''))
  zone_13_prec_driest_month <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio14_13.tif",sep=''))
  zone_13_ps <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio15_13.tif",sep=''))
  zone_13_prec_wettest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio16_13.tif",sep=''))
  zone_13_prec_driest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio17_13.tif",sep=''))
  zone_13_prec_warmest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio18_13.tif",sep=''))
  zone_13_prec_coldest <- raster(paste(data.path, "/clim_zones/bio_13_tif/bio19_13.tif",sep=''))
  
  bio_13 <- stack(bio_13, 
                  zone_13_tmean,
                  zone_13_mdr,
                  zone_13_iso,
                  zone_13_ts,
                  zone_13_tmax_month,
                  zone_13_tmin_month,
                  zone_13_tar,
                  zone_13_tmean_wettest,
                  zone_13_tmean_driest,
                  zone_13_tmean_warmest,
                  zone_13_tmean_coldest,
                  zone_13_prec_an,
                  zone_13_prec_wettest_month,
                  zone_13_prec_driest_month,
                  zone_13_ps,
                  zone_13_prec_wettest,
                  zone_13_prec_driest,
                  zone_13_prec_warmest,
                  zone_13_prec_coldest)
  
  var_names <- c(
    "bio1",
    "bio2",
    "bio3",
    "bio4",
    "bio5",
    "bio6",
    "bio7",
    "bio8",
    "bio9",
    "bio10",
    "bio11",
    "bio12",
    "bio13",
    "bio14",
    "bio15",
    "bio16",
    "bio17",
    "bio18",
    "bio19"
  )
  
  df_names <- c(
    "lon",
    "lat",
    "bio1",
    "bio2",
    "bio3",
    "bio4",
    "bio5",
    "bio6",
    "bio7",
    "bio8",
    "bio9",
    "bio10",
    "bio11",
    "bio12",
    "bio13",
    "bio14",
    "bio15",
    "bio16",
    "bio17",
    "bio18",
    "bio19"
  )
  # SAVE THE DATA
  
  names(bio_13) <- var_names
  bio_13 <- crop(bio_13, nh_bounding_box)
  saveRDS(bio_13, paste(data.path, "/bio_13.rds", sep = ''))
}else{
  bio_13 <- readRDS(paste(data.path, "/bio_13.rds", sep = ''))
}

# GET THE GLOSSY POINTS FROM RASTER
# CREATE DATAFRAMES FROM RASTER FOR PRESENCE AND UNKNONW POINTS
glossy_data_frame <- extract(bio_13, glossy_spatial_points, method = 'bilinear', na.rm = TRUE)
glossy_data_frame <- as.data.frame(glossy_data_frame)
glossy_data_frame <- cbind(glossy_coords$latitude, glossy_data_frame)
glossy_data_frame <- cbind(glossy_coords$longitude, glossy_data_frame)
colnames(glossy_data_frame) <- df_names
glossy_data_frame <- glossy_data_frame[complete.cases(glossy_data_frame), ]

nh_raster <- crop(bio_13, extent(nh_bounding_box), snap = 'in')
fr <- rasterize(nh_bounding_box, nh_raster)
nh_raster <- mask(x = nh_raster, mask = fr)
nh_coordinate_list <- coordinates(nh_raster)

nh_data_frame <- as.data.frame(nh_raster)
nh_data_frame <- cbind(nh_coordinate_list[,2], nh_data_frame)
nh_data_frame <- cbind(nh_coordinate_list[,1], nh_data_frame)
colnames(nh_data_frame) <- df_names
nh_data_frame <- nh_data_frame[complete.cases(nh_data_frame), ]

print(sprintf(" %d observances", nrow(glossy_data_frame)))
sprintf(" %d unknown", nrow(nh_data_frame))
#PLOT A RASTER LAYER AND THE GLOSSY POINTS IN THE EXAMINED AREA
# plot(nh_raster$bio1)
# plot(glossy_spatial_points, add=TRUE)
# plot(nh_bounding_box, add=TRUE)

# PLOT THE POLYGON AND POINTS
# map<-get_map(location='united states', zoom=4, maptype = "terrain", source='google',color='color')
# ggmap(map)+
# 
#   geom_point(data = nh_data_frame, aes(x = as.numeric(nh_data_frame$lon), y = as.numeric(nh_data_frame$lat)),
#              fill = "green", alpha =0.8, size = 0.0001, shape = 21)

saveRDS(nh_data_frame, "../../resources/nh_data_frame.rds")
saveRDS(glossy_data_frame, "../../resources/glossy_data_frame.rds")
