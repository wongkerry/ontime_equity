library(raster)
library(rgdal)
library(sf)
library(classInt)
library(tmap)
library(data.table)
library(ggplot2)
library(tmap)
library(extrafont)
library(ggridges)
library(mgcv)
library(GWmodel)
library(ggspatial)

options(digits=2)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

setwd("C:\\Users\\wkerr\\Dropbox\\OnTIME local\\papers\\equity_paper")

# water <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\PC\\Downloads\\Africa_waterbodies", 
#                  layer = "Africa_waterbody")

aba          <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Aba_Abia_urban_LGAs")
abuja        <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Abuja_urban_LGAs")
benincity    <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "BeninCity_urban_LGAs")
#enugu        <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Enugu_urban_LGAs")
ibadan       <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Ibadan_urban_LGAs")
jos          <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Jos_urban_LGAs")
kaduna       <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Kaduna_urban_LGAs")
kano         <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Kano_urban_LGAs")
lagos        <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Lagos_Abia_urban_LGAs")
ilorin       <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Llorin_urban_LGAs")
maiduguri    <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Maiduguri_urban_LGAs")
#nsukka       <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Nsukka_urban_LGAs")
onitsha      <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Onitsha_urban_LGAs")
owerri       <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Owerri_urban_LGAs")
portharcourt <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "PortHarcourt_urban_LGAs")
#umuahia      <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Umuahia_urban_LGAs")
uyo          <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Uyo_urban_LGAs")
warri        <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Warri_urban_LGAs")

#remove some LGAs where we did not work in
aba <- subset(aba, lga_code != 33019)
abuja <- subset(abuja, !lga_code %in% c(26004, 27023, 27024))
onitsha$OBJECTID <- NULL
onitsha$Shape_Leng <- NULL
onitsha$Shape_Area <- NULL

#load HF data
HF <- readxl::read_xlsx("C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Datasets\\OnTIME_Nigeria_Masterlist_v2_Updated_withcitynames_20230322.xlsx", sheet = "Database")
HF <- data.table(HF)
HF <- HF[, .(`City Name`, latitude, longitude, owner, orig_order)]
#listed as in Benin City but very far away (in Lagos)
#remove from visualization
HF <- HF[orig_order != 794]
HF <- HF[orig_order != 762]

names(HF)[1] <- "city"
HF[, owner := as.character(owner)]
HF[owner==1, owner := "Public CEmOC"]
HF[owner==2, owner := "Private CEmOC"]
HF[, owner := as.factor(owner)]
names(HF)[4] <- "Ownership"
HF[city == "Benin", city := "Benin City"]
HF[city == "Portharcourt", city := "Port Harcourt"]
HF[, city := tolower(city)]
HF <- SpatialPointsDataFrame(coords = HF[,c(3,2)], data = HF,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

NG <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Base layers\\GRID3_Nigeria_-_State_Boundaries", layer = "NGA_State_Boundaries")

#load google metrics outputs
s2 <- as.data.table(qs::qread("data\\cleaned_s2.qs"))

try <- s2[facility_type=="public" & departure_time =="weekday6-8pm"]

try <- try[, .(city, x, y, rwi, n60, population, wiq)]

try[, n60_5 := as.numeric(Hmisc::cut2(n60, cuts = c(0, 1, 5, 10, 100)))]
try[, group := as.character(interaction(n60_5, wiq))]

map_col <- c(
  "4.5" = '#aacde3',
  "3.5" = '#80b5d6',
  "2.5" = "#2b83ba",
  "1.5" = "#226995",
  "4.4" = '#ddf1db',
  "3.4" = '#cdebc8',
  "2.4" = "#abdda4",
  "1.4" = "#89b183",
  "4.3" = '#ffffe5', 
  "3.3" = '#ffffd9', 
  "2.3" = "#ffffbf", 
  "1.3" = "#cccc99", 
  "4.2" = '#fedfc0',
  "3.2" = '#fecea0',
  "2.2" = "#fdae61",
  "1.2" = "#ca8b4e",
  "4.1" = '#efa3a4',
  "3.1" = '#e77577',
  "2.1" = "#d7191c",
  "1.1" = "#ac1416")
  
try[, cols := map_col[group]]


# ggplot(try[]) +
#   geom_point(aes(x=rwi, y=n60, col=as.character(group)), size=2) +
#   scale_color_manual(values = colors) +
#   scale_x_continuous(name = "Relative wealth index", expand = c(0.02, 0)) +
#   scale_y_continuous(name = "Numebr of public CEmOC within reach under 60 minutes", 
#                      expand = c(0.02, 0)) +
#   facet_wrap(.~city, ncol=5) +
#   theme_bw() +
#   theme(legend.position = "none", 
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#   

try.sp <- SpatialPointsDataFrame(data=data.frame(try), coords=cbind(try$x,try$y), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

double <- ggplot() + 
  geom_sf(data=st_as_sf(subset(try.sp, city=="Ibadan")), aes(col=cols)) +
  geom_sf(data = st_as_sf(subset(HF, city == "ibadan" & Ownership=="Public CEmOC")), 
          shape=3, size=2, stroke=2) +
  scale_color_identity() +
  theme_bw() +
  theme(legend.position = "none") +
  annotation_scale()
