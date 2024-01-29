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
library(ggpubr)
library(tidyverse)
options(digits=2)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

setwd("C:\\Users\\wkerr\\Dropbox\\OnTIME local\\papers\\equity_paper")

aba          <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Aba_Abia_urban_LGAs")
abuja        <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Abuja_urban_LGAs")
benincity    <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "BeninCity_urban_LGAs")
ibadan       <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Ibadan_urban_LGAs")
jos          <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Jos_urban_LGAs")
kaduna       <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Kaduna_urban_LGAs")
kano         <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Kano_urban_LGAs")
lagos        <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Lagos_Abia_urban_LGAs")
ilorin       <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Llorin_urban_LGAs")
maiduguri    <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Maiduguri_urban_LGAs")
onitsha      <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Onitsha_urban_LGAs")
owerri       <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Owerri_urban_LGAs")
portharcourt <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "PortHarcourt_urban_LGAs")
uyo          <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Uyo_urban_LGAs")
warri        <- st_read(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Shapefiles of urban LGAs", layer = "Warri_urban_LGAs")

#remove some LGAs where we did not work in
aba <- subset(aba, lga_code != 33019)
abuja <- subset(abuja, !lga_code %in% c(26004, 27023, 27024))
onitsha$OBJECTID <- NULL
onitsha$Shape_Leng <- NULL
onitsha$Shape_Area <- NULL
owerri <- subset(owerri, lga_name != "Mbatoli")


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
HF[, city := stringr::str_to_title(city)]
HF <- SpatialPointsDataFrame(coords = HF[,c(3,2)], data = HF,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

NG <- readOGR(dsn = "C:\\Users\\wkerr\\Dropbox\\OnTIME Consortium\\Analysis\\Base layers\\GRID3_Nigeria_-_State_Boundaries", layer = "NGA_State_Boundaries")

#load google metrics outputs
s2 <- as.data.table(qs::qread("data\\cleaned_s2.qs"))

try <- s2[facility_type=="public" & departure_time =="weekday6-8pm" & population>0]

try <- try[, .(city, facility_type, departure_time, x, y, rwi, tt1, population, wiq)]
  max <- try[, .(max = max(tt1, na.rm = TRUE)), by = .(city, facility_type, departure_time)]
  max[, max := max+1]
  try <- merge(try, max, by = c("city", "facility_type", "departure_time"))  
  try[is.na(tt1), tt1 := max]
  try[is.infinite(tt1), tt1 := max]

try[, tt1_5 := as.numeric(Hmisc::cut2(tt1, cuts = c(0, 60, 120, 200)))]
try[, group := as.character(interaction(tt1_5, wiq))]

try[wiq %in% c(3,4,5) & tt1>60, col := "#e66101"]
try[wiq %in% c(1,2) & tt1>60, col := "#fdb863"]

try[wiq %in% c(3,4,5) & tt1<=60, col := "#5e3c99"]
try[wiq %in% c(1,2) & tt1<=60, col := "#b2abd2"]


try[, corr := cor.test(rwi, tt1)$estimate, by = city]  
try[, ci := paste0("(",
                   round(cor.test(rwi, tt1)$conf.int[1], 2), ",",
                   round(cor.test(rwi, tt1)$conf.int[2], 2), ")"), by = city]  
  
perc <- try[, .(N = .N), by = .(city, col)]
perc[col %in% c("#fdb863", "#b2abd2"), poor := 1]
perc[, tot := sum(N), by = .(city, poor)]
perc[, p := N*100/tot]
perc <- perc[, .(city, col, p)]
perc <- dcast(perc, city  ~ col , value.car = "p")

  order <- perc[, .(city, `#fdb863`)]
  setorder(order, -`#fdb863`)
  order[, order := 1:nrow(order)]
  
try <- merge(try, order[, .(city, order)], by = "city")

  corr <- unique(try[, .(city, order, corr, ci)])
  corr[, corr := round(corr,2)]
  
  vline <- try[wiq==2, .(city, order, rwi)]
  vline <- vline[, .(vline=max(rwi)), by = .(order, city)]

order.labs <- order$city
names(order.labs) <- order$order

  ##over 100 private
  
allcities_scatter <- ggplot() +
  labs(title="Relative wealth index (RWI) and travel time of individual S2 cells \nin 15 cities in Nigeria (weekday 18-20 h)", 
       caption="CEmOC: Comprehensive Emergency Obstetric Care") +
  geom_point(data=try[], aes(x=rwi, y=tt1, col=col), size=0.5, alpha=0.5) +
  geom_hline(yintercept=c(60), linetype="dotted") +
  geom_vline(data=vline, aes(xintercept=vline), linetype="dotted") +
  geom_text(data=corr, aes(x=0.8, y=170, 
                           label = paste0("r=", corr, " ", ci)), size=2.5, fontface = "bold") +
  facet_wrap(.~order, ncol=5, 
             labeller = labeller(order = order.labs)) +
  scale_color_identity(name = "", guide = "legend", 
                       labels = c("Wealthiest 60% of S2 cells, MTT ≤ 60 min",
                                  "Least wealthy 40% of S2 cells, MTT ≤ 60 min",
                                  "Least wealthy 40% of S2 cells, MTT > 60 min", 
                                  "Wealthiest 60% of S2 cells, MTT > 60 min")) +
  scale_x_continuous(name = "Relative wealth index (RWI)", expand = c(0, 0)) +
  scale_y_continuous(name = "Travel time to the nearest CEmOC facility (minutes)", expand = c(0, 0), breaks = c(0, 30, 60, 120)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(face="italic"), 
        legend.position = "top",
        legend.direction = "horizontal") +
  guides(color=guide_legend(nrow=2,byrow=TRUE, override.aes = list(size = 3)))

outdir <- "C:/Users/wkerr/Dropbox/OnTIME Consortium/Publications/Equity paper/Resubmission/figures/20231228/"
ggsave(paste0(outdir, "Figure_3.png"), 
       allcities_scatter, width = 21, height = 15, unit="cm", device='tiff', dpi=300)


try.sp <- SpatialPointsDataFrame(data=data.frame(try), coords=cbind(try$x,try$y), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

this_c <- "Kano"
ggplot() + 
  geom_sf(data=st_as_sf(kano), fill = "grey90", alpha=0.5, col="grey75") +
  geom_sf(data=st_as_sf(subset(try.sp, city == this_c)), aes(col=col), size=1.5) +
  geom_sf(data = st_as_sf(subset(HF,   city   == this_c & Ownership=="Public CEmOC")),
          shape=3, stroke=2) +
  scale_color_identity() +
  ggtitle(this_c) +
  # facet_wrap(.~city) +
  theme_bw() +
  theme(legend.position = "none") +
  annotation_scale()

city_lga <- read.csv("C:\\Users\\wkerr\\Dropbox\\OnTIME local\\papers\\ng_wide_paper\\data\\state and city.csv")
cities_sf <- rbind(benincity, kaduna, portharcourt, onitsha)
titles <- c("Benin City", "Kaduna", "Port Harcourt", "Onitsha")
cities_sf <- merge(cities_sf, city_lga %>% select(lga_code, city), all.x = TRUE)

cities <- unique(cities_sf$city)
maps <- list()

for (i in 1:length(cities)){
  
  this_c <- cities[i]
  
  maps[[i]] <- ggplot() + 
    geom_sf(data=cities_sf %>% filter(city == this_c), aes(fill="state_code"), col="grey75") +
    scale_fill_manual(name = "", labels = "Population at cell = 0", values = "grey95") +
    geom_sf(data=st_as_sf(subset(try.sp, city==titles[i])), aes(col=col), size=0.8, alpha=0.8, shape=15) +
    scale_color_identity(name = "", guide = "legend", 
                         labels = c("Wealthiest 60% of S2 cells, MTT ≤ 60 min",
                                    "Least wealthy 40% of S2 cells, MTT ≤ 60 min",
                                    "Least wealthy 40% of S2 cells, MTT > 60 min", 
                                    "Wealthiest 60% of S2 cells, MTT > 60 min")) +
    geom_sf(data = st_as_sf(subset(HF, city == titles[i] & Ownership=="Public CEmOC")),
            aes(shape="city"), stroke=2) +
    scale_shape_manual(name = "", labels = "Public CEmOC facilites", values=3) + 
    ggtitle(paste0(letters[i], ". ", titles[i])) +
    theme_bw() +
    theme(legend.position = "bottom") +
    annotation_scale() +
    guides(color=guide_legend(nrow=2,byrow=TRUE, override.aes = list(size = 2)))
  
  # save_name <- paste0(this_c, ".pdf")
  # ggsave(save_name, scale=0.8)
}


fig4_out <- ggpubr::ggarrange(maps[[1]], maps[[2]], maps[[3]], maps[[4]],
                              common.legend = TRUE, legend="top") + bgcolor("white") + border("white")
fig4_out <- annotate_figure(fig4_out, 
                                top = text_grob("Locations of public CEmOC facilites and relative wealth at S2 cell level", size=15))
fig4_out <- annotate_figure(fig4_out, bottom = text_grob("CEmOC: Comprehensive emergency obstetric care", size=12, face="italic", hjust=-0.27))
fig4_out <- annotate_figure(fig4_out, bottom = text_grob("MTT: Median travel time", size=12, face="italic", hjust=-1.7))
fig4_out <- fig4_out + bgcolor("white") + border("white")



outdir <- "C:/Users/wkerr/Dropbox/OnTIME Consortium/Publications/Equity paper/Resubmission/figures/20231228/"
ggsave(paste0(outdir, "fig4.png"), 
       fig4_out, width =25, height = 25, unit="cm", device='tiff', dpi=300)


