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

#s2 cell data is available from figshare (see ref 43)
s2 <- as.data.table(s2.google)

#remove dup s2 cells  
s2 <- unique(setDT(s2)[order(facility_type, departure_time, s2cellid, ward_code)], 
             by = c("facility_type", "departure_time", "s2cellid"))

##remove s2 cells with pop=0
s2 <- s2[, pop0 := fifelse(population==0)]

#need to exclude a bunch of LGAs
s2$lga_code <- as.character(s2$lga_code)
city <- qs::qread("..\\NG_wide_paper\\data\\lgas_in_cities.qs")
city$lga_code <- as.character(city$lga_code)
  
  s2 <- merge(s2, city[, .(lga_code, city)], all.y = TRUE)
  
  setnames(s2, "min_travel_time_1_minutes", "tt1")
  setnames(s2, "min_travel_time_2_minutes", "tt2")
  setnames(s2, "min_travel_time_3_minutes", "tt3")
  
  setnames(s2, "n_within_15", "n15")
  setnames(s2, "n_within_30", "n30")
  setnames(s2, "n_within_60", "n60")
  
  s2[, c("y","x") := data.table(stringr::str_split_fixed(center_lat_lng," ",2))]
  s2[, y := as.numeric(y)]
  s2[, x := as.numeric(x)]
  s2[, center_lat_lng := NULL]
  
  s2$geometry_wkt <- NULL
  
  s2[, tt1 := as.numeric(tt1)]
  s2[, tt2 := as.numeric(tt2)]
  s2[, tt3 := as.numeric(tt3)]
  s2[, n15 := as.numeric(n15)]
  s2[, n30 := as.numeric(n30)]
  s2[, n60 := as.numeric(n60)]
  
  s2[is.na(n15), n15 := 0]
  s2[is.na(n30), n30 := 0]
  s2[is.na(n60), n60 := 0]
  
  #create WIQ from rwi
  s2[, rwi := as.numeric(rwi)]
  wiq <- s2[facility_type=="both" & departure_time=="weekday6-8pm", .(s2cellid, rwi, city)]
  setorder(wiq, rwi)
  wiq[, wiq := as.numeric(Hmisc::cut2(rwi, g=5)), by = city]
  wiq <- unique(wiq[, .(s2cellid, wiq)])
  
  s2 <- merge(s2, wiq, by = c("s2cellid"))  
  
qs::qsave(s2, "data\\cleaned_s2.qs")
