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

setwd("C:\\Users\\comp_name\\Dropbox\\OnTIME local\\papers\\equity_paper")

#load google metrics outputs
s2 <- as.data.table(qs::qread("data\\cleaned_s2.qs"))

  ##missing
  miss <- unique(s2[population>0 & is.na(tt1), .(city, s2cellid)])
  miss <- miss[, .(miss=.N), by = .(city)]
  
  ##pop zero
  zero <- unique(s2[population==0, .(city, s2cellid)])
  zero <- zero[, .(zero=.N), by = .(city)]
  
cell <- unique(s2[, .(city, s2cellid)])
cell <- cell[, .(N = .N), by = city]

cell <- merge(cell, miss, all=TRUE)
cell <- merge(cell, zero, all=TRUE)
cell[, miss := round(miss*100/N, 1)]
cell[, zero := round(zero*100/N, 1)]

