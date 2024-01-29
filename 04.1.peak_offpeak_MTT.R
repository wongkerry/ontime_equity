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

#load google metrics outputs
s2 <- as.data.table(qs::qread("data\\cleaned_s2.qs"))

  max <- s2[, .(max = max(tt1, na.rm = TRUE)), by = .(city, facility_type, departure_time)]
  max[, max := max+1]
  s2 <- merge(s2, max, by = c("city", "facility_type", "departure_time"))  
  s2[is.na(tt1), tt1 := max]

diff <- s2[departure_time %in% c("weekday6-8pm", "weekend1-3am")]
diff <- diff[facility_type=="public", .(s2cellid, city, departure_time, tt1, wiq)]
diff <- diff[departure_time == "weekday6-8pm", departure_time := "peak"]
diff <- diff[departure_time == "weekend1-3am", departure_time := "offpeak"]

diff <- diff[, .(median = median(tt1)), by = .(city, wiq, departure_time)]
diff <- dcast(diff, city + wiq ~ departure_time, value.vars = "median")
diff[, more := peak - offpeak]

popMTT <- ggplot(diff[]) + theme_bw() +
  labs(subtitle = "a. Median travel time (MTT) to the nearest public CEmOC facility") +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  geom_point(aes(x=offpeak, y=peak, fill = as.integer(wiq)), shape=21, color = "NA", size=4, alpha=0.8) +  
  scale_x_continuous(name = "MTT to the nearest \npubilc CEmOC facilites in minutes - peak", 
                     expand = c(0, 0), limits = c(0, 90), breaks = seq(0, 90, 15)) + 
  scale_y_continuous(name = "MTT to the nearest \npubilc CEmOC facilites in minutes - off-peak", 
                     expand = c(0, 0), limits = c(0, 90), breaks = seq(0, 90, 15)) +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  theme_bw() +
  theme(text = element_text(size=11), 
        plot.subtitle = element_text(size=11), 
        legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1.5, reverse=TRUE))

