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

median <- s2[, .(d05 = quantile(tt1, 0.5, na.rm = TRUE)), 
             by = .(city, facility_type, departure_time, wiq)]
order <- median[facility_type=="public" & departure_time=="weekday6-8pm" & wiq==5]
setorder(order, d05)
order[, order := 1:nrow(order)]

median <- merge(median, order[, .(city, order)])

pub_line <- median[facility_type=="public" & departure_time=="weekday6-8pm", 
                   .(min = min(d05), 
                     max = max(d05)), by = order]
both_line <- median[facility_type=="both" & departure_time=="weekday6-8pm", 
                    .(min = min(d05), 
                      max = max(d05)), by = order]

  
  ##add MTT by city
  city <- s2[, .(d05 = quantile(tt1, 0.5, na.rm = TRUE)), 
             by = .(city, facility_type, departure_time)]
  city <- merge(city, order[, .(city, order)])

et1 <- ggplot() +
  geom_vline(xintercept = c(0, 30, 60, 90), linetype = "dotted") +
  geom_segment(data=pub_line, aes(x=min, xend=max, y=order, yend=order)) +
  geom_point(data=median[facility_type=="public" & departure_time=="weekday6-8pm"], 
             aes(x=d05, y=order,  fill=as.integer(wiq)), 
             color = "black", shape=21, size=4, alpha=0.75) +
  scale_y_continuous(name = "", breaks = order$order, labels = order$city) +
  scale_x_continuous(name = "a. MTT to the nearest public \n CEmOC facility (minutes)", expand = c(0, 0), 
                     limits = c(0, 100), breaks = c(0, 30, 60, 90), position="top") +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  theme_bw() +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10), 
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"), 
        legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = , reverse=TRUE))

et2 <- ggplot() +
  geom_vline(xintercept = c(0, 30, 60, 90), linetype = "dotted") +
  geom_segment(data=both_line, aes(x=min, xend=max, y=order, yend=order)) +
  geom_point(data=median[facility_type=="both" & departure_time=="weekday6-8pm"], 
             aes(x=d05, y=order,  fill=as.integer(wiq)), 
             color = "black", shape=21, size=4, alpha=0.75) +
  scale_y_continuous(name = "", breaks = order$order, labels = order$city) +
  scale_x_continuous(name = "b. MTT to the nearest public or private \n CEmOC facility (minutes)", expand = c(0, 0), 
                     limits = c(0, 100), breaks = c(0, 30, 60, 90), position="top") +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  theme_bw() +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10), 
        legend.position = "bottom", 
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1.5, reverse=TRUE))



equiplot_out <- ggpubr::ggarrange(et1, et2, en1, en2, 
                                  common.legend = TRUE, legend="top") + bgcolor("white") + border("white")
equiplot_out <- annotate_figure(equiplot_out, 
                top = text_grob("Equiplot of geographic accessibility by relative wealth in 15 cities in Nigeria", size=15),
                bottom = text_grob("CEmOC: Comprehensive emergency obstetric care", size=10, face="italic", hjust=-0.3))
equiplot_out <- annotate_figure(equiplot_out,
                bottom = text_grob("MTT: Median travel time", , size=10, face="italic", hjust=-1.75))
equiplot_out <- equiplot_out + bgcolor("white") + border("white")


