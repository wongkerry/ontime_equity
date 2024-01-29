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

options(digits=2)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

setwd("C:\\Users\\wkerr\\Dropbox\\OnTIME local\\papers\\equity_paper")

#load google metrics outputs
s2 <- as.data.table(qs::qread("data\\cleaned_s2.qs"))

mean <- s2[, .(d05 = floor(mean(n60))), 
             by = .(city, facility_type, departure_time, wiq)]

order <- mean[facility_type=="public" & departure_time=="weekday6-8pm" & wiq==5]
setorder(order, d05)
order[, order := 1:nrow(order)]

mean <- merge(mean, order[, .(city, order)])

pub_line <- mean[facility_type=="public" & departure_time=="weekday6-8pm", 
                   .(min = min(d05), 
                     max = max(d05)), by = order]
both_line <- mean[facility_type=="both" & departure_time=="weekday6-8pm", 
                    .(min = min(d05), 
                      max = max(d05)), by = order]


en1 <- ggplot() +
  # geom_vline(xintercept=10, linetype="dotted") +
  geom_segment(data=pub_line, aes(x=min, xend=max, y=order, yend=order)) +
  geom_point(data=mean[facility_type=="public" & departure_time=="weekday6-8pm"], 
             aes(x=d05, y=order,  fill=as.integer(wiq)), 
             color = "black", shape=21, size=4, alpha=0.75, 
             position=position_jitter(h=0, w=0.1)) +
  scale_x_continuous(name = "c. Average number of public \nCEmOC facilities reachable within 60 minutes", 
                     expand = c(0.02, 0), breaks = seq(0, 13,2), position="top") +
  scale_y_continuous(name = "", breaks = order$order, labels = order$city) +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  # scale_shape_manual(name = "",
  #                  labels = c("Public facilities only"), 
  #                  values = c(16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10), 
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"), 
        legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1.5, reverse=TRUE))

en2 <- ggplot() +
  # geom_vline(xintercept=10, linetype="dotted") +
  # geom_segment(data=pub_line, aes(x=min, xend=max, y=order+0.15, yend=order+0.15)) +
  # geom_point(data=mean[facility_type=="public" & departure_time=="weekday6-8pm"], 
  #            aes(x=d05, y=order+0.15, color=as.integer(wiq), shape = facility_type), size=4, alpha=0.75) +
  geom_segment(data=both_line, aes(x=min, xend=max, y=order, yend=order)) +
  geom_point(data=mean[facility_type=="both" & departure_time=="weekday6-8pm"],
             aes(x=d05, y=order,  fill=as.integer(wiq)), 
             color = "black", shape=21, size=4, alpha=0.75,
             position=position_jitter(h=0, w=0.1)) +
  scale_x_continuous(name = "d. Average number of public or private \nCEmOC facilities reachable within 60 minutes", 
                     expand = c(0.02, 0), breaks = seq(0, 300, 50), position="top") +
  scale_y_continuous(name = "", breaks = order$order, labels = order$city) +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  theme_bw() +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10),         legend.position = "bottom", 
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1.5, reverse=TRUE))


