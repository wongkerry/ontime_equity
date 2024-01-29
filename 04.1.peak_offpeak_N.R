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

diff <- s2[departure_time %in% c("weekday6-8pm", "weekend1-3am")]
diff <- diff[facility_type=="public", .(s2cellid, city, departure_time, n60, wiq)]
diff <- diff[departure_time == "weekday6-8pm", departure_time := "peak"]
diff <- diff[departure_time == "weekend1-3am", departure_time := "offpeak"]

diff <- diff[, .(mean = floor(mean(n60))), by = .(city, wiq, departure_time)]
diff <- dcast(diff, city + wiq ~ departure_time, value.vars = "mean")
diff[, more := offpeak-peak]

popN <- ggplot() + theme_bw() +
  labs(subtitle = "b. No. of public CEmOC facilities reachable within 60 minutes") +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  geom_point(data=diff, aes(x=peak, y=offpeak, fill = as.integer(wiq)), 
              shape=21, color = "NA", size=4, alpha=0.75, 
             position=position_jitter(h=0.2, w=0.2)) +
  geom_text(data=diff[more>=3],
    aes(x=peak, y=offpeak, label=city), 
    position=position_jitter(h=0.2, w=0.2), 
    check_overlap = T, family="Segoe UI", 
  ) +
  scale_x_continuous(name = "No. of pubilc CEmOC facilites \nreachable within 60 minutes (peak)",
                     expand = c(0, 0), limits = c(0, 17), breaks = seq(0, 17, 2)) +
  scale_y_continuous(name = "No. of pubilc CEmOC facilites \nreachable within 60 minutes (off-peak)", 
                     expand = c(0, 0), limits = c(0, 17), breaks = seq(0, 17, 2)) +
  scale_fill_distiller(name = "Relative wealth", palette = "Spectral", trans = "reverse", breaks = 1:5, 
                        labels = c("Q1 (least wealthy 20%)","Q2","Q3","Q4","Q5")) +
  theme_bw() +
  theme(text = element_text(size=11), 
        plot.subtitle = element_text(size=11), 
        legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1.5, reverse=TRUE))


pop_out <- ggpubr::ggarrange(popMTT, popN, nrow=1, 
                             common.legend = TRUE, legend="top")
pop_out <- annotate_figure(pop_out, top="Geographic accessiblity to the nearest public CEmOC facility \n in 15 cities in Nigeria by wealth quintile of S2 cells – peak vs. off-peak")
pop_out <- pop_out + bgcolor("white") + border("white")

