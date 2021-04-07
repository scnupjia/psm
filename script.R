### Figure 1a ###
library(rworldmap)
library(ggplot2)
library(RColorBrewer)
library(scatterpie)
ggplot() + 
 geom_polygon(data = worldmap, aes(x = long,y = lat,group = group),fill = "grey",color = "grey") +
 geom_scatterpie(aes(lon, lat, r = radius),
 data = piedat, 
 cols = c("Rhizosphere", "Bulk","Sediment","Roots","Water", "Composts","Municipal.solid.waste","Rock")) +
 geom_scatterpie_legend(piedat$radius, x = -160, y = -55, n = 3, labeller = function(x) x) +
 theme_void() +
 coord_sf(crs = 102003, datum = NA) +
 scale_fill_manual(
 breaks = c("Rhizosphere", "Bulk","Sediment","Roots","Water", "Composts","Municipal.solid.waste","Rock"),
 labels = c("Rhizosphere", "Bulk","Sediment","Roots","Water", "Composts","Municipal.solid.waste","Rock"),
 values = c("Rhizosphere" = "#4DAF4A",
 "Bulk" = "#377EB8",
 "Sediment" = "#E41A1C",
 "Roots" = "#984EA3",
 "Water" = "#FFFF33",
 "Composts" = "#FF7F00",
 "Municipal.solid.waste" = "#A65628",
 "Rock" = "#F781BF")) +
 labs(fill = "Sample type")

### Figure 1b ###
ggplot(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lTP), ], aes(x = lTP, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Sample.type))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Log Total P (mg/kg)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Rhizosphere soil" = "#4DAF4A",
 "Bulk soil" = "#377EB8",
 "Sediment" = "#E41A1C",
 "Roots" = "#984EA3",
 "Composts" = "#FF7F00",
 "Municipal solid waste" = "#A65628",
 "Rock" = "#F781BF")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lTP), ]$lTP-0.4), max(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lTP), ]$lTP+0.4)), ylim = c(3, 8.8))
 
### Figure 1c ###
ggplot(BRhiSRoCMR[complete.cases(BRhiSRoCMR$pH), ], aes(x = pH, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Sample.type))+
 labs(x = "pH", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Rhizosphere soil" = "#4DAF4A",
 "Bulk soil" = "#377EB8",
 "Sediment" = "#E41A1C",
 "Roots" = "#984EA3",
 "Composts" = "#FF7F00",
 "Municipal solid waste" = "#A65628",
 "Rock" = "#F781BF")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(BRhiSRoCMR[complete.cases(BRhiSRoCMR$pH), ]$pH-0.5), max(BRhiSRoCMR[complete.cases(BRhiSRoCMR$pH), ]$pH+0.5)), ylim = c(min(BRhiSRoCMR[complete.cases(BRhiSRoCMR$pH), ]$PSM-0.5), max(BRhiSRoCMR[complete.cases(BRhiSRoCMR$pH), ]$PSM+0.2)))
 
### Figure 1d ###
ggplot(BRhiSRoCMR, aes(x = MAT, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Sample.type))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Annual mean temperature (буC)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Rhizosphere soil" = "#4DAF4A",
 "Bulk soil" = "#377EB8",
 "Sediment" = "#E41A1C",
 "Roots" = "#984EA3",
 "Composts" = "#FF7F00",
 "Municipal solid waste" = "#A65628",
 "Rock" = "#F781BF")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(BRhiSRoCMR$MAT-2), max(BRhiSRoCMR$MAT+2)), ylim = c(2.2, 9.2))

### Figure 2a ###
ggplot() + 
 geom_path(data = country_df, 
 aes(x = long, y = lat, group = group), colour = "grey") + 
 geom_polygon(data = china.map, 
 aes(x = long, y = lat, group = group), colour = "grey") + 
 coord_quickmap() +
 geom_scatterpie(aes(lon, lat, r = radius/3),
 data = piedat, 
 cols = c("Farmland", "Forest","Grassland", "Gobi", "Mined")) + 
 geom_scatterpie_legend(piedat$radius, x = 85, y = 20, n = 15, labeller = function(x) x) +
 theme_bw() +
 scale_fill_manual(
 breaks = c("Farmland", "Forest", "Gobi", "Grassland", "Mined"),
 labels = c("Farmland", "Forest", "Gobi", "Grassland", "Mined"),
 values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
								"Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 labs(fill = "Habitat type")
 
### Figure 2b ###
ggplot(dat.china, aes(x = TP, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Total P (mg/kg)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$TP-50), max(dat.china$TP+50)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
 
### Figure 2c ###
ggplot(dat.china, aes(x = lAP, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Log Available P (mg/kg)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$lAP-0.1), max(dat.china$lAP+0.1)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
 
### Figure 2d ###
ggplot(dat.china, aes(x = pH, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 labs(x = "pH", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$pH-0.2), max(dat.china$pH+0.2)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
 
### Figure 2e ###
ggplot(dat.china, aes(x = lNO3, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Log NO3--N (mg/kg)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$lNO3-0.2), max(dat.china$lNO3+0.2)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
 
### Figure 2f ###
ggplot(dat.china[dat.china$lDOC>0,], aes(x = lDOC, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Log DOC (mg/kg)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$lDOC-0.1), max(dat.china$lDOC+0.1)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
 
### Figure 2g ###
ggplot(dat.china, aes(x = MAT, y = PSM)) + 
 geom_point(pch = 1, size = 3, aes(color = Habitat))+
 geom_smooth(method = lm, se = FALSE, color = "darkblue", size = 1.6)+
 labs(x = "Annual mean temperature (буC)", y = "Log Population density of PSM")+
 scale_color_manual(values = c("Farmland" = "#f4cd41",
 "Forest" = "#1d928c",
 "Grassland" = "#6d92cb",
 "Gobi" = "#cdab7f", 
 "Mined land" = "#ed7148")) +
 theme_bw() +
 theme(legend.position = "none",
 text = element_text(size = 14),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 12),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 12, hjust = 1),
 plot.margin = unit(c(1,1,1,1), "lines")) +
 coord_cartesian(xlim = c(min(dat.china$MAT-0.5), max(dat.china$MAT+0.5)), ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))

### Figure 3 ###
#Figure 3a and b were produced by SigmaPlot 14.0
#Phylogenies in Figure 3c and d were constructed by RAxML with the following script and visualized by iTOL v4

### Figure 4a ###
#Figure 4a was produced by Adobe Illustrator CS6 and SigmaPlot 14.0

### Figure 4b ###
ggplot(pH, aes(x = group, y = average, fill = group)) + 
 geom_bar(stat = "identity", width = 0.8) +
 theme_bw() +
 geom_errorbar(aes(ymin = average-se, ymax = average+se), width = .2, position = position_dodge(.9)) +
 scale_fill_manual(values = brewer.pal(n = 3, name = "PRGn")[c(1,3)])+
 labs(y = "Soil pH (average)") +
 theme(legend.position = "none",
 text = element_text(size = 24),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 24),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 24, hjust = 1)) +
 scale_x_discrete(labels = c("Positive group (n = 29)", "Negative/no effect group (n = 31)")) +
 coord_cartesian(ylim = c(0, 8))
 
### Figure 4c ###
ggplot(AP, aes(x = group, y = average, fill = group)) + 
 geom_bar(stat = "identity", width = 0.8) +
 theme_bw() +
 geom_errorbar(aes(ymin = average-se, ymax = average+se), width = .2, position = position_dodge(.9)) +
 scale_fill_manual(values = brewer.pal(n = 3, name = "PRGn")[c(1,3)])+
 labs(y = "Available P in soil (mg/kg, average)") +
 theme(legend.position = "none",
 text = element_text(size = 24),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 24),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 24, hjust = 1)) +
 scale_x_discrete(labels = c("Positive group (n = 21)", "Negative/no effect group (n = 14)")) +
 coord_cartesian(ylim = c(0, 27))
 
### Figure 4d ###
ggplot(crop, aes(x = group, y = percentage, fill = group)) + 
 geom_bar(stat = "identity", width = 0.8) +
 theme_bw() +
 scale_fill_manual(values = pal_locuszoom()(7)[c(1,2,3,4)])+
 labs(y = "Percentage of positive effect case (%)") +
 theme(legend.position = "none",
 text = element_text(size = 24),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 24),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 24, hjust = 1)) +
 scale_x_discrete(labels = c("Chickpea (n = 31)", "Maize (n = 25)", "Wheat (n = 17)", "All crops (n = 95)")) +
 coord_cartesian(ylim = c(0, 86))

### Figure 4e ###
ggplot(strains, aes(x = group, y = percentage, fill = group)) + 
 geom_bar(stat = "identity", width = 0.8) +
 theme_bw() +
 scale_fill_manual(values = pal_locuszoom()(7)[c(5,6,7)])+
 labs(y = "Percentage of positive effect case (%)") +
 theme(legend.position = "none",
 text = element_text(size = 24),
 axis.text.x = element_text(face = "plain", color = "#000000", size = 24),
 axis.text.y = element_text(face = "plain", color = "#000000", size = 24, hjust = 1)) +
 scale_x_discrete(labels = c("Fungal strains (n = 6)", "Bacterial strains (n = 89)", "All strains (n = 95)"))+
 coord_cartesian(ylim = c(0, 56))

### Figure 5a, b, c and d ###
#Phylogenies in Figure 5a, b, c and d were constructed by RAxML with the following script and visualized by iTOL v4
#raxmlHPC-PTHREADS -f acid.phos.aln -m PROTGAMMALG -s combo.aln -N 100 -p 12345 -x 12345 -T 20 -n combo
#raxmlHPC-PTHREADS -f alka.phos.aln -m PROTGAMMALG -s combo.aln -N 100 -p 12345 -x 12345 -T 20 -n combo
#raxmlHPC-PTHREADS -f phytase.aln -m PROTGAMMALG -s combo.aln -N 100 -p 12345 -x 12345 -T 20 -n combo
#raxmlHPC-PTHREADS -f glucose.aln -m PROTGAMMALG -s combo.aln -N 100 -p 12345 -x 12345 -T 20 -n combo

### Figure 1Sa ###
ggplot(BRhiSRoCW, aes(x=Sample.type, y=PSM, fill=Sample.type)) + 
      geom_boxplot() +
      geom_jitter(shape=1, size=3, aes(color=Sample.type), position=position_jitter(0.2)) +
	  theme_bw() +
      theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
	  scale_color_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                               "Bulk soil"= "#377EB8",
                               "Sediment"= "#E41A1C",
                               "Roots"= "#984EA3",
                               "Water"= "#FFFF33",
                               "Composts"="#FF7F00")) +
      scale_fill_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                               "Bulk soil"= "#377EB8",
                               "Sediment"= "#E41A1C",
                               "Roots"= "#984EA3",
                               "Water"= "#FFFF33",
                               "Composts"="#FF7F00")) +
      labs(x = "Sample type", y = "Log Population density of PSM") +
      scale_x_discrete(labels=c("Composts", "Rhizosphere soil", "Bulk soil", "Roots", "Sediment","Water")) +
	  scale_y_continuous(breaks=c(4,6,8)) +
	  coord_cartesian(ylim = c(2.5, 9.5))
	  
### Figure 1Sb ###
ggplot(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lAP), ], aes(x=lAP, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Sample.type))+
  labs(x="Log Available P (mg/kg)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                                                     "Bulk soil"= "#377EB8",
                                                     "Sediment"= "#E41A1C",
                                                     "Roots"= "#984EA3",
                                                     "Composts"="#FF7F00",
                                                     "Municipal solid waste"="#A65628",
                                                     "Rock"="#F781BF")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lAP), ]$lAP-0.3), max(BRhiSRoCMR[complete.cases(BRhiSRoCMR$lAP), ]$lAP+0.3)), ylim = c(2.5, 8.5))
  
### Figure 1Sc ###
ggplot(BRhiSRoCMR, aes(x=MAP, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Sample.type))+
  labs(x="Annual precipitation (mm)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                                                     "Bulk soil"= "#377EB8",
                                                     "Sediment"= "#E41A1C",
                                                     "Roots"= "#984EA3",
                                                     "Composts"="#FF7F00",
                                                     "Municipal solid waste"="#A65628",
                                                     "Rock"="#F781BF")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(BRhiSRoCMR$MAP-260), max(BRhiSRoCMR$MAP+260)), ylim = c(2.5, 9.5))

### Figure 1Sd ###
ggplot(BRhiSRoCMR, aes(x=lat, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Sample.type))+
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  labs(x="Latitude (бу)", y = "Log Population density of PSM") +
  scale_color_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                                                     "Bulk soil"= "#377EB8",
                                                     "Sediment"= "#E41A1C",
                                                     "Roots"= "#984EA3",
                                                     "Composts"="#FF7F00",
                                                     "Municipal solid waste"="#A65628",
                                                     "Rock"="#F781BF")) +
  coord_cartesian(xlim = c(min(BRhiSRoCMR$lat-5), max(BRhiSRoCMR$lat+5)), ylim = c(2.2, 9.2))

### Figure 1Se ###
ggplot(BRhiSRoCMR, aes(x=long, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Sample.type))+
  labs(x="Longitude (бу)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Rhizosphere soil"= "#4DAF4A",
                                                     "Bulk soil"= "#377EB8",
                                                     "Sediment"= "#E41A1C",
                                                     "Roots"= "#984EA3",
                                                     "Composts"="#FF7F00",
                                                     "Municipal solid waste"="#A65628",
                                                     "Rock"="#F781BF")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(BRhiSRoCMR$long-20), max(BRhiSRoCMR$long+20)), ylim = c(2.2, 9.2))
  
### Figure 2Sa ###
ggplot(dat.china, aes(x=Habitat, y=PSM, fill=Habitat)) + 
        geom_boxplot() +
		theme_bw() +
        geom_jitter(shape=1, size=3, aes(color=Habitat), position=position_jitter(0.2)) +
        theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=4),
             plot.margin = unit(c(1,1,1,1), "lines")) +
        scale_color_manual(values = c("Farmland"="#f4cd41",
                                "Forest"= "#1d928c",
                                "Grassland"= "#6d92cb",
								"Gobi"= "#cdab7f", 
                                "Mined land"= "#ed7148")) +
        scale_fill_manual(values = c("Farmland"="#f4cd41",
                                "Forest"= "#1d928c",
                                "Grassland"= "#6d92cb",
								"Gobi"= "#cdab7f", 
                                "Mined land"= "#ed7148")) +
        labs(x = "Habitat", y = "Log Population density of PSM") +
        scale_x_discrete(labels=c("Farmland", "Forest", "Grassland", "Gobi", "Mined land")) +
        coord_cartesian(ylim = c(min(dat.china$PSM-0.2), max(dat.china$PSM+0.2)))
		
### Figure 2Sb ###
ggplot(dat, aes(x=lEC, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  geom_smooth(method=lm, se=FALSE, color = "darkblue", size = 1.6)+
  labs(x="Log EC (S/m)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$lEC-0.06), max(dat$lEC+0.06)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Sc ###
ggplot(dat, aes(x=lNH4, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  labs(x="Log NH4+-N (mg/kg)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$lNH4-0.05), max(dat$lNH4+0.05)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Sd ###
ggplot(dat, aes(x=lWSOC, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  labs(x="Log WSOC (mg/kg)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$lWSOC-0.2), max(dat$lWSOC+0.2)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Se ###
ggplot(dat, aes(x=MAP, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  geom_smooth(method=lm, se=FALSE, color = "darkblue", size = 1.6)+
  labs(x="Annual precipitation (mm)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$MAP-80), max(dat$MAP+80)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Sf ###
ggplot(dat, aes(x=lat, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  geom_smooth(method=lm, se=FALSE, color = "darkblue", size = 1.6)+
  labs(x="Latitude (бу)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$lat-2), max(dat$lat+2)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Sg ###
ggplot(dat, aes(x=long, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  geom_smooth(method=lm, se=FALSE, color = "darkblue", size = 1.6)+
  labs(x="Longitude (бу)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$long-2), max(dat$long+2)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))
  
### Figure 2Sh ###
ggplot(dat, aes(x=lElevation, y=PSM)) + 
  geom_point(pch=1, size=3, aes(color=Habitat))+
  geom_smooth(method=lm, se=FALSE, color = "darkblue", size = 1.6)+
  labs(x="Log Elevation (m)", y = "Log Population density of PSM")+
  scale_color_manual(values = c("Farmland"="#f4cd41",
                                                   "Forest"= "#1d928c",
                                                   "Grassland"= "#6d92cb",
                                                   "Gobi"= "#cdab7f", 
                                                   "Mined land"= "#ed7148")) +
  theme_bw() +
  theme(legend.position="none",
             text = element_text(size=14),
             axis.text.x = element_text(face="plain", color="#000000", size=12),
             axis.text.y = element_text(face="plain", color="#000000", size=12, hjust=1),
             plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_cartesian(xlim = c(min(dat$lElevation-0.2), max(dat$lElevation+0.2)), ylim = c(min(dat$PSM-0.2), max(dat$PSM+0.2)))