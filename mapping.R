library(RColorBrewer)
library(maptools)
library(sp)


ug_map<-rgdal::readOGR("./datasources/uganda_districts_2019_i.shp")
ug_map@data <- as.data.frame(apply(ug_map@data, 2, function(x) iconv(x, "latin1", "UTF-8"))) %>%
  merge(dist_sum, by = "DName2019", all = TRUE) 
cut <- cut(as.numeric(ug_map1$Freq), breaks = c(0, 25, 50, 75, 100, 150, 200))
ug_map$cut<- cut
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(6)
cutColors <- cut(as.numeric(ug_map1$Freq), breaks = c(0, 25, 50, 75, 100, 150,200), labels = colors)
ug_map$colors<-cutColors

plot(ug_map, col = as.character(ug_map@data$colors))
legend("bottomright", cex = 0.7, legend = levels(ug_map$cut), fill = colors, 
       title = "Suspect Case No.")


