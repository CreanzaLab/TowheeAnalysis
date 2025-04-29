library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(stats)

################################################################################
############################# Hybrid Sightings #################################

#HybridSightings_eBird <- read.csv("~/PUBLISH_HybridTowheeUniqueSightings.csv") 

BreedingSeason_HybridSightings_eBird <- read.csv("~/PUBLISH_HybridTowheeUniqueSightingsBreedingSeason.csv") #months: 4-8


#plot
world <- ne_countries(scale = "medium", returnclass = "sf") #obtaining map data for plotting


Hybrid_Sightings_Map <- ggplot(BreedingSeason_HybridSightings_eBird, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = "#805B87"), size = 1.5, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  scale_color_manual(values = "#805B87") +
  theme_classic() +
  geom_vline(xintercept = -102) +
  geom_vline(xintercept = -91)
Hybrid_Sightings_Map  #PDF 6.55 x 3.77

#Hybrid_Sightings_Map <- ggplot(HybridSightings_eBird, show.legend = FALSE) +
#  geom_sf(data = world, fill = "white") +
#  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = "#805B87"), size = 1.5, show.legend = FALSE) +
#  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
#  scale_shape_manual(values = c(16, 8)) +
#  theme(plot.title = element_text(size=15)) +
#  scale_color_manual(values = "#805B87") +
#  theme_classic()
#Hybrid_Sightings_Map  #PDF 6.55 x 3.77

#calculate proportion of sightings that fall within 102 W and 91 W 
sightings_within_overlapzone1 <- as.data.frame(between(as.numeric(BreedingSeason_HybridSightings_eBird$LONGITUDE), -102, -91))
table(sightings_within_overlapzone1)

proportion_sightings_within_overlapzone = 158/(158+45) #77.8% of sightings are found within zone of overlap
