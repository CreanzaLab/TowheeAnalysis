require(dplyr)
library(ggplot2)
library(data.table)

ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#for analysis of only breeding season, read in following file:
#ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

ALL_Syll_Loc_Data_XCandML_fraction <- subset(ALL_Syll_Loc_Data_XCandML, select = c(1, 20, 3)) #grab columns you need

###remove decimals #flooring so everything 1.00 to 1.99.. = 1
ALL_Syll_Loc_Data_XCandML_fraction$Longitude = sub("\\..*", "", ALL_Syll_Loc_Data_XCandML_fraction$Longitude)

count_longitude <- ALL_Syll_Loc_Data_XCandML_fraction %>% count(Species, Longitude, name = "count") #count of each species at each degree longitude

#count total at each longitude
count_longitude_total <- ALL_Syll_Loc_Data_XCandML_fraction %>% count(Longitude, name = "counttotal")

#merge count_longitude and count_longitude_total by degree longitude 
plot_fraction_per_degree <- merge(count_longitude, count_longitude_total ,by=c('Longitude'))

plot_fraction_per_degree$count <- as.numeric(plot_fraction_per_degree$count)
plot_fraction_per_degree$counttotal <- as.numeric(plot_fraction_per_degree$counttotal)


#divide count by count total for each longitude
plot_fraction_per_degree <- plot_fraction_per_degree                               # Replicating data
plot_fraction_per_degree$countfraction <- plot_fraction_per_degree$count / plot_fraction_per_degree$counttotal

###sort by degree longitude
#change longitude to numeric
plot_fraction_per_degree$Longitude <- as.numeric(plot_fraction_per_degree$Longitude)

#plot
ggplot(plot_fraction_per_degree, aes(x=Longitude, y=countfraction, group=Species)) +
  geom_line(aes(color=Species)) +
  scale_color_manual(values = c("#960019", "#5C6B9C")) +                         #PNG = 950 x 550 #PDF 9.5 x 5.5
  theme_classic() +
  geom_vline(xintercept = -102) +
  geom_vline(xintercept = -91)

################################################################################
#Put those found in zone of overlap in a new dataframe

#limit by longitude coordinates
hybridzonedata <- ALL_Syll_Loc_Data_XCandML$Longitude >= -102 & ALL_Syll_Loc_Data_XCandML$Longitude <= -91    # Identify rows in range

final_hybridzonedata <- ALL_Syll_Loc_Data_XCandML[hybridzonedata, ]


################################################################################
######making column of hybrid zone species names
ALL_Syll_Loc_Data_XCandML$SpeciesOverlap = ALL_Syll_Loc_Data_XCandML$Species

ALL_Syll_Loc_Data_XCandML$SpeciesOverlap[which(ALL_Syll_Loc_Data_XCandML$Recording_ID %in% final_hybridzonedata$Recording_ID & ALL_Syll_Loc_Data_XCandML$Species == "Pipilo erythrophthalmus")]= "Pipilo erythrophthalmus Overlap"

ALL_Syll_Loc_Data_XCandML$SpeciesOverlap[which(ALL_Syll_Loc_Data_XCandML$Recording_ID %in% final_hybridzonedata$Recording_ID & ALL_Syll_Loc_Data_XCandML$Species == "Pipilo maculatus")]= "Pipilo maculatus Overlap"

#write.csv(ALL_Syll_Loc_Data_XCandML, "~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv") #for all samples
#write.csv(ALL_Syll_Loc_Data_XCandML, "~/PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_4.1.23.csv") #for breeding season samples only 
