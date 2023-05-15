#______________________________________________________________________________#
#------------------ ALL TOWHEE SPEARMAN RANK CORRELATION ----------------------#
library(tidyr)
library(readr)


ALL_Syll_Loc_Data_XCandML <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_All_XCandML_raw_songdata_3.31.23.csv")
##In order to do analysis on breeding season samples only, read in the following file:
#ALL_Syll_Loc_Data_XCandML <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_breedingseasonONLY_XCandML_raw_songdata_4.1.23.csv")


################################################################################
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data


use_song_data_cols <- c(4:19)


spearman_stat_table = data.frame(matrix(ncol = 6, nrow = 16)) 
colnames_df_vector <- colnames(ALL_Syll_Loc_Data_XCandML[c(4:19)])
long_lat_names <- c("Lat_S", "Lat_p", "Lat_rho" , "Long_S", "Long_p", "Long_rho")
rownames(spearman_stat_table) <- colnames_df_vector
colnames(spearman_stat_table) <- long_lat_names


for (ii in 1:16) {
    i = use_song_data_cols[ii]
    data_temp = cor.test(ALL_Syll_Loc_Data_XCandML$Latitude, 
                         ALL_Syll_Loc_Data_XCandML[,i], method = c("spearman"), exact = FALSE)
    spearman_stat_table[ii,1] = data_temp$statistic
    spearman_stat_table[ii,2] = data_temp$p.value
    spearman_stat_table[ii,3] = data_temp$estimate
    data_temp = cor.test(ALL_Syll_Loc_Data_XCandML$Longitude, 
                         ALL_Syll_Loc_Data_XCandML[,i], method = c("spearman"), exact = FALSE)
    spearman_stat_table[ii,4] = data_temp$statistic
    spearman_stat_table[ii,5] = data_temp$p.value
    spearman_stat_table[ii,6] = data_temp$estimate
}

#______________________________________________________________________________#
###plot data
library(ggplot2)
library(ggpubr)

ALL_Syll_Loc_Data_XCandML_unordered <- ALL_Syll_Loc_Data_XCandML[sample(nrow(ALL_Syll_Loc_Data_XCandML)),]


bout_duration_ms_plot_spearman <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, bout_duration_ms)) +
  geom_point(aes(color = Species), alpha = 0.8) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Bout Duration (ms))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Bout Duration") +
  #stat_cor(method="spearman") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) +
  theme_classic()

num_syllables_plot_spearman <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_syllables)) +
  geom_point(aes(color = Species), alpha = 0.8) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (# of Sylls.)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Syllables") +
  #stat_cor(method="spearman") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) +
  theme_classic()

avg_syllable_duration_ms_plot_spearman <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_syllable_duration_ms)) +
  geom_point(aes(color = Species), alpha = 0.8) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Avg. Syll. Dur. (ms))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllable Duration") +
  #stat_cor(method="spearman") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) +
  theme_classic()
  
num_unique_syllables_plot_spearman <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_unique_syllables)) +
  geom_point(aes(color = Species), alpha = 0.8) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (# of Unique Sylls.)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Unique Syllables") +
  #stat_cor(method="spearman") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) +
  theme_classic()

avg_sylls_upper_freq_Hz_plot_spearman <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_sylls_upper_freq_Hz)) +
  geom_point(aes(color = Species), alpha = 0.8) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Avg. Syll. Upper Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllables Upper Frequency") +
  #stat_cor(method="spearman") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo erythrophthalmus" = "#960019")) +
  theme_classic()

#______________________________________________________________________________#

###map data
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf")

#Largest Silence Duration
largestsildur_ggplot <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = ALL_Syll_Loc_Data_XCandML, aes(x = Longitude, y = Latitude, shape = Species, colour = num_syllable_per_bout_duration_1_ms), size = 3.5, alpha = 0.8) +
  coord_sf(xlim = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_color_viridis_c() +
  labs(colour="Log (Largest Silence Duration (ms))") +
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10))
largestsildur_ggplot
