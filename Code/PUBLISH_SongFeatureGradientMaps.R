


# Read in the dataset
ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1:22)]

ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data

################################################################################
library(ggplot2)
library(ggpubr)

#Bout Duration
BoutDuration <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = bout_duration_ms), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Bout Duration (ms)") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#BoutDuration
BoutDuration_legend <- get_legend(BoutDuration)
plot(BoutDuration_legend)

#Num Sylls
NumSylls <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = num_syllables), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Number of Syllables") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#NumSylls
NumSylls_legend <- get_legend(NumSylls)
plot(NumSylls_legend)

#Num Sylls per Bout Duration
NumSyllsperBoutDur <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = num_syllable_per_bout_duration_1_ms), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Number of Syllables per Bout Duration") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#NumSyllsperBoutDur
NumSyllsperBoutDur_legend <- get_legend(NumSyllsperBoutDur)
plot(NumSyllsperBoutDur_legend)

#Largest Syll Dur
LargSyllDur <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = largest_syllable_duration_ms), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Largest Syllable Duration") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#LargSyllDur
LargSyllDur_legend <- get_legend(LargSyllDur)
plot(LargSyllDur_legend)

#Smallest Syll Dur
SmallSyllDur <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = smallest_syllable_duration_ms), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Smallest Syllable Duration") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#SmallSyllDur
SmallSyllDur_legend <- get_legend(SmallSyllDur)
plot(SmallSyllDur_legend)

#Avg Syll Dur
AvgSyllDur <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = avg_syllable_duration_ms), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="Avg Syllable Duration") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#AvgSyllDur
AvgSyllDur_legend <- get_legend(AvgSyllDur)
plot(AvgSyllDur_legend)

#Num Unique Sylls
NumUniqueSylls <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = num_unique_syllables), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="NumUniqueSylls") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#NumUniqueSylls
NumUniqueSylls_legend <- get_legend(NumUniqueSylls)
plot(NumUniqueSylls_legend)

#Num Unique Sylls
NumSyllsperUniqueSylls <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = num_syllables_per_num_unique), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="NumSyllsperUniqueSylls") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#NumSyllsperUniqueSylls
NumSyllsperUniqueSylls_legend <- get_legend(NumSyllsperUniqueSylls)
plot(NumSyllsperUniqueSylls_legend)

#Avg Sylls Upper Freq
AvgSyllsUpperFreq <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = avg_sylls_upper_freq_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="AvgSyllsUpperFreq") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#AvgSyllsUpperFreq
AvgSyllsUpperFreq_legend <- get_legend(AvgSyllsUpperFreq)
plot(AvgSyllsUpperFreq_legend)

#Avg Sylls Lower Freq
AvgSyllsLowerFreq <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = avg_sylls_lower_freq_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="AvgSyllsLowerFreq") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#AvgSyllsLowerFreq
AvgSyllsLowerFreq_legend <- get_legend(AvgSyllsLowerFreq)
plot(AvgSyllsLowerFreq_legend)

#Max Sylls Freq
MaxSyllsFreq <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = max_sylls_freq_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="MaxSyllsFreq") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#MaxSyllsFreq
MaxSyllsFreq_legend <- get_legend(MaxSyllsFreq)
plot(MaxSyllsFreq_legend)

#Min Sylls Freq
MinSyllsFreq <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = min_sylls_freq_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="MinSyllsFreq") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#MinSyllsFreq
MinSyllsFreq_legend <- get_legend(MinSyllsFreq)
plot(MinSyllsFreq_legend)

#OverallSyllsFreqRange
OverallSyllsFreqRange <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = overall_sylls_freq_range_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="OverallSyllsFreqRange") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#OverallSyllsFreqRange
OverallSyllsFreqRange_legend <- get_legend(OverallSyllsFreqRange)
plot(OverallSyllsFreqRange_legend)

#LargestSyllsFreqRange
LargestSyllsFreqRange <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = largest_sylls_freq_range_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="LargestSyllsFreqRange") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#LargestSyllsFreqRange
LargestSyllsFreqRange_legend <- get_legend(LargestSyllsFreqRange)
plot(LargestSyllsFreqRange_legend)

#SmallestSyllsFreqRange
SmallestSyllsFreqRange <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = smallest_sylls_freq_range_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="SmallestSyllsFreqRange") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#SmallestSyllsFreqRange
SmallestSyllsFreqRange_legend <- get_legend(SmallestSyllsFreqRange)
plot(SmallestSyllsFreqRange_legend)

#AvgSyllsFreqRange
AvgSyllsFreqRange <- ggplot(ALL_Syll_Loc_Data_XCandML_data) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = Species, colour = avg_sylls_freq_range_Hz), size = 3.5, alpha = 0.85, show.legend = TRUE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Species", colour="AvgSyllsFreqRange") +
  scale_color_viridis_c() +
  theme (legend.position = "right")
#AvgSyllsFreqRange
AvgSyllsFreqRange_legend <- get_legend(AvgSyllsFreqRange)
plot(AvgSyllsFreqRange_legend)

################################################################################
#plot maps together
DurationSyllsFeatures <- ggarrange(BoutDuration, NumSylls, NumSyllsperBoutDur, LargSyllDur, 
                     SmallSyllDur, AvgSyllDur, NumUniqueSylls, NumSyllsperUniqueSylls,
                    labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                    common.legend = TRUE, 
                    ncol = 3, nrow = 3)
DurationSyllsFeatures #PDF: 19x17 landscape 

FrequencyFeatures <- ggarrange(AvgSyllsUpperFreq, AvgSyllsLowerFreq, MaxSyllsFreq, MinSyllsFreq, 
                     OverallSyllsFreqRange, LargestSyllsFreqRange, SmallestSyllsFreqRange,
                     AvgSyllsFreqRange,
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                     common.legend = TRUE, 
                     ncol = 3, nrow = 3)
FrequencyFeatures #PDF: 19x17 landscape

#plot legends
DurationSyllsFeatures_legends <- ggarrange(BoutDuration_legend, NumSylls_legend, NumSyllsperBoutDur_legend, 
                      LargSyllDur_legend, SmallSyllDur_legend, AvgSyllDur_legend, 
                      NumUniqueSylls_legend, NumSyllsperUniqueSylls_legend,
                      labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                      ncol = 3, nrow = 3)
DurationSyllsFeatures_legends #pdf: 10x12

FrequencyFeatures_legends <- ggarrange(AvgSyllsUpperFreq_legend, AvgSyllsLowerFreq_legend, MaxSyllsFreq_legend, 
                                       MinSyllsFreq_legend, OverallSyllsFreqRange_legend, LargestSyllsFreqRange_legend,
                                       SmallestSyllsFreqRange_legend, AvgSyllsFreqRange_legend,
                                           labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                           ncol = 3, nrow = 3)
FrequencyFeatures_legends
