###plot data
library(ggplot2)
library(ggpubr)

ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML <- ALL_Syll_Loc_Data_XCandML[,c(1:22)]

#log transform song data:
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data

ALL_Syll_Loc_Data_XCandML_unordered <- ALL_Syll_Loc_Data_XCandML[sample(nrow(ALL_Syll_Loc_Data_XCandML)),]

bout_duration_ms_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, bout_duration_ms)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Bout Duration (ms))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Bout Duration") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
bout_duration_ms_plot

num_syllables_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_syllables)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (# of Sylls.)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Syllables") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
num_syllables_plot

rate_syllable_production_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_syllable_per_bout_duration_1_ms)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Rate of Syllable Production)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Rate of Syllable Production") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
rate_syllable_production_plot

largest_syll_dur_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, largest_syllable_duration_ms)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (largest Syll dur)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Largest Syll Duration") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
largest_syll_dur_plot

smallest_syll_dur_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, smallest_syllable_duration_ms)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (smallest Syll dur)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Smallest Syll Duration") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
smallest_syll_dur_plot

avg_syllable_duration_ms_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_syllable_duration_ms)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Avg. Syll. Dur. (ms))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllable Duration") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
avg_syllable_duration_ms_plot

num_unique_syllables_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_unique_syllables)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (# of Unique Sylls.)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Number of Unique Syllables") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
num_unique_syllables_plot

num_syllables_per_num_unique_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, num_syllables_per_num_unique)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (# sylls per Unique Sylls.)") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "num_syllables_per_num_unique") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
num_syllables_per_num_unique_plot

avg_sylls_upper_freq_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_sylls_upper_freq_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Avg. Syll. Upper Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllables Upper Frequency") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
avg_sylls_upper_freq_Hz_plot

avg_sylls_lower_freq_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_sylls_lower_freq_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Avg. Syll. Lower Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Average Syllables lower Frequency") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
avg_sylls_lower_freq_Hz_plot

max_sylls_freq_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, max_sylls_freq_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (Max. Syll. Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "Max Syllables Frequency") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
max_sylls_freq_Hz_plot

min_sylls_freq_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, min_sylls_freq_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (min. Syll. Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "min Syllables Frequency") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
min_sylls_freq_Hz_plot

overall_sylls_freq_range_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, overall_sylls_freq_range_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (overall Syll. Freq.))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "overall Syllables Frequency") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
overall_sylls_freq_range_Hz_plot

largest_sylls_freq_range_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, largest_sylls_freq_range_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (largest Syll. Freq. range))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "largest Syll. Freq. range") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
largest_sylls_freq_range_Hz_plot

smallest_sylls_freq_range_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, smallest_sylls_freq_range_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (smallest Syll. Freq. range))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "smallest Syll. Freq. range") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
smallest_sylls_freq_range_Hz_plot

avg_sylls_freq_range_Hz_plot <- ggplot(data=ALL_Syll_Loc_Data_XCandML_unordered, aes(Longitude, avg_sylls_freq_range_Hz)) +
  geom_point(aes(color = SpeciesOverlap), alpha = 0.8, show.legend = FALSE) +
  geom_smooth(span = 5, col="black") +
  ylab("Log (avg Syll. Freq. range))") +
  theme(plot.title = element_text(size=15, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 12, hjust = 0, face = "bold")) +
  labs(subtitle = "avg Syll. Freq. range") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  theme_classic()
avg_sylls_freq_range_Hz_plot

####combine the plots
Longitude_variation_16panelfigure <- ggarrange(bout_duration_ms_plot, num_syllables_plot,
                                               rate_syllable_production_plot, largest_syll_dur_plot,
                                               smallest_syll_dur_plot, avg_syllable_duration_ms_plot,
                                               num_unique_syllables_plot, num_syllables_per_num_unique_plot,
                                               avg_sylls_upper_freq_Hz_plot, avg_sylls_lower_freq_Hz_plot,
                                               max_sylls_freq_Hz_plot, min_sylls_freq_Hz_plot,
                                               overall_sylls_freq_range_Hz_plot, largest_sylls_freq_range_Hz_plot,
                                               smallest_sylls_freq_range_Hz_plot, avg_sylls_freq_range_Hz_plot,
                                           labels = c("A","B","C","D","E","F","G","H",
                                                      "I","J","K","L","M","N","O","P"),
                                           nrow = 4, ncol = 4)
Longitude_variation_16panelfigure  #save PDF 14 x 13 

