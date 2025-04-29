library(ggplot2)
library(dplyr)

ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML_data$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_data$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML_data$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_data$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "1",
    "0"
  ))

# Calculate percentages for each file type within each species
percent_data_filetype <- ALL_Syll_Loc_Data_XCandML_data %>%
  group_by(Species, OriginalFileType) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

percent_data_samplingrate <- ALL_Syll_Loc_Data_XCandML_data %>%
  group_by(Species, SamplingRate) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

percent_data_source <- ALL_Syll_Loc_Data_XCandML_data %>%
  group_by(Species, Source) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Stacked bar chart of file type
ggplot(percent_data_filetype, aes(x = Species, y = percent, fill = OriginalFileType)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "File Type Distribution for Eastern and Spotted Towhee",
       x = "Species",
       y = "Percentage",
       fill = "File Type") +
  theme_minimal()

# Stacked bar chart of source
ggplot(percent_data_source, aes(x = Species, y = percent, fill = Source)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Source Distribution for Eastern and Spotted Towhee",
       x = "Species",
       y = "Percentage",
       fill = "Source") +
  theme_minimal()

# Stacked bar chart of sampling rate
ggplot(percent_data_samplingrate, aes(x = Species, y = percent, fill = as.factor(SamplingRate))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Sampling Rate Distribution for Eastern and Spotted Towhee",
       x = "Species",
       y = "Percentage",
       fill = "Sampling Rate") +
  theme_minimal()



