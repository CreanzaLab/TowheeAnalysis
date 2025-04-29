#read the data
ALL_Syll_Loc_Data_XCandML_unique <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#for Breeding use the following:
#ALL_XCandML_songdata_breedingseasonONLY_unique <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")


#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML_unique$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_unique$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML_unique$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_unique$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML_unique <- ALL_Syll_Loc_Data_XCandML_unique %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "converted",
    "unconverted"
  ))

####for BREEDING SEASON ONLY do this: 
#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_XCandML_songdata_breedingseasonONLY_unique$FileTypeConversionStatus <- ifelse(ALL_XCandML_songdata_breedingseasonONLY_unique$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_XCandML_songdata_breedingseasonONLY_unique$SamplingRateConversionStatus <- ifelse(ALL_XCandML_songdata_breedingseasonONLY_unique$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_XCandML_songdata_breedingseasonONLY_unique <- ALL_XCandML_songdata_breedingseasonONLY_unique %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "converted",
    "unconverted"
  ))

ALL_Syll_Loc_Data_XCandML_unique <- ALL_XCandML_songdata_breedingseasonONLY_unique
################################################################################
model_results <- data.frame(
  intercept_t_value = numeric(),
  intercept_p_value = numeric(),
  Latitude_t_value = numeric(),
  Latitude_p_value = numeric(),
  Longitude_t_value = numeric(),
  Longitude_p_value = numeric(),
  Soecies_t_value = numeric(),
  Soecies_p_value = numeric(),
  Conversion_t_value = numeric(),
  Congersion__p_value = numeric(),
  stringsAsFactors = FALSE
)

model_results_aic_conversion <- data.frame(
  AIC = numeric(),
  stringsAsFactors = FALSE
)

#bout_duration_ms
#GLM uses one bout from each recording
bout_duration_ms.glm <- glm(bout_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(bout_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))
colnames(model_results) <- c("intercept_t_value","intercept_p_value","Latitude_t_value","Latitude_p_value","Longitude_t_value","Longitude_p_value","Species_t_value","Species_p_value","Conversion_t_value","Conversion_p_value")
colnames(model_results_aic_conversion) <- c("AIC_Conv")

#num_syllables
num_syllables.glm <- glm(num_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllables.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#num_syllable_per_bout_duration_1_ms
num_syllable_per_bout_duration_1_ms.glm <- glm(num_syllable_per_bout_duration_1_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllable_per_bout_duration_1_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#largest_syllable_duration_ms
largest_syllable_duration_ms.glm <- glm(largest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(largest_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#smallest_syllable_duration_ms
smallest_syllable_duration_ms.glm <- glm(smallest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(smallest_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#avg_syllable_duration_ms
avg_syllable_duration_ms.glm <- glm(avg_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#num_unique_syllables
num_unique_syllables.glm <- glm(num_unique_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_unique_syllables.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#num_syllables_per_num_unique
num_syllables_per_num_unique.glm <- glm(num_syllables_per_num_unique ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllables_per_num_unique.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#avg_sylls_upper_freq_Hz
avg_sylls_upper_freq_Hz.glm <- glm(avg_sylls_upper_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_upper_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#avg_sylls_lower_freq_Hz
avg_sylls_lower_freq_Hz.glm <- glm(avg_sylls_lower_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_lower_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#max_sylls_freq_Hz
max_sylls_freq_Hz.glm <- glm(max_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(max_sylls_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#min_sylls_freq_Hz
min_sylls_freq_Hz.glm <- glm(min_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(min_sylls_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#overall_sylls_freq_range_Hz
overall_sylls_freq_range_Hz.glm <- glm(overall_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(overall_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#largest_sylls_freq_range_Hz
largest_sylls_freq_range_Hz.glm <- glm(largest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(largest_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#smallest_sylls_freq_range_Hz
smallest_sylls_freq_range_Hz.glm <- glm(smallest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(smallest_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#avg_sylls_freq_range_Hz
avg_sylls_freq_range.glm <- glm(avg_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_freq_range.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
model_results_aic_conversion <- rbind(model_results_aic_conversion, c(AIC = summary_model[["aic"]]))

#write.csv(model_results,file = "~/PUBLISH_towhee_glm_model_results_unique_BREEDING_10.12.24.csv")


#testing for inflation to assess if there is collinearity 
library(car)
vif(avg_sylls_freq_range.glm)




