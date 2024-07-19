library(dplyr)
library(lme4)

################################################################################
################################################################################
################################################################################
############################# GLM/GLMM FOR REVIEWS #############################

#read the data

#ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType.csv")
ALL_Syll_Loc_Data_XCandML_unique <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#ALL_XCandML_songdata_breedingseasonONLY <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType.csv")
ALL_XCandML_songdata_breedingseasonONLY_unique <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

################################################################################
#################### PRE-EDIT DATA FOR GENERALIZED MODELS ###################### 
#do this for converted files combined with both 
#MODELS FOR CONVERTED vs. UNCONVERTED FOR EACH BOUT FOR EACH SONG FEATURE

#don't need to log transform if using a log link function in the GLM(M) 
#ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data

#All bouts (one or more from each recording)
#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML <- ALL_Syll_Loc_Data_XCandML %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "converted",
    "unconverted"
  ))


#Unique bouts (only one from each recording)
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

################################################################################
#GLM(M)s FOR EACH SONG FEATURE, INCLUDING LATITUDE, LONGITUDE, SPECIES, AND CONVERSION (i.e. does the converted subset have different song feature values

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


#bout_duration_ms
#GLM uses one bout from each recording
bout_duration_ms.glm <- glm(bout_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(bout_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
colnames(model_results) <- c("intercept_t_value","intercept_p_value","Latitude_t_value","Latitude_p_value","Longitude_t_value","Longitude_p_value","Species_t_value","Species_p_value","Conversion_t_value","Conversion_p_value")

#GLMM uses multiple bouts from each recording and treats recording ID as a random effect (did not use this one)
bout_duration_ms.glmm <- glmer(bout_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(bout_duration_ms.glmm)


#num_syllables
num_syllables.glm <- glm(num_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllables.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

num_syllables.glmm <- glmer(num_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(num_syllables.glmm)
##did not converge

#num_syllable_per_bout_duration_1_ms
num_syllable_per_bout_duration_1_ms.glm <- glm(num_syllable_per_bout_duration_1_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllable_per_bout_duration_1_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

num_syllable_per_bout_duration_1_ms.glmm <- glmer(num_syllable_per_bout_duration_1_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(num_syllable_per_bout_duration_1_ms.glmm)


#largest_syllable_duration_ms
largest_syllable_duration_ms.glm <- glm(largest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(largest_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

largest_syllable_duration_ms.glmm <- glmer(largest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(largest_syllable_duration_ms.glmm)


#smallest_syllable_duration_ms
smallest_syllable_duration_ms.glm <- glm(smallest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(smallest_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

smallest_syllable_duration_ms.glmm <- glmer(smallest_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(smallest_syllable_duration_ms.glmm)


#avg_syllable_duration_ms
avg_syllable_duration_ms.glm <- glm(avg_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_syllable_duration_ms.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

avg_syllable_duration_ms.glmm <- glmer(avg_syllable_duration_ms ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary_model <- summary(avg_syllable_duration_ms.glmm)


#num_unique_syllables
num_unique_syllables.glm <- glm(num_unique_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_unique_syllables.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

num_unique_syllables.glmm <- glmer(num_unique_syllables ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(num_unique_syllables.glmm)


#num_syllables_per_num_unique
num_syllables_per_num_unique.glm <- glm(num_syllables_per_num_unique ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(num_syllables_per_num_unique.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

num_syllables_per_num_unique.glmm <- glmer(num_syllables_per_num_unique ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(num_syllables_per_num_unique.glmm)


#avg_sylls_upper_freq_Hz
avg_sylls_upper_freq_Hz.glm <- glm(avg_sylls_upper_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_upper_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

avg_sylls_upper_freq_Hz.glmm <- glmer(avg_sylls_upper_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(avg_sylls_upper_freq_Hz.glmm)


#avg_sylls_lower_freq_Hz
avg_sylls_lower_freq_Hz.glm <- glm(avg_sylls_lower_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_lower_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

avg_sylls_lower_freq_Hz.glmm <- glmer(avg_sylls_lower_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(avg_sylls_lower_freq_Hz.glmm)


#max_sylls_freq_Hz
max_sylls_freq_Hz.glm <- glm(max_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(max_sylls_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

max_sylls_freq_Hz.glmm <- glmer(max_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(max_sylls_freq_Hz.glmm)


#min_sylls_freq_Hz
min_sylls_freq_Hz.glm <- glm(min_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(min_sylls_freq_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

min_sylls_freq_Hz.glmm <- glmer(min_sylls_freq_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(min_sylls_freq_Hz.glmm)


#overall_sylls_freq_range_Hz
overall_sylls_freq_range_Hz.glm <- glm(overall_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(overall_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

overall_sylls_freq_range_Hz.glmm <- glmer(overall_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(overall_sylls_freq_range_Hz.glmm)


#largest_sylls_freq_range_Hz
largest_sylls_freq_range_Hz.glm <- glm(largest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(largest_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

largest_sylls_freq_range_Hz.glmm <- glmer(largest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(largest_sylls_freq_range_Hz.glmm)


#smallest_sylls_freq_range_Hz
smallest_sylls_freq_range_Hz.glm <- glm(smallest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(smallest_sylls_freq_range_Hz.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

smallest_sylls_freq_range_Hz.glmm <- glmer(smallest_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(smallest_sylls_freq_range_Hz.glmm)


#avg_sylls_freq_range_Hz
avg_sylls_freq_range.glm <- glm(avg_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_unique, family = gaussian(link = "log"))
summary_model <- summary(avg_sylls_freq_range.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

avg_sylls_freq_range.glmm <- glmer(avg_sylls_freq_range_Hz ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus + (1 | Recording_ID), data = ALL_Syll_Loc_Data_XCandML, family = gaussian(link = "log"), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e8)))
summary(avg_sylls_freq_range.glmm)

write.csv(model_results,file = "towhee_glm_model_results_breeding.csv")




#WILCOXON OF MACAULAY VS XENOCANTO for Eastern & Spotted separately
ALL_Syll_Loc_Data_XCandML_level <- Eastern #also run with Spotted
ALL_Syll_Loc_Data_XCandML_level$Source <- factor(ALL_Syll_Loc_Data_XCandML_level$Source,     #reorder factor levels
                                                                   c("XC", "ML")) #change to converted vs unconverted for only EASTERN and then only SPOTTED

use_song_data_cols <- c(4:19)


wilcoxon_stat_table = data.frame(matrix(ncol = 2, nrow = 16))
colnames_df_vector <- colnames(ALL_Syll_Loc_Data_XCandML_level[c(4:19)])
data_names <- c("W", "p-val")
rownames(wilcoxon_stat_table) <- colnames_df_vector
colnames(wilcoxon_stat_table) <- data_names

for (ii in 1:16) {
  i = use_song_data_cols[ii]
  stat.test <-  
    wilcox.test(ALL_Syll_Loc_Data_XCandML_level[,i] ~ ALL_Syll_Loc_Data_XCandML_level$Source)
  wilcoxon_stat_table[ii,1] = stat.test$statistic
  wilcoxon_stat_table[ii,2] = stat.test$p.value
}


#whether each species has roughly the same proportion of converted bouts
###box plots 
#empirical p-value