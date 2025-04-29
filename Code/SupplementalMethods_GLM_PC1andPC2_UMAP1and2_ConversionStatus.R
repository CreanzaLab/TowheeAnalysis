################################################################################

ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

#log transform song data:
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data

################################################################################
################################################################################
####################### Principal Component Analysis ###########################

library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)

### this is centered (subtract mean so that new mean becomes zero) and scaled (divide by standard dev.)
### to correct for differences in measurements so scale no longer matters 
pca_resid <- prcomp(ALL_Syll_Loc_Data_XCandML[,4:19], scale = TRUE)

pca_resid

### standard deviation (measure of variability across that PC; eigenvalues)
### proportion of variance (proportion of all variability in original data explained by that PC)
summary(pca_resid)

### give PC1 and PC2 values for each sample in data and plot
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- cbind(ALL_Syll_Loc_Data_XCandML, pca_resid$x[, 1:2])
head(ALL_Syll_Loc_Data_XCandML_PC1_PC2)


#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML_PC1_PC2$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_PC1_PC2$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML_PC1_PC2$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_PC1_PC2$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- ALL_Syll_Loc_Data_XCandML_PC1_PC2 %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "converted",
    "unconverted"
  ))

################################################################################
model_results <- data.frame(
  intercept_t_value = numeric(),
  intercept_p_value = numeric(),
  Latitude_t_value = numeric(),
  Latitude_p_value = numeric(),
  Longitude_t_value = numeric(),
  Longitude_p_value = numeric(),
  Species_t_value = numeric(),
  Species_p_value = numeric(),
  Conversion_t_value = numeric(),
  Conversion__p_value = numeric(),
  stringsAsFactors = FALSE
)

#PC1
PC1.glm <- glm(PC1 ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_PC1_PC2, family = gaussian(link = "identity"))
summary_model <- summary(PC1.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
colnames(model_results) <- c("intercept_t_value","intercept_p_value","Latitude_t_value","Latitude_p_value","Longitude_t_value","Longitude_p_value","Species_t_value","Species_p_value","Conversion_t_value","Conversion_p_value")

#PC2
PC2.glm <- glm(PC2 ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_PC1_PC2, family = gaussian(link = "identity"))
summary_model <- summary(PC2.glm)
model_results <- rbind(model_results, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))

################################################################################
################################################################################
################################################################################
#UMAP
library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
library(umap)
library(MASS)
library(caret)

#use this to test all song data, NOT including hybrid/unsure samples:
UMAPdataforanalysis <- read.csv("/Users/leonx1/Library/CloudStorage/Box-Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

UMAPdataforanalysis[,c(4:19)] <- log(UMAPdataforanalysis[,c(4:19)]) #log transform song data

UMAPdataforanalysis[,c(4:19)] <- scale(UMAPdataforanalysis[4:19], center = TRUE, scale = TRUE)

#define random states
randomstate <- 123

#run UMAP with the specified random state
towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state = randomstate) #LDA Result: 0.8635548 ;Cohen's Kappa: 0.7128665 ;McNemar's P-Value: 0.1368383 ;Balanced Accuracy: 0.858332 Permutation P-value: 0

TowheeUMAP = towhee.umap$layout
colnames(TowheeUMAP) = c("UMAP1", "UMAP2")
ALL_Syll_Loc_Data_XCandML_UMAP = cbind(UMAPdataforanalysis, TowheeUMAP)

#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML_UMAP$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_UMAP$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML_UMAP$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML_UMAP$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML_UMAP <- ALL_Syll_Loc_Data_XCandML_UMAP %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "converted",
    "unconverted"
  ))


################################################################################
model_results_UMAP <- data.frame(
  intercept_t_value = numeric(),
  intercept_p_value = numeric(),
  Latitude_t_value = numeric(),
  Latitude_p_value = numeric(),
  Longitude_t_value = numeric(),
  Longitude_p_value = numeric(),
  Species_t_value = numeric(),
  Species_p_value = numeric(),
  Conversion_t_value = numeric(),
  Congersion__p_value = numeric(),
  stringsAsFactors = FALSE
)

#UMAP1
UMAP1.glm <- glm(UMAP1 ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_UMAP, family = gaussian(link = "identity"))
summary_model <- summary(UMAP1.glm)
model_results_UMAP <- rbind(model_results_UMAP, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))
colnames(model_results_UMAP) <- c("intercept_t_value","intercept_p_value","Latitude_t_value","Latitude_p_value","Longitude_t_value","Longitude_p_value","Species_t_value","Species_p_value","Conversion_t_value","Conversion_p_value")

#UMAP2
UMAP2.glm <- glm(UMAP2 ~ scale(Latitude) + scale(Longitude) + Species + OverallConversionStatus, data = ALL_Syll_Loc_Data_XCandML_UMAP, family = gaussian(link = "identity"))
summary_model <- summary(UMAP2.glm)
model_results_UMAP <- rbind(model_results_UMAP, c(summary_model$coefficients[1, "t value"],summary_model$coefficients[1, "Pr(>|t|)"],summary_model$coefficients[2, "t value"],summary_model$coefficients[2, "Pr(>|t|)"],summary_model$coefficients[3, "t value"],summary_model$coefficients[3, "Pr(>|t|)"],summary_model$coefficients[4, "t value"],summary_model$coefficients[4, "Pr(>|t|)"],summary_model$coefficients[5, "t value"],summary_model$coefficients[5, "Pr(>|t|)"]))


