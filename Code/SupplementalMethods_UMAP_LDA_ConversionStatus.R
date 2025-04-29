library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
library(umap)
library(MASS)
library(caret)
################################################################################
ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")

#log transform song data:
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML[,c(4:19)]) #log transform song data


#make another column that records "converted" for OriginalFileType != "WAV" and "unconverted" for OriginalFileType = "WAV"
ALL_Syll_Loc_Data_XCandML$FileTypeConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML$OriginalFileType == "WAV", "unconverted", "converted")

#make another column that records "converted" for SamplingRate != "41000" and "unconverted" for SamplingRate == "44100"
ALL_Syll_Loc_Data_XCandML$SamplingRateConversionStatus <- ifelse(ALL_Syll_Loc_Data_XCandML$SamplingRate == 44100, "unconverted", "converted")

#make another column that records "converted" for SamplingRateConversionStatus == "converted" or FileTypeConversionStatus == "converted"
ALL_Syll_Loc_Data_XCandML <- ALL_Syll_Loc_Data_XCandML %>%
  mutate(OverallConversionStatus = ifelse(
    SamplingRateConversionStatus == "converted" | FileTypeConversionStatus == "converted",
    "1",
    "0"
  ))

ALL_Syll_Loc_Data_XCandML <- subset(ALL_Syll_Loc_Data_XCandML, select = c(1:19, 27, 20)) #ALL: 2785 #Breeding: 2436
ALL_Syll_Loc_Data_XCandML$OverallConversionStatus <- as.numeric(ALL_Syll_Loc_Data_XCandML$OverallConversionStatus) #ALL: 2785 #Breeding: 2436
UMAPdataforanalysis <- ALL_Syll_Loc_Data_XCandML

#this is centered (subtract mean so that new mean becomes zero) and scaled (divide by standard dev.)
#to correct for differences in measurements so scale no longer matters:
UMAPdataforanalysis[,c(4:19)] <- scale(UMAPdataforanalysis[4:19], center = TRUE, scale = TRUE)

################################################################################
############################### UMAP + LDA #####################################
#LDA is used only for song data that did NOT include hybrid/unsure samples
#skip this section if only mapping the data that includes hybrid/unsure samples

#define random states
randomstate <- 123

#run UMAP with the specified random state
towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state = randomstate) 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=25) #test neighbors 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=50) #test neighbors 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, min_dist=0.5) #test min dist 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, min_dist=0.9) #test min dist 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=50, min_dist=0.9) #test both 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=50, min_dist=0.5) #test both 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=25, min_dist=0.9) #test both 
#towhee.umap <- umap(UMAPdataforanalysis[,4:20], random_state=randomstate, n_neighbors=25, min_dist=0.5) #test both 

TowheeUMAP = towhee.umap$layout
colnames(TowheeUMAP) = c("UMAP1", "UMAP2")
ALL_Syll_Loc_Data_XCandML_UMAP = cbind(UMAPdataforanalysis, TowheeUMAP)

#fit the LDA model
fit <- lda(Species ~ UMAP1 + UMAP2, data = ALL_Syll_Loc_Data_XCandML_UMAP, na.action = "na.omit", CV = TRUE)

#create a data frame for predictions
prediction_dataframe <- data.frame(predictions = as.vector(fit$class))
prediction_dataframe$Species <- ALL_Syll_Loc_Data_XCandML_UMAP$Species
prediction_dataframe$Species <- as.factor(prediction_dataframe$Species)
prediction_dataframe$predictions <- as.factor(prediction_dataframe$predictions)

#compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)

#calculate metrics
kappa_result <- cm$overall["Kappa"]
mcnemar_pvalue <- cm$overall["McnemarPValue"]
balanced_accuracy <- cm$byClass["Balanced Accuracy"]
umap_lda_results <- sum(diag(prop.table(table(ALL_Syll_Loc_Data_XCandML_UMAP$Species, fit$class))))

################################################################################
############################# Permutation Test #################################
library(caret)

####test set non-overlap
#calculate the original accuracy of first test set
original_accuracy <- sum(prediction_dataframe$Species == prediction_dataframe$predictions) / nrow(prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies <- replicate(n_permutations, {
  permuted_species <- sample(prediction_dataframe$Species) #shuffle the species labels
  permuted_accuracy <- sum(permuted_species == prediction_dataframe$predictions) / nrow(prediction_dataframe)
  return(permuted_accuracy)
})

#calculate the p-value
p_value <- sum(permuted_accuracies >= original_accuracy) / n_permutations

#determine if the classifier is better than chance
if (p_value < 0.05) {
  cat("The classifier is performing better than chance at the 0.05 significance level.\n")
} else {
  cat("The classifier is not performing better than chance at the 0.05 significance level.\n")
}

#print results for each metric
cat("#LDA Result:", umap_lda_results, ";Cohen's Kappa:", kappa_result, ";McNemar's P-Value:", mcnemar_pvalue, ";Balanced Accuracy:", balanced_accuracy, "Permutation P-value:", p_value)  

################################################################################
################################################################################
################################## Plot UMAP ###################################

#create df with labels
conversion.labels <- as.factor(UMAPdataforanalysis$OverallConversionStatus)
conversion.labelsdf <- as.data.frame(conversion.labels)

#create df with UMAP projection results
layout <- towhee.umap$layout
layoutdf <- as.data.frame(layout)

#bind columns together from layout and towhee.labels
plotUMAPpoints <- cbind(conversion.labelsdf, layoutdf)

####################
UMAPplot <- ggplot(plotUMAPpoints, aes(V1, V2, col = conversion.labels, fill = conversion.labels), show.legend = TRUE) +
  geom_point(shape = 21, col = "black", size = 3.0, alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#494848", "1" = "#B4B4B4")) +
  labs(title = "UMAP visualization of the towhee song dataset") + 
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10), legend.position = "top") +
  theme_classic()
UMAPplot
