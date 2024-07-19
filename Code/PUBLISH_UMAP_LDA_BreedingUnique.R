#UMAP + LDA breeding unique

library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
library(umap)
library(MASS)
library(caret)

#use this to test all unique breeding song data, NOT including hybrid/unsure samples:
UMAPdataforanalysis <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#use this to test all song data, INCLUDING hybrid/unsure samples:
#UMAPdataforanalysis <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv")

UMAPdataforanalysis <- UMAPdataforanalysis[,c(1,3:23)] 
UMAPdataforanalysis[,c(4:19)] <- log(UMAPdataforanalysis[,c(4:19)]) #log transform song data

#this is centered (subtract mean so that new mean becomes zero) and scaled (divide by standard dev.)
#to correct for differences in measurements so scale no longer matters:
UMAPdataforanalysis[,c(4:19)] <- scale(UMAPdataforanalysis[4:19], center = TRUE, scale = TRUE)


#load the necessary libraries
library(umap)
library(MASS)
library(caret)
library(irr)
################################################################################
############################### UMAP + LDA #####################################
#LDA is used only for song data that did NOT include hybrid/unsure samples
#skip this section if only mapping the data that includes hybrid/unsure samples

#define random states
randomstate <- 123

#run each of the following umaps separately
towhee.umap <- umap(UMAPdataforanalysis[, 4:19], random_state = randomstate) #LDA Result: 0.8530981 ;Cohen's Kappa: 0.6835441 ;McNemar's P-Value: 0.0643413 ;Balanced Accuracy: 0.8448749 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25) #test neighbors #LDA Result: 0.8682807 ;Cohen's Kappa: 0.7144665 ;McNemar's P-Value: 0.576745 ;Balanced Accuracy: 0.8582242 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50) #test neighbors #LDA Result: 0.8522774 ;Cohen's Kappa: 0.6852375 ;McNemar's P-Value: 3.131787e-05 ;Balanced Accuracy: 0.849531 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, min_dist=0.5) #test min dist #LDA Result: 0.8608945 ;Cohen's Kappa: 0.7002644 ;McNemar's P-Value: 0.06480129 ;Balanced Accuracy: 0.8532226 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, min_dist=0.9) #test min dist #LDA Result: 0.8366844 ;Cohen's Kappa: 0.6478339 ;McNemar's P-Value: 0.1202108 ;Balanced Accuracy: 0.8265309 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50, min_dist=0.9) #test both #LDA Result: 0.7858022 ;Cohen's Kappa: 0.5315512 ;McNemar's P-Value: 0.3140873 ;Balanced Accuracy: 0.764167 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50, min_dist=0.5) #test both #LDA Result: 0.8075503 ;Cohen's Kappa: 0.5836563 ;McNemar's P-Value: 0.4058817 ;Balanced Accuracy: 0.7932264 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25, min_dist=0.9) #test both #LDA Result: 0.8375051 ;Cohen's Kappa: 0.6499539 ;McNemar's P-Value: 0.07860874 ;Balanced Accuracy: 0.8279273 ;Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25, min_dist=0.5) #test both #LDA Result: 0.8551498 ;Cohen's Kappa: 0.689284 ;McNemar's P-Value: 0.005645673 ;Balanced Accuracy: 0.8492484 ;Permutation P-value: 0

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
  
#compute the confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)
  
#calculate metrics
kappa_result <- cm$overall["Kappa"]
mcnemar_pvalue <- cm$overall["McnemarPValue"]
balanced_accuracy <- cm$byClass["Balanced Accuracy"]
umap_lda_results <- sum(diag(prop.table(table(ALL_Syll_Loc_Data_XCandML_UMAP$Species, fit$class))))

############################# Permutation Test #################################
library(caret)

#calculate the original accuracy of first test set
original_accuracy <- sum(prediction_dataframe$Species == prediction_dataframe$predictions) / nrow(prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies <- replicate(n_permutations, {
  permuted_species <- sample(prediction_dataframe$Species) # Shuffle the species labels
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
cat("#LDA Result:", umap_lda_results, ";Cohen's Kappa:", kappa_result, ";McNemar's P-Value:", mcnemar_pvalue, ";Balanced Accuracy:", balanced_accuracy, ";Permutation P-value:", p_value)  

################################################################################
#for UMAP of data that INCLUDE hybrid/unsure samples, run this line instead of umaps above: 
#towhee.umap <- umap(UMAPdataforanalysis[,4:19])

################################################################################
################################################################################
################################## Plot UMAP ###################################

#create df with labels
towhee.labels <- as.factor(UMAPdataforanalysis$SpeciesOverlap)
towhee.labelsdf <- as.data.frame(towhee.labels)

#create df with UMAP projection results
layout <- towhee.umap$layout
layoutdf <- as.data.frame(layout)

#bind columns together from layout and towhee.labels
plotUMAPpoints <- cbind(towhee.labelsdf, layoutdf)

####################
UMAPplot <- ggplot(plotUMAPpoints, aes(V1, V2, col = towhee.labels, fill = towhee.labels), show.legend = TRUE) +
  geom_point(shape = 21, col = "black", size = 3.0, alpha = 0.8) +
  scale_fill_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC", "Hybrid/Unsure" = "black")) +
  labs(title = "UMAP visualization of the towhee song dataset") + 
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10), legend.position = "top") +
  theme_classic()
UMAPplot

#save PNG 950 x 550 #save PDF 9.5 x 5.5 
