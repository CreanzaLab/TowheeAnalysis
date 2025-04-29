library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
library(umap)
library(MASS)
library(caret)

#use this to test all song data, NOT including hybrid/unsure samples:
UMAPdataforanalysis <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#use this to test all song data, INCLUDING hybrid/unsure samples:
#UMAPdataforanalysis <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv")


UMAPdataforanalysis[,c(4:19)] <- log(UMAPdataforanalysis[,c(4:19)]) #log transform song data

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
towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state = randomstate) #LDA Result: 0.8635548 ;Cohen's Kappa: 0.7128665 ;McNemar's P-Value: 0.1368383 ;Balanced Accuracy: 0.858332 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25) #test neighbors #LDA Result: 0.8520646 ;Cohen's Kappa: 0.6917359 ;McNemar's P-Value: 2.818584e-05 ;Balanced Accuracy: 0.8511496 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50) #test neighbors #LDA Result: 0.8621185 ;Cohen's Kappa: 0.7114748 ;McNemar's P-Value: 0.00185263 ;Balanced Accuracy: 0.8596538 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, min_dist=0.5) #test min dist #LDA Result: 0.8481149 ;Cohen's Kappa: 0.6825617 ;McNemar's P-Value: 0.0009454588 ;Balanced Accuracy: 0.8454623 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, min_dist=0.9) #test min dist #LDA Result: 0.8262118 ;Cohen's Kappa: 0.6351847 ;McNemar's P-Value: 0.05063674 ;Balanced Accuracy: 0.8200737 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50, min_dist=0.9) #test both #LDA Result: 0.7533214 ;Cohen's Kappa: 0.4754263 ;McNemar's P-Value: 0.2854003 ;Balanced Accuracy: 0.7364891 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=50, min_dist=0.5) #test both #LDA Result: 0.8290844 ;Cohen's Kappa: 0.6455858 ;McNemar's P-Value: 2.226645e-07 ;Balanced Accuracy: 0.8293271 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25, min_dist=0.9) #test both #LDA Result: 0.8247756 ;Cohen's Kappa: 0.6329458 ;McNemar's P-Value: 0.01278391 ;Balanced Accuracy: 0.8196198 Permutation P-value: 0
#towhee.umap <- umap(UMAPdataforanalysis[,4:19], random_state=randomstate, n_neighbors=25, min_dist=0.5) #test both #LDA Result: 0.8495512 ;Cohen's Kappa: 0.6864436 ;McNemar's P-Value: 4.0665e-05 ;Balanced Accuracy: 0.8484021 Permutation P-value: 0

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
#for UMAP of data that INCLUDE hybrid/unsure samples, run this line instead of umap above: 
#towhee.umap <- umap(UMAPdataforanalysis[,4:19])

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