library(tidyverse)
library(ggfortify)
library(plotly)
library(ggplot2)
library(readr)
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
ALL_Syll_Loc_Data_XCandML[,c(4:19)] <- scale(ALL_Syll_Loc_Data_XCandML[4:19], center = TRUE, scale = TRUE)

################################################################################
################################################################################
####################### Principal Component Analysis ###########################


### this is centered (subtract mean so that new mean becomes zero) and scaled (divide by standard dev.)
### to correct for differences in measurements so scale no longer matters 
pca_resid <- prcomp(ALL_Syll_Loc_Data_XCandML[,4:20], scale = FALSE)

pca_resid

### standard deviation (measure of variability across that PC; eigenvalues)
### proportion of variance (proportion of all variability in original data explained by that PC)
summary(pca_resid)

### scre plot (shows variances (square of stdev) across each PC)

PCA_variance = pca_resid$sdev^2 / sum(pca_resid$sdev^2)

# Scree line plot
qplot(c(1:17), PCA_variance) +
  geom_line() +
  geom_point(size=4)+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ylim(0, 0.4) +
  scale_x_continuous(n.breaks = 17)

# scree bar graph
qplot(c(1:17), PCA_variance) +
  geom_col()+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.4) +
  scale_x_continuous(n.breaks = 17)

### content of pca_res
str(pca_resid)
### pca_res$x has PCA scores for each individual observation
### gives coordinates of individual point on PC scales
pca_resid$x


### give PC1 and PC2 values for each sample in data and plot
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- cbind(ALL_Syll_Loc_Data_XCandML, pca_resid$x[, 1:2])
head(ALL_Syll_Loc_Data_XCandML_PC1_PC2)

#randomize order of rows for plotting
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- ALL_Syll_Loc_Data_XCandML_PC1_PC2[sample(nrow(ALL_Syll_Loc_Data_XCandML_PC1_PC2)),]

### plot with 95% confidence ellipse
### this is a linear combination of those variables 
PCA_songdata <- ggplot(ALL_Syll_Loc_Data_XCandML_PC1_PC2, aes(PC1, PC2, col = as.factor(OverallConversionStatus), fill = as.factor(OverallConversionStatus))) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.1) +
  geom_point(shape = 21, col = "black", size = 2.0) +
  scale_fill_manual(values = c("0" = "#494848", "1" = "#B4B4B4")) +
  xlab("PC1") +
  ylab("PC2") +
  theme (legend.position = "top")
PCA_songdata

### correlations b/w variables and PCs
cor(ALL_Syll_Loc_Data_XCandML[4:20], ALL_Syll_Loc_Data_XCandML_PC1_PC2[,c("PC1","PC2")])


################################################################################
################################################################################
#################### Linear Discriminant Analysis ##############################
library(MASS)
#LDA
fitPCA <- lda(Species ~ PC1 + PC2, data=ALL_Syll_Loc_Data_XCandML_PC1_PC2,
              na.action="na.omit", CV=TRUE)
fitPCA # show results

#####
library(irr)
prediction_dataframe <- data.frame()
predictions <- as.vector(fitPCA$class)
prediction_dataframe <- as.data.frame(predictions)

#add actual species 
prediction_dataframe$Species <- ALL_Syll_Loc_Data_XCandML_PC1_PC2$Species

#convert to factor with two levels
prediction_dataframe$Species <- as.factor(prediction_dataframe$Species) 
prediction_dataframe$predictions <- as.factor(prediction_dataframe$predictions)

library(caret)
# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)
# Calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result 

Mcnemar_pvalue <- cm$overall["McnemarPValue"] 
Mcnemar_pvalue 

#accuracy
accuracy <-cm$overall["Accuracy"]
accuracy 

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy 

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
  permuted_species <- sample(prediction_dataframe$Species) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == prediction_dataframe$predictions) / nrow(prediction_dataframe)
  return(permuted_accuracy)
})

#calculate the p-value
p_value <- sum(permuted_accuracies >= original_accuracy) / n_permutations


cat("Original Accuracy:", original_accuracy, "\n")
cat("P-value:", p_value, "\n")

#determine if the classifier is better than chance
if (p_value < 0.05) {
  cat("The classifier is performing better than chance at the 0.05 significance level.\n")
} else {
  cat("The classifier is not performing better than chance at the 0.05 significance level.\n")
}

