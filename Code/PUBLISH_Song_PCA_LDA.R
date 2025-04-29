################################################################################

ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML <- ALL_Syll_Loc_Data_XCandML[,c(1:22)]

#to repeat analysis on only breeding season, run the following:
#ALL_Syll_Loc_Data_XCandML <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#ALL_Syll_Loc_Data_XCandML <- ALL_Syll_Loc_Data_XCandML[,c(1,3:23)]

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

### scre plot (shows variances (square of stdev) across each PC)

PCA_variance = pca_resid$sdev^2 / sum(pca_resid$sdev^2)

# Scree line plot
qplot(c(1:16), PCA_variance) +
  geom_line() +
  geom_point(size=4)+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ylim(0, 0.4) +
  scale_x_continuous(n.breaks = 16)

# scree bar graph
qplot(c(1:16), PCA_variance) +
  geom_col()+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.4) +
  scale_x_continuous(n.breaks = 16)

### content of pca_res
str(pca_resid)
### pca_res$x has PCA scores for each individual observation
### gives coordinates of individual point on PC scales
pca_resid$x


### give PC1 and PC2 values for each sample in data and plot
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- cbind(ALL_Syll_Loc_Data_XCandML, pca_resid$x[, 1:5])
head(ALL_Syll_Loc_Data_XCandML_PC1_PC2)

#randomize order of rows for plotting
ALL_Syll_Loc_Data_XCandML_PC1_PC2 <- ALL_Syll_Loc_Data_XCandML_PC1_PC2[sample(nrow(ALL_Syll_Loc_Data_XCandML_PC1_PC2)),]

### plot with 95% confidence ellipse
### this is a linear combination of those variables 
PCA_songdata <- ggplot(ALL_Syll_Loc_Data_XCandML_PC1_PC2, aes(PC1, PC2, col = SpeciesOverlap, fill = SpeciesOverlap)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.1) +
  geom_point(shape = 21, col = "black", size = 2.0) +
  scale_fill_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) +
  xlab("PC1") +
  ylab("PC2") +
  theme (legend.position = "top")
PCA_songdata

### correlations b/w variables and PCs
cor(ALL_Syll_Loc_Data_XCandML[4:19], ALL_Syll_Loc_Data_XCandML_PC1_PC2[,c("PC1","PC2")])

### plot PC1 on map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

PC1_ggplot <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = ALL_Syll_Loc_Data_XCandML_PC1_PC2, aes(x = Longitude, y = Latitude , shape = SpeciesOverlap, colour = PC1), size = 3.5, show.legend = TRUE) +
  coord_sf(xlim = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_color_viridis_c() +
  labs(colour="PC1") +
  theme (legend.position = "bottom")
#PC1_ggplot

### plot PC2 on map
PC2_ggplot <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = ALL_Syll_Loc_Data_XCandML_PC1_PC2, aes(x = Longitude, y = Latitude, shape = SpeciesOverlap, colour = PC2), size = 3.5, show.legend = TRUE) +
  coord_sf(xlim = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_color_viridis_c() +
  labs(colour="PC2") +
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10)) +
  theme (legend.position = "bottom")
#PC2_ggplot



################################################################################
################################################################################
#################### Linear Discriminant Analysis ##############################

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

# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)
# Calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result #ALL: 0.4879022 #Breeding: 0.4664987

Mcnemar_pvalue <- cm$overall["McnemarPValue"] 
Mcnemar_pvalue #ALL: 0.0003529364 #Breeding: 1.346614e-05 

#accuracy
accuracy <-cm$overall["Accuracy"]
accuracy #ALL: 0.761939 #Breeding: 0.7610837 

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy #ALL: 0.7399226 #Breeding: 0.7270193

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

################################################################################
################################################################################
########################## Procrustes Projection ###############################

#this shows the new projection on the map
library(vegan)
library(MCMCpack)
library(maps)

Towhee_PCA_results <- as.data.frame(pca_resid$x)
Towhee_PCA_results_PC1andPC2 <- subset(Towhee_PCA_results, select = c(PC1, PC2))

PCA_towhee_data <- subset(ALL_Syll_Loc_Data_XCandML, select = c(Latitude, Longitude, Species, SpeciesOverlap))


song_coord_matrix <- as.matrix(cbind(PCA_towhee_data$Longitude,PCA_towhee_data$Latitude))
PCA_matrix <- as.matrix(cbind(Towhee_PCA_results_PC1andPC2$PC1,Towhee_PCA_results_PC1andPC2$PC2))

pro_song <- procrustes(PCA_matrix,song_coord_matrix,translation=TRUE,dilation=TRUE)

protest(song_coord_matrix, PCA_matrix)


pro_song <- pro_song$X.new 
pro_song <- data.frame(pro_song)
procrustesfinal <- cbind(pro_song, PCA_towhee_data$SpeciesOverlap)
colnames(procrustesfinal) <- c('Longitude', 'Latitude', 'SpeciesOverlap')

procrustesfinal_overlap <- procrustesfinal[sample(nrow(procrustesfinal)),]


procrustes_songdata <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = procrustesfinal_overlap, aes(x = Longitude, y = Latitude, colour = SpeciesOverlap), size = 2.0, alpha = 0.8) +
  coord_sf(xlim = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  theme(plot.title = element_text(size=15), legend.title = element_text (size=10)) +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C", "Pipilo maculatus Overlap" = "#a6cee3", "Pipilo erythrophthalmus" = "#960019", "Pipilo erythrophthalmus Overlap" = "#FFCCCC")) #colors: #5C6B9C #960019
procrustes_songdata


