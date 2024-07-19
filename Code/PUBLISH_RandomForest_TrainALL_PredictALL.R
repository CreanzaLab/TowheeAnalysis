#Random Forest Train on subset of all sample--Test on Subset of all subset 
#ALL and Breeding
library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)

# Read in the dataset
ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1:22)]

#to repeat analysis on only breeding season, run the following:
#ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1,3:23)]

#continue here
ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data
ALL_Syll_Loc_Data_XCandML_data$Species <- make.names(ALL_Syll_Loc_Data_XCandML_data$Species) #ALL: 2788 #Breeding: 2437
ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- make.names(ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap)

#################################################################################

ALL_Syll_Loc_Data_XCandML <- subset(ALL_Syll_Loc_Data_XCandML_data, select = c(1:20,22)) #ALL: 2788 #Breeding: 2437

################################################################################
###all zone of overlap data #will use this for plotting later
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_OverlapONLY, ] #ALL: 217 #Breeding: 167
ALL_Syll_Loc_Data_OverlapONLY_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))

################################################################################

data <- ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.maculatus" | ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.erythrophthalmus" 
data <- ALL_Syll_Loc_Data_XCandML_data[data, ] 
data <- subset(data, select = c(1, 4:20))

# Randomly split the data into training and test sets
set.seed(123)
train_index <- sample(1:nrow(data), 0.75*nrow(data)) #ALL: 2091 #Breeding: 1827 
train_data <- data[train_index, ] #ALL: 2091 total training; 1301 eastern; 790 spotted #Breeding: 1827 total training; 1169 eastern; 658 spotted
test_data <- data[-train_index, ] #ALL: 697 total testing #Breeding: 610 total testing


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #ALL: 790 #Breeding: 658
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #ALL: 1301 #Breeding: 1169

set.seed(123)
train_data_spotted <- n_spotted[sample(nrow(n_spotted), min(nrow(n_spotted), nrow(n_eastern))), ] #ALL: 790 #Breeding: 658
set.seed(123)
train_data_eastern <- n_eastern[sample(nrow(n_eastern), min(nrow(n_spotted), nrow(n_eastern))), ] #ALL: 790 #Breeding: 658

train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #ALL: 1580 total; 790 eastern; 790 spotted #Breeding: 1316 total; 658 eastern; 658 spotted
train_data_downsampled <- subset(train_data_downsampled, select = -c(1))

# If it's not already a factor, convert it to a factor
class(train_data_downsampled$Species) #check class
train_data_downsampled$Species <- as.factor(train_data_downsampled$Species) #convert to factor


################################################################################
# Create a trainControl object for 10-fold cross-validation
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE)

# Define the parameter grid to search over
grid <- expand.grid(mtry = 1:16, min.node.size = c(1:10), splitrule = c("gini"))

# Train the model using the downsampled training set
set.seed(123)
bestTunemodel <- train(Species ~ ., data = train_data_downsampled, method = "ranger", num.trees = 500, tuneGrid = grid, trControl = ctrl)

################################################################################
# Fit a random forest model using the downsampled training set
set.seed(123)
model_ALL <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)


# Make predictions on the test set
test_data$Species <- make.names(test_data$Species)
set.seed(123)
predictions <- predict(model_ALL, data = test_data)$predictions


# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species) #ALL: accuracy = 0.8852224 #Breeding: accuracy = 0.9196721
accuracy
################################################################################
#bind original with predictions
plot_data_pred <- cbind(test_data, predictions)

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################
#####Cohen's Kappa

library(irr)
prediction_dataframe <- plot_data_pred
prediction_dataframe$Species <- as.factor(prediction_dataframe$Species) #convert to factor
prediction_dataframe$predictions <- as.factor(prediction_dataframe$predictions) #convert to factor

# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)
# Calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result #ALL: 0.7648607 #Breeding: 0.8278127

Mcnemar_pvalue <- cm$overall["McnemarPValue"] #Breeding: 0.01012799
Mcnemar_pvalue #ALL: 0.002538687 

accuracy <-cm$overall["Accuracy"]
accuracy

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy #ALL: 0.8888117 #Breeding: 0.9221306

################################################################################
############################# Permutation Test #################################
library(caret)

#calculate the original accuracy
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
Predictions_With_Coords <- merge(plot_data_pred, subset(ALL_Syll_Loc_Data_XCandML, select = c(1:19))) 
Predictions_With_Coords$predictions <- as.character(Predictions_With_Coords$predictions)


Predictions_With_Coords$Prediction_w_Overlap = Predictions_With_Coords$predictions

Predictions_With_Coords$Prediction_w_Overlap[which(Predictions_With_Coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords$predictions == "Pipilo.erythrophthalmus")]= "Pipilo.erythrophthalmus.Overlap"
Predictions_With_Coords$Prediction_w_Overlap[which(Predictions_With_Coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords$predictions == "Pipilo.maculatus")]= "Pipilo.maculatus.Overlap"

################################################################################
#order dataframe to have incorrect predictions on top

Predictions_With_Coords_incorrect <- Predictions_With_Coords[Predictions_With_Coords$correct == 'Incorrect',]

Predictions_With_Coords_correct <- Predictions_With_Coords[Predictions_With_Coords$correct == 'Correct',]

Predictions_With_Coords_ordered <- rbind(Predictions_With_Coords_correct, Predictions_With_Coords_incorrect)

################################################################################
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") #obtaining map data for plotting

RFM_map_ALL_PredictALL <- ggplot(Predictions_With_Coords_ordered, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = correct, colour = Prediction_w_Overlap), size = 3.0, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Prediction", colour="Prediction Class") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.maculatus.Overlap" = "#a6cee3", "Pipilo.erythrophthalmus" = "#960019", "Pipilo.erythrophthalmus.Overlap" = "#FFCCCC")) +
  theme (legend.position = "right") +
  theme_classic()
RFM_map_ALL_PredictALL

##########
#check which IDs had incorrect predictions
RFM_prediction_dataframe <- Predictions_With_Coords
