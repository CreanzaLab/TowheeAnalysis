#Random Forest Train on Edges--Test on Overlap
library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(stringr)


# Read in the dataset
ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1:22)]

#to repeat analysis on only breeding season, run the following:
#ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1,3:23)]

#continue here
ALL_Syll_Loc_Data_XCandML_data$Species <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$Species, " ", ".") 
ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap, " ", ".")

ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data #ALL: 2785 #Breeding: 2436

###all data except for zone of overlap
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus" 
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_NoOverlap, ] #ALL: 2569 #Breeding: 2269
ALL_Syll_Loc_Data_NoOverlap <- subset(ALL_Syll_Loc_Data_NoOverlap, select = c(1, 4:20))


###all zone of overlap data #will use this for new predictions
# Read in the new test dataset
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_OverlapONLY, ] #ALL: 216 #Breeding: 167
ALL_Syll_Loc_Data_OverlapONLY_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))
new_test_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1,4:19))

################################################################################

# Randomly split the data into training and test sets

AllResults=data.frame(testrun=1:1000,accuracy_outerrange=1:1000,accuracy_overlapzone=1:1000)
for(seedval in 1:1000){
  
  print(seedval)
 
  seed1=seedval*42
  set.seed(seed1)

train_index <- sample(1:nrow(ALL_Syll_Loc_Data_NoOverlap), nrow(ALL_Syll_Loc_Data_NoOverlap) - nrow(new_test_data)) #ALL: 2353 #Breeding: 2102 #reserving same size subsample as the number that will be tested in order to compare predictions
train_data <- ALL_Syll_Loc_Data_NoOverlap[train_index, ] #ALL: 2353 total training; 1470 eastern; 883 spotted #Breeding: 2102 total training; 1358 eastern; 744 spotted
test_data <- ALL_Syll_Loc_Data_NoOverlap[-train_index, ] #ALL: 216 total testing #Breeding: 167 total testing


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #ALL: 883 #Breeding: 744
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #ALL: 1470 #Breeding: 1358

#for all do this:
set.seed(123)
train_data_spotted <- n_spotted[sample(nrow(n_spotted), 796), ] 
set.seed(123)
train_data_eastern <- n_eastern[sample(nrow(n_eastern), 796), ] 

#for breeding do this:
#set.seed(123)
#train_data_spotted <- n_spotted[sample(nrow(n_spotted), 652), ] 
#set.seed(123)
#train_data_eastern <- n_eastern[sample(nrow(n_eastern), 652), ] 


train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #ALL: 1592 #Breeding: 1304
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
model_ALLedges <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)


# Make predictions on the test set
set.seed(123)
predictions <- predict(model_ALLedges, data = test_data)$predictions

# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species)  #ALL: 0.9354839 #Breeding: 0.9281437
accuracy

plot_data_pred <- cbind(test_data, predictions) 

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################

# Make predictions on the new test set
set.seed(123)
new_predictions <- predict(model_ALLedges, data = new_test_data)$predictions

# Calculate the accuracy of the model on the new test set
new_accuracy <- mean(new_predictions == ALL_Syll_Loc_Data_OverlapONLY_data$Species) #ALL: 0.8425926 #Breeding: 0.8742515
new_accuracy

#bind original with predictions
plot_data_pred_new <- cbind(ALL_Syll_Loc_Data_OverlapONLY_data, new_predictions) 

plot_data_pred_new <- plot_data_pred_new %>% mutate(correct = case_when(
  Species == new_predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 


AllResults$testrun[seedval] <- seedval
AllResults$accuracy_outerrange[seedval] <- accuracy
AllResults$accuracy_overlapzone[seedval] <- new_accuracy

}

################################################################################
############################## Cohen's Kappa ###################################

library(irr)
prediction_dataframe <- plot_data_pred
prediction_dataframe$Species <- as.factor(prediction_dataframe$Species) #convert to factor
prediction_dataframe$predictions <- as.factor(prediction_dataframe$predictions) #convert to factor

# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predictions, prediction_dataframe$Species)
# Calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result 

Mcnemar_pvalue <- cm$overall["McnemarPValue"]  
Mcnemar_pvalue

accuracy <-cm$overall["Accuracy"]  
accuracy

balanced_accuracy <- cm$byClass["Balanced Accuracy"] 
balanced_accuracy

#
library(irr)
prediction_dataframe2 <- plot_data_pred_new
prediction_dataframe2$Species <- as.factor(prediction_dataframe2$Species) #convert to factor
prediction_dataframe2$new_predictions <- as.factor(prediction_dataframe2$new_predictions) #convert to factor

# Compute confusion matrix
cm2 <- confusionMatrix(prediction_dataframe2$new_predictions, prediction_dataframe2$Species)

# Calculate Cohen's kappa
kappa_result2 <- cm2$overall["Kappa"]
kappa_result2 

Mcnemar_pvalue2 <- cm2$overall["McnemarPValue"]
Mcnemar_pvalue2

accuracy2 <-cm2$overall["Accuracy"] 
accuracy2

balanced_accuracy2 <- cm2$byClass["Balanced Accuracy"] 
balanced_accuracy2

################################################################################
############################# Permutation Test #################################
library(caret)

####test set non-overlap
#calculate the original accuracy of first test set
original_accuracy1 <- sum(prediction_dataframe$Species == prediction_dataframe$predictions) / nrow(prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies1 <- replicate(n_permutations, {
  permuted_species <- sample(prediction_dataframe$Species) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == prediction_dataframe$predictions) / nrow(prediction_dataframe)
  return(permuted_accuracy)
})

#calculate the p-value
p_value1 <- sum(permuted_accuracies1 >= original_accuracy1) / n_permutations


cat("Original Accuracy1:", original_accuracy1, "\n")
cat("P-value1:", p_value1, "\n")

#determine if the classifier is better than chance
if (p_value1 < 0.05) {
  cat("The classifier is performing better than chance at the 0.05 significance level.\n")
} else {
  cat("The classifier is not performing better than chance at the 0.05 significance level.\n")
}

####test set overlap
#calculate the original accuracy of 2nd test set predictions
original_accuracy2 <- sum(prediction_dataframe2$Species == prediction_dataframe2$new_predictions) / nrow(prediction_dataframe2)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies2 <- replicate(n_permutations, {
  permuted_species <- sample(prediction_dataframe2$Species) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == prediction_dataframe2$new_predictions) / nrow(prediction_dataframe2)
  return(permuted_accuracy)
})

#calculate the p-value
p_value2 <- sum(permuted_accuracies2 >= original_accuracy2) / n_permutations


cat("Original Accuracy2:", original_accuracy2, "\n")
cat("P-value2:", p_value2, "\n")

#determine if the classifier is better than chance
if (p_value2 < 0.05) {
  cat("The classifier is performing better than chance at the 0.05 significance level.\n")
} else {
  cat("The classifier is not performing better than chance at the 0.05 significance level.\n")
}
################################################################################
################################################################################
################################################################################
#bind data together for plotting
#rbind 
colnames(plot_data_pred_new)[19] <- "predictions" 
ALL_predictions_ALL <- rbind(plot_data_pred_new, plot_data_pred) 
ALL_predictions_ALL_coords <- merge(ALL_predictions_ALL, subset(ALL_Syll_Loc_Data_XCandML_data, select = c(1:19,22))) 


################################################################################

ALL_predictions_ALL_coords$predictions <- as.character(ALL_predictions_ALL_coords$predictions)

ALL_predictions_ALL_coords$Prediction_w_Overlap = ALL_predictions_ALL_coords$predictions

ALL_predictions_ALL_coords$Prediction_w_Overlap[which(ALL_predictions_ALL_coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & ALL_predictions_ALL_coords$predictions == "Pipilo.erythrophthalmus")]= "Pipilo.erythrophthalmus.Overlap"
ALL_predictions_ALL_coords$Prediction_w_Overlap[which(ALL_predictions_ALL_coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & ALL_predictions_ALL_coords$predictions == "Pipilo.maculatus")]= "Pipilo.maculatus.Overlap"

################################################################################
#order dataframe to have incorrect predictions on top

ALL_predictions_ALL_coords_incorrect <- ALL_predictions_ALL_coords[ALL_predictions_ALL_coords$correct == 'Incorrect',]

ALL_predictions_ALL_coords_correct <- ALL_predictions_ALL_coords[ALL_predictions_ALL_coords$correct == 'Correct',]

ALL_predictions_ALL_coords_ordered <- rbind(ALL_predictions_ALL_coords_correct, ALL_predictions_ALL_coords_incorrect)

################################################################################
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") #obtaining map data for plotting

RFM_map_ALL_PredictOverlap <- ggplot(ALL_predictions_ALL_coords_ordered, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = correct, colour = Prediction_w_Overlap), size = 3, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Prediction", colour="Prediction Class") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.maculatus.Overlap" = "#a6cee3", "Pipilo.erythrophthalmus" = "#960019", "Pipilo.erythrophthalmus.Overlap" = "#FFCCCC")) +
  theme (legend.position = "right") +
  theme_classic()
RFM_map_ALL_PredictOverlap


################################################################################
##################### Predictions for Hybrid/Unsure Samples ####################
ALL_Syll_Loc_Data_XCandML_withhybridunsure <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv")
hybrid_unsure_only <- ALL_Syll_Loc_Data_XCandML_withhybridunsure[apply(ALL_Syll_Loc_Data_XCandML_withhybridunsure, 1, function(x) any(grepl("Hybrid/Unsure", x))), ]
#to repeat analysis on only breeding season, run the following:
#ALL_Syll_Loc_Data_XCandML_withhybridunsure <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_unique_withhybridunsure.csv")
#hybrid_unsure_only <- ALL_Syll_Loc_Data_XCandML_withhybridunsure[apply(ALL_Syll_Loc_Data_XCandML_withhybridunsure, 1, function(x) any(grepl("Hybrid/Unsure", x))), ]
#hybrid_unsure_only <- hybrid_unsure_only[,c(1,3:23)]


hybrid_unsure_only[,c(4:19)] <- log(hybrid_unsure_only[,c(4:19)])

hybrid_unsure_test <- hybrid_unsure_only[,c(1,4:19)]


set.seed(123)
hybrid_unsure_predictions <- predict(model_ALLedges, data = hybrid_unsure_test)$predictions

#bind original with predictions
plot_data_pred_hybridunsure <- cbind(hybrid_unsure_only, hybrid_unsure_predictions) 


################################################################################
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") #obtaining map data for plotting

RFM_map_hybridunsure_PredictOverlap <- ggplot(plot_data_pred_hybridunsure, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, colour = hybrid_unsure_predictions), size = 3, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Prediction", colour="Prediction Class") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.erythrophthalmus" = "#960019")) +
  theme (legend.position = "right") +
  theme_classic() +
  geom_vline(xintercept = -102) +
  geom_vline(xintercept = -91)
RFM_map_hybridunsure_PredictOverlap
