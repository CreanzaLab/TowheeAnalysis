library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)

# Read in the dataset
breeding_Syll_Loc_Data_XCandML_data <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_4.1.23.csv")

breeding_Syll_Loc_Data_XCandML_data$Species <- str_replace_all(breeding_Syll_Loc_Data_XCandML_data$Species, " ", ".") 
breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- str_replace_all(breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap, " ", ".")

breeding_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(breeding_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data #3037

###all data except for zone of overlap
breeding_Syll_Loc_Data_NoOverlap <- breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus" | breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus" 
breeding_Syll_Loc_Data_NoOverlap <- breeding_Syll_Loc_Data_XCandML_data[breeding_Syll_Loc_Data_NoOverlap, ] #2826
breeding_Syll_Loc_Data_NoOverlap <- subset(breeding_Syll_Loc_Data_NoOverlap, select = c(1, 4:20))


###all zone of overlap data #will use this for new predictions
# Read in the new test dataset
breeding_Syll_Loc_Data_OverlapONLY <- breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
breeding_Syll_Loc_Data_OverlapONLY <- breeding_Syll_Loc_Data_XCandML_data[breeding_Syll_Loc_Data_OverlapONLY, ] #211
breeding_Syll_Loc_Data_OverlapONLY_data <- subset(breeding_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))
new_test_data <- subset(breeding_Syll_Loc_Data_OverlapONLY, select = c(1,4:19))

################################################################################

# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(breeding_Syll_Loc_Data_NoOverlap), nrow(breeding_Syll_Loc_Data_NoOverlap) - nrow(new_test_data)) #2615 #reserving same size subsample as the number that will be tested in order to compare predictions
train_data <- breeding_Syll_Loc_Data_NoOverlap[train_index, ] #2615 total training #1630 eastern #985 spotted
test_data <- breeding_Syll_Loc_Data_NoOverlap[-train_index, ] #211 total testing 


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #985
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #1630

train_data_spotted <- n_spotted[sample(nrow(n_spotted), 853), ] #853
train_data_eastern <- n_eastern[sample(nrow(n_eastern), 853), ] #853


train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #1706 
train_data_downsampled <- subset(train_data_downsampled, select = -c(1))


# If it's not already a factor, convert it to a factor
class(train_data_downsampled$Species) #check class
train_data_downsampled$Species <- as.factor(train_data_downsampled$Species) #convert to factor


################################################################################
# Create a trainControl object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE)

# Define the parameter grid to search over
grid <- expand.grid(mtry = 1:16, min.node.size = c(1:10), splitrule = c("gini")) #change node 10,100,1000


# Train the model using the downsampled training set
bestTunemodel <- train(Species ~ ., data = train_data_downsampled, method = "ranger", num.trees = 500, tuneGrid = grid, trControl = ctrl)

################################################################################

# Fit a random forest model using the downsampled training set
model_breeding_edges <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)


# Make predictions on the test set
predictions <- predict(model_breeding_edges, data = test_data)$predictions

# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species) 

plot_data_pred <- cbind(test_data, predictions) #211

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################

# Make predictions on the new test set
new_predictions <- predict(model_breeding_edges, data = new_test_data)$predictions

# Calculate the accuracy of the model on the new test set
new_accuracy <- mean(new_predictions == breeding_Syll_Loc_Data_OverlapONLY_data$Species) 

#bind original with predictions
plot_data_pred_new <- cbind(breeding_Syll_Loc_Data_OverlapONLY_data, new_predictions) #211

plot_data_pred_new <- plot_data_pred_new %>% mutate(correct = case_when(
  Species == new_predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################

#rbind 
colnames(plot_data_pred_new)[19] <- "predictions" 
ALL_predictions_breeding <- rbind(plot_data_pred_new, plot_data_pred) #422
ALL_predictions_breeding_coords <- merge(ALL_predictions_breeding, subset(breeding_Syll_Loc_Data_XCandML_data, select = c(1:19,23))) #422


################################################################################

ALL_predictions_breeding_coords$predictions <- as.character(ALL_predictions_breeding_coords$predictions)

ALL_predictions_breeding_coords$Prediction_w_Overlap = ALL_predictions_breeding_coords$predictions

ALL_predictions_breeding_coords$Prediction_w_Overlap[which(ALL_predictions_breeding_coords$Recording_ID %in% breeding_Syll_Loc_Data_OverlapONLY$Recording_ID & ALL_predictions_breeding_coords$predictions == "Pipilo.erythrophthalmus")]= "Pipilo.erythrophthalmus.Overlap"
ALL_predictions_breeding_coords$Prediction_w_Overlap[which(ALL_predictions_breeding_coords$Recording_ID %in% breeding_Syll_Loc_Data_OverlapONLY$Recording_ID & ALL_predictions_breeding_coords$predictions == "Pipilo.maculatus")]= "Pipilo.maculatus.Overlap"

################################################################################
#order dataframe to have incorrect predictions on top

ALL_predictions_breeding_coords_incorrect <- ALL_predictions_breeding_coords[ALL_predictions_breeding_coords$correct == 'Incorrect',]

ALL_predictions_breeding_coords_correct <- ALL_predictions_breeding_coords[ALL_predictions_breeding_coords$correct == 'Correct',]

ALL_predictions_breeding_coords_ordered <- rbind(ALL_predictions_breeding_coords_correct, ALL_predictions_breeding_coords_incorrect)

################################################################################

RFM_map_Breeding_PredictOverlap <- ggplot(ALL_predictions_breeding_coords_ordered, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = correct, colour = Prediction_w_Overlap), size = 3, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Prediction", colour="Prediction Class") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.maculatus.Overlap" = "#a6cee3", "Pipilo.erythrophthalmus" = "#960019", "Pipilo.erythrophthalmus.Overlap" = "#FFCCCC")) +
  theme (legend.position = "right") +
  theme_classic()
RFM_map_Breeding_PredictOverlap
