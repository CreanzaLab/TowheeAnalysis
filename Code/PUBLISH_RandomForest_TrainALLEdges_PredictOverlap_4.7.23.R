library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(stringr)

# Read in the dataset
ALL_Syll_Loc_Data_XCandML_data <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv")

ALL_Syll_Loc_Data_XCandML_data$Species <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$Species, " ", ".") 
ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap, " ", ".")

ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data #3515

###all data except for zone of overlap
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus" 
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_NoOverlap, ] #3216
ALL_Syll_Loc_Data_NoOverlap <- subset(ALL_Syll_Loc_Data_NoOverlap, select = c(1, 4:20))


###all zone of overlap data #will use this for new predictions
# Read in the new test dataset
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_OverlapONLY, ] #299
ALL_Syll_Loc_Data_OverlapONLY_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))
new_test_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1,4:19))

################################################################################

# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(ALL_Syll_Loc_Data_NoOverlap), nrow(ALL_Syll_Loc_Data_NoOverlap) - nrow(new_test_data)) #2917 #reserving same size subsample as the number that will be tested in order to compare predictions
train_data <- ALL_Syll_Loc_Data_NoOverlap[train_index, ] #2917 total training #1758 eastern #1159 spotted
test_data <- ALL_Syll_Loc_Data_NoOverlap[-train_index, ] #299 total testing 


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #1159
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #1758

train_data_spotted <- n_spotted[sample(nrow(n_spotted), 1062), ] #1062
train_data_eastern <- n_eastern[sample(nrow(n_eastern), 1062), ] #1062



train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #2124 
train_data_downsampled <- subset(train_data_downsampled, select = -c(1))


# If it's not already a factor, convert it to a factor
class(train_data_downsampled$Species) #check class
train_data_downsampled$Species <- as.factor(train_data_downsampled$Species) #convert to factor


################################################################################
# Create a trainControl object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE)

# Define the parameter grid to search over
grid <- expand.grid(mtry = 1:16, min.node.size = c(1:10), splitrule = c("gini"))


# Train the model using the downsampled training set
bestTunemodel <- train(Species ~ ., data = train_data_downsampled, method = "ranger", num.trees = 500, tuneGrid = grid, trControl = ctrl)

################################################################################

# Fit a random forest model using the downsampled training set
model_ALLedges <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)


# Make predictions on the test set
predictions <- predict(model_ALLedges, data = test_data)$predictions

# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species)  

plot_data_pred <- cbind(test_data, predictions) #299

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################

# Make predictions on the new test set
new_predictions <- predict(model_ALLedges, data = new_test_data)$predictions

# Calculate the accuracy of the model on the new test set
new_accuracy <- mean(new_predictions == ALL_Syll_Loc_Data_OverlapONLY_data$Species) 

#bind original with predictions
plot_data_pred_new <- cbind(ALL_Syll_Loc_Data_OverlapONLY_data, new_predictions) #299

plot_data_pred_new <- plot_data_pred_new %>% mutate(correct = case_when(
  Species == new_predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 

################################################################################

#rbind 
colnames(plot_data_pred_new)[19] <- "predictions" 
ALL_predictions_ALL <- rbind(plot_data_pred_new, plot_data_pred) #598
ALL_predictions_ALL_coords <- merge(ALL_predictions_ALL, subset(ALL_Syll_Loc_Data_XCandML_data, select = c(1:19,22))) #598


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

