library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)

# Read in the dataset

breeding_Syll_Loc_Data_XCandML_data <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_4.1.23.csv")
breeding_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(breeding_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data
breeding_Syll_Loc_Data_XCandML_data$Species <- make.names(breeding_Syll_Loc_Data_XCandML_data$Species) #3037
breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- make.names(breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap) #3037


breeding_Syll_Loc_Data_XCandML <- subset(breeding_ALL_Syll_Loc_Data_XCandML_data, select = c(1:20, 22)) #3037

################################################################################
#use this to merge for plotting later
breeding_Syll_Loc_Data_OverlapONLY <- breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | breeding_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
breeding_Syll_Loc_Data_OverlapONLY <- breeding_Syll_Loc_Data_XCandML_data[breeding_Syll_Loc_Data_OverlapONLY, ] #211
breeding_Syll_Loc_Data_OverlapONLY_data <- subset(breeding_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))
################################################################################


data <- breeding_ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.maculatus" | breeding_ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.erythrophthalmus" 
data <- breeding_ALL_Syll_Loc_Data_XCandML_data[data, ] #3037
data <- subset(data, select = c(1, 4:20))


# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(data), 0.75*nrow(data)) #2277 
train_data <- data[train_index, ] #2277 total training #1424 eastern #853 spotted
test_data <- data[-train_index, ] #760 total testing 


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #853
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #1424

train_data_spotted <- n_spotted[sample(nrow(n_spotted), min(nrow(n_spotted), nrow(n_eastern))), ] #853
train_data_eastern <- n_eastern[sample(nrow(n_eastern), min(nrow(n_spotted), nrow(n_eastern))), ] #853

train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #1706 #853 eastern #853 spotted
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
model_breeding <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)



# Make predictions on the test set
test_data$Species <- make.names(test_data$Species)
predictions <- predict(model_breeding, data = test_data)$predictions


# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species)  

################################################################################

#bind original with predictions
plot_data_pred <- cbind(test_data, predictions) #760

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 



Predictions_With_Coords_breeding <- merge(plot_data_pred, subset(breeding_Syll_Loc_Data_XCandML_data, select = c(1:19))) #760
Predictions_With_Coords_breeding$predictions <- as.character(Predictions_With_Coords_breeding$predictions)


################################################################################

Predictions_With_Coords_breeding$Prediction_w_Overlap = Predictions_With_Coords_breeding$predictions

Predictions_With_Coords_breeding$Prediction_w_Overlap[which(Predictions_With_Coords_breeding$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords_breeding$predictions == "Pipilo.erythrophthalmus")]= "Pipilo.erythrophthalmus.Overlap"
Predictions_With_Coords_breeding$Prediction_w_Overlap[which(Predictions_With_Coords_breeding$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords_breeding$predictions == "Pipilo.maculatus")]= "Pipilo.maculatus.Overlap"

################################################################################
#order dataframe to have incorrect predictions on top

Predictions_With_Coords_breeding_incorrect <- Predictions_With_Coords_breeding[Predictions_With_Coords_breeding$correct == 'Incorrect',]

Predictions_With_Coords_breeding_correct <- Predictions_With_Coords_breeding[Predictions_With_Coords_breeding$correct == 'Correct',]

Predictions_With_Coords_breeding_ordered <- rbind(Predictions_With_Coords_breeding_correct, Predictions_With_Coords_breeding_incorrect)

################################################################################

RFM_map_Breeding_PredictBreeding <- ggplot(Predictions_With_Coords_breeding_ordered, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, shape = correct, colour = Prediction_w_Overlap), size = 3.0, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(shape="Prediction", colour="Prediction Class") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.maculatus.Overlap" = "#a6cee3", "Pipilo.erythrophthalmus" = "#960019", "Pipilo.erythrophthalmus.Overlap" = "#FFCCCC")) +
  theme(legend.position = "right") +
  theme_classic()
RFM_map_Breeding_PredictBreeding

