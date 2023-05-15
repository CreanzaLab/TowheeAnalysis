library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)

# Read in the dataset

ALL_Syll_Loc_Data_XCandML_data <- read.csv("C:/Users/xxime/Box/Ximena_Nicole/TOWHEE_ANALYSES_2022-2023/1_PUBLISH_MATERIALS/DATA/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv")
ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data
ALL_Syll_Loc_Data_XCandML_data$Species <- make.names(ALL_Syll_Loc_Data_XCandML_data$Species) #3515
ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- make.names(ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap)

#################################################################################

ALL_Syll_Loc_Data_XCandML <- subset(ALL_Syll_Loc_Data_XCandML_data, select = c(1:20,22)) #3515

################################################################################
###all zone of overlap data #will use this for plotting later
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_OverlapONLY, ] #299
ALL_Syll_Loc_Data_OverlapONLY_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))

################################################################################


data <- ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.maculatus" | ALL_Syll_Loc_Data_XCandML_data$Species == "Pipilo.erythrophthalmus" 
data <- ALL_Syll_Loc_Data_XCandML_data[data, ] 
data <- subset(data, select = c(1, 4:20))


# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(data), 0.75*nrow(data)) #2636 
train_data <- data[train_index, ] #2636 total training #1574 eastern #1062 spotted
test_data <- data[-train_index, ] #879 total testing 


# Downsample the training set
n_spotted <- subset(train_data, train_data$Species == "Pipilo.maculatus") #1062
n_eastern <- subset(train_data, train_data$Species == "Pipilo.erythrophthalmus") #1574

train_data_spotted <- n_spotted[sample(nrow(n_spotted), min(nrow(n_spotted), nrow(n_eastern))), ] #1062
train_data_eastern <- n_eastern[sample(nrow(n_eastern), min(nrow(n_spotted), nrow(n_eastern))), ] #1062

train_data_downsampled <- rbind(train_data_spotted, train_data_eastern) #2124 #1062 eastern #1062 spotted
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
model_ALL <- ranger(Species ~ ., data = train_data_downsampled, num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], importance = "permutation", local.importance = TRUE)



# Make predictions on the test set
test_data$Species <- make.names(test_data$Species)
predictions <- predict(model_ALL, data = test_data)$predictions


# Calculate the accuracy of the model on the test set
accuracy <- mean(predictions == test_data$Species) 

################################################################################

#bind original with predictions
plot_data_pred <- cbind(test_data, predictions) #879

plot_data_pred <- plot_data_pred %>% mutate(correct = case_when(
  Species == predictions ~ "Correct",
  TRUE ~ "Incorrect"
)) 




Predictions_With_Coords <- merge(plot_data_pred, subset(ALL_Syll_Loc_Data_XCandML, select = c(1:19))) #879
Predictions_With_Coords$predictions <- as.character(Predictions_With_Coords$predictions)


################################################################################

Predictions_With_Coords$Prediction_w_Overlap = Predictions_With_Coords$predictions

Predictions_With_Coords$Prediction_w_Overlap[which(Predictions_With_Coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords$predictions == "Pipilo.erythrophthalmus")]= "Pipilo.erythrophthalmus.Overlap"
Predictions_With_Coords$Prediction_w_Overlap[which(Predictions_With_Coords$Recording_ID %in% ALL_Syll_Loc_Data_OverlapONLY$Recording_ID & Predictions_With_Coords$predictions == "Pipilo.maculatus")]= "Pipilo.maculatus.Overlap"

################################################################################
#order dataframe to have incorrect predictions on top

Predictions_With_Coords_incorrect <- Predictions_With_Coords[Predictions_With_Coords$correct == 'Incorrect',]

Predictions_With_Coords_correct <- Predictions_With_Coords[Predictions_With_Coords$correct == 'Correct',]

Predictions_With_Coords_ordered <- rbind(Predictions_With_Coords_correct, Predictions_With_Coords_incorrect)

################################################################################


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
