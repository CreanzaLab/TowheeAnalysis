library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(stringr)
library(plyr)

# Read in the dataset
ALL_Syll_Loc_Data_XCandML_data <- read.csv("C:/../PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23.csv")

ALL_Syll_Loc_Data_XCandML_data$Species <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$Species, " ", ".") 
ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap <- str_replace_all(ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap, " ", ".")

ALL_Syll_Loc_Data_XCandML_data[,c(4:19)] <- log(ALL_Syll_Loc_Data_XCandML_data[,c(4:19)]) #log transform song data #3515

###all data except for zone of overlap
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus" 
ALL_Syll_Loc_Data_NoOverlap <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_NoOverlap, ] #3216
ALL_Syll_Loc_Data_NoOverlap <- subset(ALL_Syll_Loc_Data_NoOverlap, select = c(1, 4:20))


# Read in the new test dataset
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.maculatus.Overlap" | ALL_Syll_Loc_Data_XCandML_data$SpeciesOverlap == "Pipilo.erythrophthalmus.Overlap" 
ALL_Syll_Loc_Data_OverlapONLY <- ALL_Syll_Loc_Data_XCandML_data[ALL_Syll_Loc_Data_OverlapONLY, ] #299
ALL_Syll_Loc_Data_OverlapONLY_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1, 4:20))
unused_test_data <- subset(ALL_Syll_Loc_Data_OverlapONLY, select = c(1,4:19))


####
# Read in the new test dataset
new_test_data <- read.csv("C:/../PUBLISH_Towhee_Hybrid_Unsure_Song_Data_forAnalysis_1.26.24.csv")
new_test_data[,c(2:17)] <- log(new_test_data[,c(2:17)]) #log transform

################################################################################

# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(ALL_Syll_Loc_Data_NoOverlap), nrow(ALL_Syll_Loc_Data_NoOverlap) - 
                        nrow(unused_test_data)) #2917 #reserving same size sub-sample as the number that was tested in the main text in order to compare predictions 

train_data <- ALL_Syll_Loc_Data_NoOverlap[train_index, ] #2917 total training #1758 eastern #1159 spotted

test_data <- new_test_data #31 total testing 


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
bestTunemodel <- train(Species ~ ., data = train_data_downsampled, method = "ranger", 
                       num.trees = 500, tuneGrid = grid, trControl = ctrl)

################################################################################

# Fit a random forest model using the downsampled training set
model_ALLedges <- ranger(Species ~ ., data = train_data_downsampled, 
                         num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], 
                         min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], 
                         importance = "permutation", local.importance = TRUE)


# Make predictions on the test set
predictions <- predict(model_ALLedges, data = test_data)$predictions

# plot
plot_data_pred <- cbind(test_data, predictions) #31
plot_data_pred$predictions <- as.character(plot_data_pred$predictions)


################################################################################
################################################################################

#rbind 
Filtered_HybridUnsure_List_metadata <- read.csv("C:/../PUBLISH_Hybrid_ML_FilestoAnalyze_FINAL.csv")
Filtered_HybridUnsure_List_metadata <- subset(Filtered_HybridUnsure_List_metadata, select = c(1, 17, 18))
names(Filtered_HybridUnsure_List_metadata)[1] <- "Recording_ID"


hybrid_predictions_ALL_coords <- join(plot_data_pred, Filtered_HybridUnsure_List_metadata , by = "Recording_ID")

################################################################################
################################################################################
################################################################################

RFM_map_ALL_PredictOverlap_hybrid <- ggplot(hybrid_predictions_ALL_coords, show.legend = FALSE) +
  geom_sf(data = world, fill = "white") +
  geom_point(aes(x = Longitude, y = Latitude, colour = predictions), size = 3, alpha = 0.8, show.legend = FALSE) +
  coord_sf(xlim  = c(-130, -65), ylim = c(15, 55), expand = FALSE) +
  scale_shape_manual(values = c(16, 8)) +
  theme(plot.title = element_text(size=15)) +
  labs(colour="predictions") +
  scale_color_manual(values = c("Pipilo.maculatus" = "#5C6B9C", "Pipilo.erythrophthalmus" = "#960019")) +
  theme (legend.position = "right") +
  theme_classic() +
  geom_vline(xintercept = -102) +
  geom_vline(xintercept = -91)
RFM_map_ALL_PredictOverlap_hybrid #PDF 6.55 x 3.77
