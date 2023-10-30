library(ranger)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(stringr)
library(plyr)

# Read in the dataset
breeding_Syll_Loc_Data_XCandML_data <- read.csv("C:/../PUBLISH_breedingseasonONLY_XCandML_songdata_with_overlapzone_4.1.23.csv")

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
unused_test_data <- subset(breeding_Syll_Loc_Data_OverlapONLY, select = c(1,4:19))



####
# Read in the new test dataset
HybridUnsure_SongData_subset <- read.csv("C:/../PUBLISH_Towhee_Hybrid_Unsure_Song_Data_forAnalysis.csv")
new_test_data <- HybridUnsure_SongData_subset #31

################################################################################

# Set a seed for reproducibility
set.seed(.Random.seed)

# Randomly split the data into training and test sets
train_index <- sample(1:nrow(breeding_Syll_Loc_Data_NoOverlap), nrow(breeding_Syll_Loc_Data_NoOverlap) - nrow(unused_test_data)) #2615 #reserving same size subsample as the number that will be tested in order to compare predictions
train_data <- breeding_Syll_Loc_Data_NoOverlap[train_index, ] #2615 total training #1630 eastern #985 spotted
test_data <- new_test_data #31

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
grid <- expand.grid(mtry = 1:16, min.node.size = c(1:10), splitrule = c("gini"))

# Train the model using the downsampled training set
bestTunemodel <- train(Species ~ ., data = train_data_downsampled, method = "ranger", 
                       num.trees = 500, tuneGrid = grid, trControl = ctrl)

################################################################################

# Fit a random forest model using the downsampled training set
model_breeding_edges <- ranger(Species ~ ., data = train_data_downsampled, 
                               num.trees = 500, mtry = bestTunemodel[["bestTune"]][["mtry"]], 
                               min.node.size = bestTunemodel[["bestTune"]][["min.node.size"]], 
                               importance = "permutation", local.importance = TRUE)

# Make predictions on the test set
predictions <- predict(model_breeding_edges, data = test_data)$predictions

# plot
plot_data_pred <- cbind(test_data, predictions) #31
plot_data_pred$predictions <- as.character(plot_data_pred$predictions)


################################################################################
################################################################################

#rbind 
Filtered_HybridUnsure_List_metadata <- read.csv("C:/../PUBLISH_Hybrid_ML_FilestoAnalyze_FINAL.csv")
Filtered_HybridUnsure_List_metadata <- subset(Filtered_HybridUnsure_List_metadata, select = c(1, 17, 18))
names(Filtered_HybridUnsure_List_metadata)[1] <- "Recording_ID"


hybrid_predictions_breeding_coords <- join(plot_data_pred, Filtered_HybridUnsure_List_metadata , by = "Recording_ID")

################################################################################
################################################################################
################################################################################

RFM_map_breeding_PredictOverlap_hybrid <- ggplot(hybrid_predictions_breeding_coords, show.legend = FALSE) +
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
RFM_map_breeding_PredictOverlap_hybrid #PDF 6.55 x 3.77
