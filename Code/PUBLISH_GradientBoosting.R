
library(gbm)
library(caret)
library(xgboost)

#read in data
ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv") #2788

#for breeding season do uncomment the following
#ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv") #2437
#ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(1,3:21)]

#choose only song feature columns and species column
data <- ALL_Syll_Loc_Data_XCandML_data[,c(4:20)]

#normalize data
maxs <- apply(data[, -ncol(data)], 2, max)
mins <- apply(data[, -ncol(data)], 2, min)
scaled <- as.data.frame(scale(data[, -ncol(data)], center = mins, scale = maxs - mins))

#add the species column back to the scaled data
scaled$Species <- data$Species

data <- scaled

#assuming your data frame is named `data` and the target variable is `Species`
set.seed(123) # for reproducibility
total_samples <- nrow(data)
train_size <- floor(0.75 * total_samples)

#ensure exact number of samples in the training set
train_indices <- sample(seq_len(total_samples), size = train_size)

trainSet <- data[train_indices, ]
testSet <- data[-train_indices, ] 

#ensure the Species column is a factor
trainSet$Species <- as.factor(trainSet$Species)

#downsample the training set to have equal numbers of each species
trainSet_balanced <- downSample(x = trainSet[, -ncol(trainSet)], y = trainSet$Species)

#combine the downsampled data
trainSet <- cbind(trainSet_balanced, Species = trainSet_balanced$Class)
trainSet$Class <- NULL

#check the class distribution in the balanced training set
table(trainSet$Species)


#convert factor to integer codes and subtract 1 to have zero-based labels
trainSet$Species <- as.integer(as.factor(trainSet$Species)) - 1
testSet$Species <- as.integer(as.factor(testSet$Species)) - 1

dtrain <- xgb.DMatrix(data = as.matrix(trainSet[, -which(names(trainSet) == "Species")]), label = trainSet$Species)
dtest <- xgb.DMatrix(data = as.matrix(testSet[, -which(names(testSet) == "Species")]), label = testSet$Species)

num_class <- length(unique(trainSet$Species))  # number of classes

params <- list(
  objective = "multi:softmax", #for binary replace with "binary:logistic"
  num_class = num_class,
  eval_metric = "mlogloss" #for binary, replace with "logloss"
)

xgbModel <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10
)

importance_matrix <- xgb.importance(model = xgbModel)
xgb.plot.importance(importance_matrix)

testSet$predicted_species <- predict(xgbModel, dtest)

#convert predictions back to original factor levels
testSet$predicted_species <- as.factor(testSet$predicted_species + 1)  # add 1 to get back to original factor levels

#confusion Matrix
cm <- confusionMatrix(testSet$predicted_species, as.factor(testSet$Species +1))
print(cm)

accuracy <-cm$overall["Accuracy"]
accuracy #ALL: 0.8823529 #Breeding: 0.9244663

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy #ALL: 0.8883157 #Breeding: 0.9252043

#calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result #ALL: 0.7583054  #Breeding: 0.8391276 

Mcnemar_pvalue <- cm$overall["McnemarPValue"] 
Mcnemar_pvalue #ALL: 0.0001110421 #Breeding: 0.05527028

#########
#bind recording ID with predictions to see what id's were incorrectly predicted
#extract the recording_ID from the original dataset
Recording_ID <- ALL_Syll_Loc_Data_XCandML_data$Recording_ID

#subset the recording_IDs for the test set
test_recording_ID <- Recording_ID[-train_indices]


#create the prediction dataframe
GBM_prediction_dataframe <- data.frame(
  Recording_ID = test_recording_ID,
  Predicted = as.numeric(testSet$predicted_species),
  Actual = as.numeric(testSet$Species)
)

#convert predictions back to original factor levels
GBM_prediction_dataframe$Actual <- as.factor(GBM_prediction_dataframe$Actual + 1)


GBM_prediction_dataframe <- GBM_prediction_dataframe %>% mutate(correct = case_when(
  Actual == Predicted ~ "Correct",
  TRUE ~ "Incorrect"
)) 

#display the prediction dataframe
print(GBM_prediction_dataframe) 

################################################################################
############################# Permutation Test #################################
library(caret)

#calculate the original accuracy
original_accuracy <- sum(GBM_prediction_dataframe$Actual == GBM_prediction_dataframe$Predicted) / nrow(GBM_prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies <- replicate(n_permutations, {
  permuted_species <- sample(GBM_prediction_dataframe$Actual) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == GBM_prediction_dataframe$Predicted) / nrow(GBM_prediction_dataframe)
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

