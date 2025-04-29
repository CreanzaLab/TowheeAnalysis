
#install and load the torch package
#install.packages("torch")
library(torch)
#install_torch()
library(dplyr)

#read in data
all_towhee_data  <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv") #2788
towhee_data <- all_towhee_data[, c(4:20)]

#for breeding season, uncomment the following:
#towhee_data  <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv") #2437
#towhee_data  <- towhee_data[,c(1,3:21)]
#towhee_data <- towhee_data[, c(4:20)]


#convert factors to numeric
towhee_data$Species <- as.numeric(as.factor(towhee_data$Species)) - 1
towhee_data$Species <- towhee_data$Species + 1 # Make labels one-indexed


#normalize data
maxs <- apply(towhee_data[, -ncol(towhee_data)], 2, max)
mins <- apply(towhee_data[, -ncol(towhee_data)], 2, min)
scaled <- as.data.frame(scale(towhee_data[, -ncol(towhee_data)], center = mins, scale = maxs - mins))

#add the species column back to the scaled data
scaled$Species <- towhee_data$Species

#split into training and test sets
set.seed(123) # for reproducibility
indexes <- sample(1:nrow(scaled), size = 0.75 * nrow(scaled)) # 75% for training
train_data <- scaled[indexes,]
test_data <- scaled[-indexes,]

#downsample the training set
library(dplyr)

#count the number of samples per species in the training set
species_counts <- table(train_data$Species)
min_samples <- min(species_counts)

#downsample each species in the training set
downsampled_train_data <- train_data %>%
  group_by(Species) %>%
  sample_n(min_samples) %>%
  ungroup()

#extract labels and features for training and test sets
train_labels <- downsampled_train_data$Species
train_data <- downsampled_train_data[, -ncol(downsampled_train_data)] #remove the species column from training data

test_labels <- test_data$Species
test_data <- test_data[, -ncol(test_data)] #remove the species column from test data

#convert to torch tensors
train_data <- torch_tensor(as.matrix(train_data), dtype = torch_float32())
train_labels <- torch_tensor(train_labels, dtype = torch_int64())
test_data <- torch_tensor(as.matrix(test_data), dtype = torch_float32())
test_labels <- torch_tensor(test_labels, dtype = torch_int64())

torch_manual_seed(123)

net <- nn_module(
  "ClassifierNet",
  initialize = function(input_size, hidden_size, output_size) {
    self$fc1 <- nn_linear(input_size, hidden_size)
    self$fc2 <- nn_linear(hidden_size, hidden_size)
    self$fc3 <- nn_linear(hidden_size, output_size) #for binary use output size = 1
  },
  forward = function(x) {
    x <- self$fc1(x)
    x <- nnf_relu(x)
    x <- self$fc2(x)
    x <- nnf_relu(x)
    x <- self$fc3(x)
    x <- nnf_log_softmax(x, dim = 1) #for binary use, "x <- nnf_sigmoid(x)"
    x
  }
)

input_size <- ncol(train_data)
hidden_size <- 30 #you can change this #64
output_size <- length(unique(towhee_data$Species)) #number of classes

model <- net(input_size, hidden_size, output_size)

loss_fn <- nnf_cross_entropy #for binary use, "loss_fn <- nnf_binary_cross_entropy"
optimizer <- optim_adam(model$parameters, lr = 0.001)

num_epochs <- 100
batch_size <- 32

for (epoch in 1:num_epochs) {
  #shuffle data
  permutation <- sample(nrow(train_data))
  train_data <- train_data[permutation,]
  train_labels <- train_labels[permutation]
  
  for (i in seq(1, nrow(train_data), by = batch_size)) {
    #batch indices
    indices <- i:min(i+batch_size-1, nrow(train_data))
    
    #forward pass
    outputs <- model(train_data[indices,])
    loss <- nnf_cross_entropy(outputs, train_labels[indices]) 
    
    #backward and optimize
    optimizer$zero_grad()
    loss$backward()
    optimizer$step()
  }
  #print loss every 10 epochs
  if (epoch %% 10 == 0) {
    cat(sprintf("Epoch [%d/%d], Loss: %.4f\n", epoch, num_epochs, loss$item()))
  }
}

model$eval()

test_outputs <- model(test_data)
test_loss <- loss_fn(test_outputs, test_labels)

cat(sprintf("Test loss: %.4f\n", test_loss$item())) 

#get the indices of the maximum values (predictions)
max_result <- torch_max(test_outputs$data(), dim = 2)
predicted_indices <- max_result[[2]]

#ensure predicted is a numeric vector
predicted <- as.numeric(predicted_indices)

#ensure test_labels is a numeric vector
test_labels_vector <- as.numeric(test_labels)

################################################################################
library(irr)
library(caret)
prediction_dataframe <- data.frame()
prediction_dataframe <- as.data.frame(predicted)

#add actual species
prediction_dataframe$actual <- as.numeric(test_labels_vector)

#convert to factor with two levels
prediction_dataframe$actual <- as.factor(prediction_dataframe$actual) 
prediction_dataframe$predicted <- as.factor(prediction_dataframe$predicted)

# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$predicted, prediction_dataframe$actual)


accuracy <-cm$overall["Accuracy"]
accuracy #ALL: 0.8995696 #Breeding: 0.908046

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy #ALL: 0.9064281 #Breeding: 0.9170857 

kappa_result <- cm$overall["Kappa"]
kappa_result #ALL: 0.7936754 #Breeding: 0.8077977

Mcnemar_pvalue <- cm$overall["McnemarPValue"] 
Mcnemar_pvalue #ALL: 2.873077e-05 #Breeding: 1.034611e-05 


#########
#bind recording ID with predictions to see what id's were incorrectly predicted
#extract the recording_ID from the original dataset
Recording_ID <- all_towhee_data$Recording_ID

#subset the recording_IDs for the test set
test_recording_ID <- Recording_ID[-indexes]

#create the prediction dataframe
Torch_prediction_dataframe <- data.frame(
  Recording_ID = test_recording_ID,
  Predicted = as.numeric(predicted),
  Actual = as.numeric(test_labels_vector)
)

Torch_prediction_dataframe <- Torch_prediction_dataframe %>% mutate(correct = case_when(
  Actual == Predicted ~ "Correct",
  TRUE ~ "Incorrect"
)) 

#display the prediction dataframe
print(Torch_prediction_dataframe) 

################################################################################
############################# Permutation Test #################################
library(caret)

#calculate the original accuracy
original_accuracy <- sum(Torch_prediction_dataframe$Actual == Torch_prediction_dataframe$Predicted) / nrow(Torch_prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies <- replicate(n_permutations, {
  permuted_species <- sample(Torch_prediction_dataframe$Actual) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == Torch_prediction_dataframe$Predicted) / nrow(Torch_prediction_dataframe)
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
