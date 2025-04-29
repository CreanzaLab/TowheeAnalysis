#https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/

####can test which features 
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(dplyr)

################################################################################

ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_All_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(4:20)]

#to repeat analysis on only breeding season, read in the following file:
#ALL_Syll_Loc_Data_XCandML_data <- read.csv("~/PUBLISH_Breeding_XCandML_songdata_with_overlapzone_4.1.23_withSamplingRateandFileType_unique.csv")
#ALL_Syll_Loc_Data_XCandML_data <- ALL_Syll_Loc_Data_XCandML_data[,c(5:21)] 

# Convert columns to numeric safely
for(i in 1:16) {
  if(is.factor(ALL_Syll_Loc_Data_XCandML_data[[i]]) || is.character(ALL_Syll_Loc_Data_XCandML_data[[i]])) {
    ALL_Syll_Loc_Data_XCandML_data[[i]] <- as.numeric(as.character(ALL_Syll_Loc_Data_XCandML_data[[i]]))
  } else if(!is.list(ALL_Syll_Loc_Data_XCandML_data[[i]])) {
    ALL_Syll_Loc_Data_XCandML_data[[i]] <- as.numeric(ALL_Syll_Loc_Data_XCandML_data[[i]])
  }
}

ALL_Syll_Loc_Data_XCandML_data$Species <- as.factor(ALL_Syll_Loc_Data_XCandML_data$Species)

# Custom pairs plot
pairs.panels(ALL_Syll_Loc_Data_XCandML_data[1:3],
             gap = 0,
             bg = c("red", "blue")[ALL_Syll_Loc_Data_XCandML_data$Species],
             pch = 21)

################################################################################
################################################################################
#################### Linear Discriminant Analysis ##############################
num_rows <- nrow(ALL_Syll_Loc_Data_XCandML_data)

# Sample indices
set.seed(123) # Setting seed for reproducibility
ind <- sample(1:num_rows, num_rows, replace = FALSE)
train_indices <- ind[1:floor(0.75 * num_rows)]
test_indices <- ind[(floor(0.75 * num_rows) + 1):num_rows]

# Create train and test sets
train_data <- ALL_Syll_Loc_Data_XCandML_data[train_indices, ] #ALL: 2088 #Breeding: 1827
test_data <- ALL_Syll_Loc_Data_XCandML_data[test_indices, ] #ALL: 697 #Breeding: 609

# Identify the species column (assuming it's named "species")
species_col <- "Species"

# Count the number of samples for each species in the training set
species_counts <- train_data %>%
  group_by(!!sym(species_col)) %>%
  summarise(count = n())

# Determine the minimum count of any species
min_count <- min(species_counts$count)

# Downsample each species to the minimum count #all: 1592 #breeding: 1304
balanced_train_data <- train_data %>%
  group_by(!!sym(species_col)) %>%
  sample_n(min_count) %>%
  ungroup()

# Verify the counts to ensure balance
balanced_species_counts <- balanced_train_data %>%
  group_by(!!sym(species_col)) %>%
  summarise(count = n())

# Print the counts for verification
print(balanced_species_counts)



linear <- lda(Species~., balanced_train_data)
linear

coefficients <- linear$scaling[, 1]

#sort the coefficients by their absolute values in descending order to see which feature contributes the most
sorted_coefficients <- sort(abs(coefficients), decreasing = TRUE)
sorted_coefficients

#obtain the correlation matrix
cor_matrix <- cor(balanced_train_data[, -which(names(balanced_train_data) == "Species")])

#Stacked histogram for discriminant function values
p <- predict(linear, balanced_train_data)
ldahist(data = p$x[,1], g = balanced_train_data$Species)

p1 <- predict(linear, balanced_train_data)$class
tab <- table(Predicted = p1, Actual = balanced_train_data$Species)
tab

accuracy_train <- sum(diag(tab))/sum(tab)
accuracy_train

p2 <- predict(linear, test_data)$class
tab1 <- table(Predicted = p2, Actual = test_data$Species)
tab1

ld_scores <- predict(linear, test_data)$x
ld_df <- data.frame(LD1 = ld_scores[, 1], Species = test_data$Species)

dev.off()
#plot test data #PDF: 7x12
ggplot(ld_df, aes(x = LD1, y = 0, color = as.factor(Species))) +
  #geom_point(shape = 19, size = 3.0, alpha = 0.8) + 
  geom_jitter(shape = 19, size = 3.0, alpha = 0.8) + #to jitter the points
  theme_classic() +
  labs(x = "LD1", y = "",  #no need for y-axis label with single dimension
       title = "LDA Plot of LD1") +
  scale_color_manual(values = c("Pipilo maculatus" = "#5C6B9C","Pipilo erythrophthalmus" = "#960019"))


################################################################################
library(irr)
library(caret)
prediction_dataframe <- data.frame()
prediction_dataframe <- as.data.frame(p2)

#add actual species
prediction_dataframe$actual <- as.character(test_data$Species)

#convert to factor with two levels
prediction_dataframe$actual <- as.factor(prediction_dataframe$actual) 
prediction_dataframe$p2 <- as.factor(prediction_dataframe$p2)

# Compute confusion matrix
cm <- confusionMatrix(prediction_dataframe$p2, prediction_dataframe$actual)

# Calculate Cohen's kappa
kappa_result <- cm$overall["Kappa"]
kappa_result #ALL: 0.726319 #Breeding: 0.8019716

Mcnemar_pvalue <- cm$overall["McnemarPValue"] 
Mcnemar_pvalue #ALL: 0.02856768 #Breeding: 0.008071487

#accuracy
accuracy <-cm$overall["Accuracy"]
accuracy #ALL: 0.8680057 #Breeding: 0.9064039

balanced_accuracy <- cm$byClass["Balanced Accuracy"]
balanced_accuracy #ALL: 0.8685229 #Breeding: 0.9090719

################################################################################
############################# Permutation Test #################################
library(caret)

#calculate the original accuracy
original_accuracy <- sum(prediction_dataframe$actual == prediction_dataframe$p2) / nrow(prediction_dataframe)

#set up permutation test
set.seed(123) # For reproducibility
n_permutations <- 1000
permuted_accuracies <- replicate(n_permutations, {
  permuted_species <- sample(prediction_dataframe$actual) # Shuffle the species labels
  permuted_accuracy <- sum(permuted_species == prediction_dataframe$p2) / nrow(prediction_dataframe)
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

