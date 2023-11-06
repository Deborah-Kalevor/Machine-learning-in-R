
### DATA SET JOIN ####
setwd("C:/Users/HP/Desktop/R Assignment")
set.seed(3085022)
library(tidymodels)
library(dplyr)

### UPLOAD DATA 
df1<-read.csv("C:/Users/HP/Desktop/R Assignment/Football Database/teams.csv")
glimpse(df1)

df2<- read.csv("C:/Users/HP/Desktop/R Assignment/Football Database/teamstats.csv")
glimpse(df2)

# Perform the left join with the "team ID" column as the join condition
joined_data <- left_join(df1, df2, by = "teamID", suffix = c(".df1", ".df2"), 
                              relationship = "many-to-many")
glimpse(joined_data)
view(joined_data)

###### CHECKING FOR MISSING VALUES#######

# Remove missing values in the data
data <- na.omit(joined_data)
 glimpse(data)
 
# Check the number of missing values in the cleaned dataset 'data'
data_clean <- sum(is.na(data))
print(data_clean)


### VISUALIZE THE DATA ####
library(gridExtra)
library(ggplot2)

######GRID 1
Name <- ggplot(joined_data, aes(x=name)) + ggtitle("Team Names") + xlab("Team Names") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Results <- ggplot(joined_data, aes(x=result)) + ggtitle("Performance") + xlab("Performance") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Location <- ggplot(joined_data, aes(x=location)) + ggtitle("Game Location") + xlab("Game Location") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(Name, Results, Location,ncol=2)

###### GRID 2
Goals <- ggplot(joined_data, aes(x=goals)) + ggtitle("Goals Scored") + xlab("Goals Scored") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Shots <- ggplot(joined_data, aes(x=shots)) + ggtitle("Shots Played") + xlab("Shots Played") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Coners <- ggplot(joined_data, aes(x=corners)) + ggtitle("Coners Played") + xlab("Coners Played") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(Goals, Shots, Coners,ncol=2)

### Grid 3
Red_cards <- ggplot(joined_data, aes(x=redCards)) + ggtitle("Red cards") + xlab("Red cards") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Yello_cards <- ggplot(joined_data, aes(x=yellowCards)) + ggtitle("Yello cards") + xlab("Yello cards") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

Fouls <- ggplot(joined_data, aes(x=fouls)) + ggtitle("Fouls") + xlab("Fouls") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(Red_cards, Yello_cards, Fouls,ncol=2)


# Define the categorical variables to be encoded
categorical_vars <- c("name", "location", "result")

# Create a copy of the original data
encoded_data <- data

# Iterate over each categorical variable
for (var in categorical_vars) {
  # Get the unique categories in the variable
  categories <- unique(encoded_data[[var]])
  
  # Create a mapping of categories to numerical labels
  labels <- seq_along(categories)
  category_mapping <- setNames(labels, categories)
  
  # Replace the categorical values with numerical labels in the data
  encoded_data[[var]] <- category_mapping[encoded_data[[var]]]
  
  # Convert the variable back to a factor with the same levels
  encoded_data[[var]] <- factor(encoded_data[[var]], levels = labels)
}

glimpse(encoded_data)
view(encoded_data)

#select a subset of variables of interest to predict results 
results <- encoded_data%>%
  select(result,location, goals, shots,shotsOnTarget, fouls,corners, yellowCards,redCards)
glimpse(results)


# Splitting the data 
results_split <- initial_split(results, prop = 0.7, strata = result)

# Get the training and testing data with stratification based on "Result"
results_training <- training(results_split)
results_testing <- testing(results_split)

glimpse(results_testing)
glimpse(results_training)

################################################################################
#Building a Feature Engineering Pipeline

#Finding Correlated Predictor Variables
results_training%>%
  select_if(is.numeric)%>%
  cor()

#Normalization  and  Removing multi-collinearity
results_norm <-recipe(result ~.,
                        data =results_training)%>%
  step_corr(all_numeric(),threshold = 0.9)%>%
  step_normalize(all_numeric())

results_norm

#Transforming the Test Data
results_norm%>%
  prep(training = results_training)%>%
  bake(new_data= results_testing)

#Recipe Object
results_recipe<- recipe(result ~.,
                       data = results_training)
#Train Recipe with Prep Function
results_recipe_prep <- results_recipe%>%
  prep(training= results_training)


#Preprocess Training Data
training_prep <- results_recipe_prep%>%
  bake(new_data = NULL)

glimpse(training_prep)

#Pre-process the Test Data
testing_prep <- results_recipe_prep%>%
  bake(new_data = results_testing)

glimpse(testing_prep)

####################################################################################
# MODELS FITTING 

####### RANDOM FOREST MODEL ##########

# Fit Random Forest Model for result prediction 
library(randomForest)
library(caret)

rand_model <- randomForest(formula = result ~ .,
                           data = training_prep,
                           ntree = 75)

# Predict using the random forest model on the testing data
predict_results <- predict(rand_model, newdata = testing_prep, type = "class")

# Calculate the confusion matrix
conf_matrix <- confusionMatrix(predict_results, testing_prep$result)
print(conf_matrix)

#Important Features in a Random Forest
randmodel2 <- randomForest(results,
                           data = results,
                           ntree = 75,
                           importance = TRUE)

randmodel2

# visualize 
randomForest::varImpPlot(rand_model,
                         sort= TRUE,
                         main="Variable Importance Plot")

###########################################
#  CALCULATING THE AUC ROC CURVE 
library(pROC)

# Get unique class labels
class_labels <- unique(testing_prep$result)

# Function to convert multiclass to binary for a specific class
convert_to_binary <- function(class_label, actual_labels) {
  binary_labels <- ifelse(actual_labels == class_label, "Positive", "Negative")
  return(factor(binary_labels, levels = c("Positive", "Negative")))
}


# Calculate ROC curves and AUC values for each class
roc_curves <- lapply(class_labels, function(class_label) {
  binary_actual <- convert_to_binary(class_label, testing_prep$result)
  binary_predicted <- convert_to_binary(class_label, predict_results)
  return(roc(binary_actual, as.numeric(binary_predicted)))
})

# Plot ROC curves for each class
plot(roc_curves[[1]], col = "blue", main = "ROC Curves for Random Forest", lwd = 2)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], add = TRUE, col = rainbow(length(class_labels))[i], lwd = 2)
}

# Calculate macro-average AUC value
auc_values <- sapply(roc_curves, function(x) x$auc)
macro_auc_value <- mean(auc_values)

# Print macro-average AUC value
cat("Macro-average AUC-ROC value:", macro_auc_value, "\n")

# Add diagonal reference line for random classification
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright", legend = paste("Class", class_labels, "AUC =", 
              round(auc_values, 2)), col = rainbow(length(class_labels)), lwd = 2)



######################################################################################
###  K NEAREST NEIGHBOR
library(class)

#Pre-processing 
sum(is.na(joined_data))

KNN_clean<- sum(complete.cases(joined_data))

glimpse(KNN_clean)

#Count the number of signs of each type
table(joined_data$result)

# Define the categorical variables to be encoded
categorical_vars <- c("name", "location")

# Create a copy of the original data
KNN_encoded_data <- joined_data

# Iterate over each categorical variable
for (var in categorical_vars) {
  # Get the unique categories in the variable
  categories <- unique(KNN_encoded_data[[var]])
  
  # Create a mapping of categories to numerical labels
  labels <- seq_along(categories)
  category_mapping <- setNames(labels, categories)
  
  # Replace the categorical values with numerical labels in the data
  KNN_encoded_data[[var]] <- category_mapping[KNN_encoded_data[[var]]]
  
  # Convert the variable back to a factor with the same levels
  KNN_encoded_data[[var]] <- factor(KNN_encoded_data[[var]], levels = labels)
}

glimpse(KNN_encoded_data)

#select a subset of variables of interest to predict results 
KNN_results <- KNN_encoded_data%>%
  select(result,location, goals, shots,shotsOnTarget, fouls,corners, yellowCards,redCards)
glimpse(KNN_results)

# Splitting the data 
KNN_results_split <- initial_split(KNN_results, prop = 0.7, strata = result)

# Get the training and testing data with stratification based on "Result"
KNN_results_training <- training(KNN_results_split)
KNN_results_testing <- testing(KNN_results_split)

glimpse(KNN_results_testing)
glimpse(KNN_results_training)

KNN_results_training <- na.omit(KNN_results_training)
glimpse(KNN_results_training)

#Create Train Labels
train_labels <- KNN_results_training$result

#Fit KNN Model
results_Pred <-knn(train = KNN_results_training[-1], test = KNN_results_testing[-1], cl= train_labels )
results_Pred

results_actual <- KNN_results_testing$result
 results_actual

#Create a Confusion Matrix
 conf_matrix <- table(results_Pred, results_actual)
 
 conf_matrix
 
#TP: 362+1019+1226 =2607
#FP: (sum of all values in correspomnding columns except TP): (535+372) + (538+345) + (372+304)= 2466
#FN: (sum of all values in correspomnding rows except TP): (538+372) + (535+304) + (372+345) = 2246
#TN: 0

#Accuracy
mean(results_Pred == results_actual)

## VISUALIZATION 
# Create a heatmap
heatmap(conf_matrix, 
        col = cm.colors(256),  
        scale = "none",        
        margins = c(5, 10),    
        main = "Confusion Matrix",
        xlab = "Actual",
        ylab = "Predicted")


########### AUC ROC CURVE FOR KNN MODEL

# Fit KNN model and predict on the test data
results_Pred <- knn(train = KNN_results_training[-1], test = KNN_results_testing[-1],
                    cl = train_labels)

results_actual <- KNN_results_testing$result

# Get unique class labels
class_labels <- unique(KNN_results$result)

# Convert actual labels to binary for each class
binary_actuals <- lapply(class_labels, function(class_label, actual_labels) {
  binary_actual <- ifelse(actual_labels == class_label, 1, 0)
  return(binary_actual)
}, actual_labels = results_actual)

# Calculate ROC curves and AUC values for each class
roc_curves <- lapply(1:length(class_labels), function(i) {
  roc_obj <- roc(binary_actuals[[i]], as.numeric(results_Pred == class_labels[i]))
  return(roc_obj)
})

# Calculate macro-average AUC value
auc_values <- sapply(roc_curves, function(x) x$auc)
macro_auc_value <- mean(auc_values, na.rm = TRUE)

# Print macro-average AUC value
cat("Macro-average AUC-ROC value:", macro_auc_value, "\n")

# Plot ROC curves for each class
plot(roc_curves[[1]], col = "blue", main = "ROC Curves for KNN Model", lwd = 2)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], add = TRUE, col = rainbow(length(class_labels))[i], lwd = 2)
}

# Add diagonal reference line for random classification
abline(a = 0, b = 1, lty = 2, col = "gray")

# Add legend
legend("bottomright", legend = paste("Class", class_labels, "AUC =", round(auc_values, 2)), col = rainbow(length(class_labels)), lwd = 2)



######## DECISION TREE #############
library(rpart)
library(rpart.plot)
library(party)

# USING THE TRAINING AND TESTING DATA OF KNN
glimpse(KNN_results_training)
glimpse(KNN_results_testing)

decision_tree_model <- rpart(result ~ ., data = KNN_results_training, method = "class")


# predictions using the decision tree model
predictions <- predict(decision_tree_model, newdata = KNN_results_testing, 
                       type = "class")

# confusion matrix
actual <- KNN_results_testing$result
confusion_matrix <- table(predictions, actual)
print(confusion_matrix)

# accuracy from the confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)

# plotting the decision tree model
rpart.plot(decision_tree_model, extra = 101)


#Plotting Decision tree with predictor variables
training_prep$goals <- factor(training_prep$goals)
training_prep$shotsOnTarget <- factor(training_prep$shotsOnTarget)
training_prep$shots <- factor(training_prep$shots)
tree1 <- ctree(result~goals+shotsOnTarget+shots, training_prep)
plot(tree1)

training_prep$fouls <- factor(training_prep$fouls)
training_prep$corners <- factor(training_prep$corners)
training_prep$yellowCards <- factor(training_prep$yellowCards)
training_prep$location <- factor(training_prep$location)
training_prep$redCards <- factor(training_prep$redCards)

tree2 <- ctree(result~fouls+corners+yellowCards+location+redCards, training_prep)
plot(tree2)


####### AUC ROC CURVE 
# Convert 'actual' and 'predictions' to factors
actual <- factor(actual)
predictions <- factor(predictions)

# Create a list to store ROC objects for each class
roc_curves <- list()

# Calculate AUC-ROC curve for each class (one-vs-all) since there is no inference of D in the test data
for (class_label in levels(actual)) {
  binary_actual <- factor(ifelse(actual == class_label, "Positive", "Negative"), levels = c("Positive", "Negative"))
  binary_predicted <- factor(ifelse(predictions == class_label, "Positive", "Negative"), levels = c("Positive", "Negative"))
  
  roc_obj <- roc(binary_actual, as.numeric(binary_predicted))
  roc_curves[[class_label]] <- roc_obj
}

# Calculate macro-average AUC value
auc_values <- sapply(roc_curves, function(roc_obj) roc_obj$auc)
valid_auc_values <- auc_values[!is.na(auc_values) & sapply(auc_values, is.numeric)]
macro_auc_value <- mean(valid_auc_values, na.rm = TRUE)

# Print macro-average AUC value
cat("Macro-average AUC-ROC value:", macro_auc_value, "\n")

# Plot ROC curves for each class
par(mfrow = c(1, length(roc_curves)))
for (i in seq_along(roc_curves)) {
  plot(roc_curves[[i]], col = rainbow(1), main = paste("ROC Curve for Class", levels(actual)[i]), lwd = 2)
}

# Add legend
legend_labels <- sapply(roc_curves, function(roc_obj) paste("Class", roc_obj$levels[[2]], "AUC =", round(roc_obj$auc, 2)))
legend("bottomright", legend = legend_labels, col = rainbow(length(roc_curves)), lwd = 2)

###################################################################################################
##### NEURAL NETWORKS 
library(neuralnet)

#Lets create a training Dataset
glimpse(testing_prep)
glimpse(training_prep)

na.omit(training_prep)
na.omit(testing_prep)

# Convert all columns to numeric
training_prep <- training_prep %>%
  mutate(across(everything(), as.numeric))
testing_prep <- testing_prep %>%
  mutate(across(everything(), as.numeric))

str(testing_prep)
str(training_prep)

# fit the neural network model on the train Data
NN_train_results <- neuralnet(result ~ ., data = training_prep, hidden = 1, 
                              act.fct = "logistic",
                              linear.output = FALSE)
plot(NN_train_results)
glimpse(NN_train_results)


testing_prep$result <- factor(testing_prep$result, c("1", "2","3"),
                            label= c("W", "L","D"))

Predict <- compute(NN_train_results, testing_prep)
glimpse(Predict)
Predict$net.result

actual <- testing_prep$result

# Extract the predicted probabilities for class "W"
prob_W <- as.vector(Predict$net.result)

# Convert probabilities to binary classes "W", "L", and "D"
threshold <- 0.33
pred <- ifelse(prob_W > threshold, "W", ifelse(prob_W < (1 - threshold), "L", "D"))


# Create the confusion matrix
conf_matrix <- table(actual, pred)
print(conf_matrix)

#Accuracy
mean(pred ==actual)


