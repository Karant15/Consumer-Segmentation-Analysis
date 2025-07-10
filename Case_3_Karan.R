###----------Consumer Segmentation Analytics---------------------
## Loading necessary libraries
library(readr)
library(fastDummies)
library(ggplot2)
require(cluster)
library(stats)
library(ggcorrplot)
library(Boruta)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(class)
library(randomForest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(mice)
library(gridExtra)
library(corrplot)
library(ROSE)
library(pROC)
library(C50)
library(factoextra)
library(cluster)
library(adabag)
library(class)
# Loading the data
data <- read.csv("C:/Users/13142/Desktop/M.S/Analytical Practicum/Case 3/Consumer.csv")
str(data)
head(data)

# Checking for missing values
colSums(is.na(data))

# Checking for zero values in numeric columns
sapply(select_if(data, is.numeric), function(x) sum(x == 0))

# Replacing zeros with NA's
data$EDU[data$EDU == 0] <- NA
data$HS[data$HS == 0] <- NA
data$Affluence.Index[data$Affluence.Index == 0] <- NA
data$SEX[data$SEX == 0] <- NA
data$CS[data$CS == 0] <- NA

# Impute missing values
# Replacing NA and zero values in 'Affluence.Index' with the mean
data <- data %>% 
  mutate(Affluence.Index = ifelse(is.na(Affluence.Index) | Affluence.Index == 0, 
                                  mean(Affluence.Index, na.rm = TRUE), 
                                  Affluence.Index))

# Defining a function to calculate mode
calculate_mode <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}

# Replacing NA and zero values in 'EDU' with the mode
mode_of_edu <- calculate_mode(data$EDU)
data <- data %>% 
  mutate(EDU = ifelse(is.na(EDU) | EDU == 0, mode_of_edu, EDU))

# Replacing NA and zero values in 'SEX' with the mode
mode_sex <- calculate_mode(data$SEX)
data <- data %>%
  mutate(SEX = ifelse(is.na(SEX) | SEX == 0, mode_sex, SEX))

# Replacing NA and zero values in 'HS' with the mode
mode_hs <- calculate_mode(data$HS)
data <- data %>%  
  mutate(HS = ifelse(is.na(HS) | HS == 0, mode_hs, HS))

# Replacing NA and zero values in 'CS' with the mode
mode_cs <- calculate_mode(data$CS)
data <- data %>%  
  mutate(CS = ifelse(is.na(CS) | CS == 0, mode_cs, CS))

# Showing the count of zero's after handling them
sapply(select_if(data, is.numeric), function(x) sum(x == 0))

# Changing the values of HS and CHILD column 
# If HS < CHILD then we put +2 to the household as parents provide the meaningfulness
filtered_data <- data %>% filter(HS < CHILD)

# Display the quantity of rows that meet this condition
quantity <- nrow(filtered_data)
print(paste("Quantity of records where HS < CHILD:", quantity))

# Modify HS by adding 2 where HS < CHILD
data <- data %>% mutate(HS = ifelse(HS < CHILD, HS + 2, HS))

##### Data Exploration #####

# Histogram for Total Volume
ggplot(data, aes(x = Total.Volume)) + 
  geom_histogram(binwidth = 100, fill = "purple", color = "gray") + 
  theme_minimal() + 
  labs(title = "Distribution of Total Volume")

# Scatter plot of Total Volume vs Average Price
ggplot(data, aes(x = Avg..Price, y = Total.Volume)) + 
  geom_point(alpha = 0.5, color = "darkgreen") + 
  labs(title = "Total Volume vs Average Price") + 
  theme_minimal()

# Bar plot for Eating Habit
ggplot(data, aes(x = factor(FEH))) + 
  geom_bar(fill = "firebrick") + 
  labs(title = "Eating Habit Distribution", x = "Eating Habit", y = "Count") +
  theme_minimal()

# Density plot for Promotion Volumes
ggplot(data) + 
  geom_density(aes(x = Pur.Vol.No.Promo....), fill = "darkblue", alpha = 0.5) + 
  geom_density(aes(x = Pur.Vol.Promo.6..), fill = "darkorange", alpha = 0.5) + 
  geom_density(aes(x = Pur.Vol.Other.Promo..), fill = "darkgreen", alpha = 0.5) +
  labs(title = "Distribution of Promotion Volumes", x = "Percentage Volume", y = "Density") +
  theme_minimal() +
  scale_fill_discrete(name = "Promotion Type")

# Boxplot for Brand Runs by Socioeconomic Class
ggplot(data, aes(x = factor(SEC), y = Brand.Runs)) + 
  geom_boxplot(fill = "darkred") + 
  labs(title = "Brand Loyalty (Brand Runs) by Socioeconomic Class", x = "Socioeconomic Class", y = "Brand Runs") +
  theme_minimal()

# Grouping data by SEC and summarizing key metrics
sec_summary <- data %>%
  group_by(SEC) %>%
  summarise(
    avg_volume = mean(Total.Volume, na.rm = TRUE),
    avg_transactions = mean(No..of..Trans, na.rm = TRUE),
    avg_price = mean(Avg..Price, na.rm = TRUE)
  )

# Plotting Total Volume, Number of Transactions, and Avg Price by SEC
ggplot(sec_summary, aes(x = factor(SEC))) +
  geom_bar(aes(y = avg_volume), stat = "identity", fill = "slateblue") +
  geom_bar(aes(y = avg_transactions), stat = "identity", fill = "tomato") +
  geom_bar(aes(y = avg_price), stat = "identity", fill = "chocolate") +
  labs(title = "Average Volume, Transactions, and Price by Socioeconomic Class", 
       x = "Socioeconomic Class", y = "Average Metrics") +
  theme_minimal()

# Scatter plot of Brand Runs vs Affluence Index with linear smoothing line
ggplot(data, aes(x = Affluence.Index, y = Brand.Runs)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Relationship between Affluence Index and Brand Runs",
       x = "Affluence Index", y = "Brand Runs") +
  theme_minimal()

# Extracting relevant columns for brand codes and percentage volume
# Replacing the column names with the actual names in dataset if they differ
brand_data <- data.frame(
  Brand = c("Br..Cd..57..144", "Br..Cd..55", "Br..Cd..272", "Br..Cd..286", 
            "Br..Cd..24", "Br..Cd..481", "Br..Cd..352", "Br..Cd..5", "Others.999"),
  Volume = c(data$Br..Cd..57..144, data$Br..Cd..55, 
             data$Br..Cd..272, data$Br..Cd..286, data$Br..Cd..24, 
             data$Br..Cd..481, data$Br..Cd..352, data$Br..Cd..5, 
             data$Others.999)
)

# Plotting the bar chart
ggplot(brand_data, aes(x = Brand, y = Volume, fill = Brand)) +
  geom_bar(stat = "identity") +
  labs(title = "Brand-wise Percentage of Volume Purchased", 
       x = "Brand Code", 
       y = "Percentage Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Extract the relevant columns for price categories
price_category_data <- data.frame(
  Category = c("Pr Cat 1", "Pr Cat 2", "Pr Cat 3", "Pr Cat 4"),
  Percentage_Volume = c(mean(data$Pr.Cat.1, na.rm = TRUE), 
                        mean(data$Pr.Cat.2, na.rm = TRUE), 
                        mean(data$Pr.Cat.3, na.rm = TRUE), 
                        mean(data$Pr.Cat.4, na.rm = TRUE))
)

# Plotting the bar chart for price categories
ggplot(price_category_data, aes(x = Category, y = Percentage_Volume, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Volume Purchased by Price Category", 
       x = "Price Category", 
       y = "Percentage Volume") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Removing the Member.id column
data <- data %>% select(-Member.id)

### Creating the Target Variable & Clustering ###
# Re-scaling the data
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalized_data <- as.data.frame(lapply(data, Normalize))

# Selecting all the variables for purchasing behaviour
data_purchase_behaviour <- normalized_data %>%
  select(-SEC, -FEH, -MT, -SEX, -AGE, -EDU, -HS, -CHILD, -CS, -Affluence.Index, -Pr.Cat.1, -Pr.Cat.2, -Pr.Cat.3,
         -Pr.Cat.4, -PropCat.5, -PropCat.6, -PropCat.7, -PropCat.8, -PropCat.9, -PropCat.10, -PropCat.11,
         -PropCat.12, -PropCat.13, -PropCat.14, -PropCat.15)

# Selecting all the variables for basis of purchase
data_basis_of_purchase <- normalized_data %>%
  select(-SEC, -FEH, -MT, -SEX, -AGE, -EDU, -HS, -CHILD, -CS, -Affluence.Index, -No..of.Brands,
         -Brand.Runs, -Total.Volume, -No..of..Trans, -Value, -Trans...Brand.Runs, -Vol.Tran, -Avg..Price,
         -Pur.Vol.No.Promo...., -Pur.Vol.Promo.6.., -Pur.Vol.Other.Promo.., -Br..Cd..57..144, -Br..Cd..55,
         -Br..Cd..272, -Br..Cd..286, -Br..Cd..24, -Br..Cd..481, -Br..Cd..352, -Br..Cd..5, -Others.999)

# Selecting all the variables for purchasing behaviour and basis of purchase
purchase_behaviour_and_basis_of_purchase <- normalized_data %>%
  select(-SEC, -FEH, -MT, -SEX, -AGE, -EDU, -HS, -CHILD, -CS, -Affluence.Index)

# Elbow method for purchase behaviour
fviz_nbclust(data_purchase_behaviour, kmeans, method = "wss") #3
# Elbow method for basis of purchase
fviz_nbclust(data_basis_of_purchase, kmeans, method = "wss") #3
# Elbow method for basis of purchase and purchase behaviour
fviz_nbclust(purchase_behaviour_and_basis_of_purchase, kmeans, method = "wss") #3

# -------------------Cluster 1 (Purchase behaviour)--------------------------
# Perform K-Means clustering for purchase behaviour
set.seed(123)
model_pb <- kmeans(data_purchase_behaviour, centers = 3, nstart = 25)

# View the clustering results
print(model_pb$centers)  # View cluster centers
table(model_pb$cluster) 

# Generating silhouette plot and calculate silhouette width
silhouette <- silhouette(model_pb$cluster, dist(data_purchase_behaviour))
plot(silhouette, col = c(1:length(model_pb$size)), border = NA)

# Cluster visualization
library(factoextra)
fviz_cluster(model_pb, data = data_purchase_behaviour, geom = "point") +
  ggtitle("Cluster Visualization: Purchase Behaviour") +
  theme_minimal()
#Determining Cluster Centers
print(model_pb$centers)

# -------------------Cluster 2 (Basis of purchase)----------------------------
# Perform K-Means clustering for basis of purchase
set.seed(123)
model_bofp <- kmeans(data_basis_of_purchase, centers = 3, nstart = 25)

# View the clustering results
print(model_bofp$centers)  # View cluster centers
table(model_bofp$cluster) 

# Generating silhouette plot and calculate silhouette width
silhtte <- silhouette(model_bofp$cluster, dist(data_basis_of_purchase))
plot(silhtte, col = c(1:length(model_bofp$size)), border = NA)

# Cluster visualization
fviz_cluster(model_bofp, data = data_basis_of_purchase, geom = "point") +
  ggtitle("Cluster Visualization: Basis of Purchase") +
  theme_minimal()
#Determining cluster centers
print(model_bofp$centers)

# -------------------Cluster 3 (Basis of Purchase & Purchase Behaviour)--------
# Perform K-Means clustering for purchase behaviour & basis of purchase
set.seed(123)
model_bofpandbop <- kmeans(purchase_behaviour_and_basis_of_purchase, centers = 3, nstart = 25)

# View the clustering results
print(model_bofpandbop$centers)  # View cluster centers
table(model_bofpandbop$cluster) 

# Generating silhouette plot and calculate silhouette width
silhouette_values <- silhouette(model_bofpandbop$cluster, dist(purchase_behaviour_and_basis_of_purchase))
plot(silhouette_values, col = 1:length(unique(model_bofpandbop$cluster)), border = NA)

# Cluster visualization
fviz_cluster(model_bofpandbop, data = purchase_behaviour_and_basis_of_purchase, geom = "point") +
  ggtitle("Cluster Visualization: Purchase Behaviour & Basis of Purchase") +
  theme_minimal()
#Determining Cluster Centers
print(model_bofpandbop$centers)

# If there is a "Cluster 2 of Purchase behavior" we put 1 otherwise 0
# Because cluster 2 of the purchasing behavior was selected as value consciousness
data$ValueConscious <- ifelse(model_pb$cluster == 2, 1, 0)

# -----------------Predictor Relevancy---------------------------------------
# Fit a random forest model to assess feature importance for ValueConscious (Classification)
set.seed(123)
rf_model_classification <- randomForest(ValueConscious  ~ ., data = data, importance = TRUE)
varImpPlot(rf_model_classification, main = "Random Forest - Feature Importance (Classification)")

# Fit a random forest model to assess feature importance for Brand Runs (Regression)
set.seed(123)
rf_model_regression <- randomForest(Brand.Runs  ~ ., data = data, importance = TRUE)
varImpPlot(rf_model_regression, main = "Random Forest - Feature Importance (Regression)")

# Selecting relevant predictors based on business considerations for classification from Random forest Model
predictors_classification <- data %>%
  select("SEC", "FEH", "MT", "SEX", "AGE", "EDU", "HS", "CHILD", "CS", "Affluence.Index", "ValueConscious")

# Selecting relevant predictors based on business considerations for regression from Random forest Model
predictors_regression <- data %>%
  filter(ValueConscious == 1) %>%
  select(SEC, FEH, MT, SEX, AGE, EDU, HS, CHILD, CS, Affluence.Index, Brand.Runs)

#----------------Correlations (Classification)-----------------------------------
# Correlation for classification predictors 
# Calculating the correlation matrix
correlation_matrix_classification <- cor(predictors_classification, use = "complete.obs")

# Visualizing the correlation matrix using a heat map
corrplot(correlation_matrix_classification, 
         method = "number",   
         type = "lower",      
         tl.col = "black",     
         tl.srt = 45,          
         number.cex = 0.6,     
         number.digits = 2) 

#--------------------Correlations (Regression)--------------------------
# Correlation for regression predictors 
# Calculating the correlation matrix
correlation_matrix_regression <- cor(predictors_regression, use = "complete.obs")

# Visualizing the correlation matrix using a heat map
corrplot(correlation_matrix_regression, 
         method = "number",   
         type = "lower",      
         tl.col = "black",     
         tl.srt = 45,          
         number.cex = 0.6,     
         number.digits = 2) 

##---------------Data Partitioning-----------------------------------
# Classification
# Splitting data into training and testing sets (70% train, 30% test)
set.seed(123)
train_index_class <- createDataPartition(predictors_classification$ValueConscious, p = 0.7, list = FALSE)
train_data_class <- predictors_classification[train_index_class, ]
test_data_class <- predictors_classification[-train_index_class, ]

# Check partition sizes
cat("Training Set Size for Classification:", nrow(train_data_class), "\n")
cat("Testing Set Size for Classification:", nrow(test_data_class), "\n")

# Regression
# Splitting data into training and testing sets (70% train, 30% test)
set.seed(123)
train_index_reg <- createDataPartition(predictors_regression$Brand.Runs, p = 0.7, list = FALSE)
train_data_reg <- predictors_regression[train_index_reg, ]
test_data_reg <- predictors_regression[-train_index_reg, ]

# Check partition sizes
cat("Training Set Size for Regression:", nrow(train_data_reg), "\n")
cat("Testing Set Size for Regression:", nrow(test_data_reg), "\n")

#------------------Classification Modelling---------------------------------
# Logistic Regression
# Building a logistic regression model on the balanced dataset
lr_model <- glm(as.factor(ValueConscious) ~ ., data = train_data_class, family = binomial)
summary(lr_model)
lr_predict <- predict(lr_model, test_data_class, type = "response")

# Converting probabilities to binary outcome (default threshold of 0.5)
predicted_classes <- ifelse(lr_predict > 0.50, 1, 0)
confusionMatrix(factor(predicted_classes, levels = c("1", "0")),
                factor(test_data_class$ValueConscious, levels = c("1", "0")))
# Adjusting threshold for higher sensitivity
optimal_threshold <- 0.4  # Example threshold, tune as needed
predicted_classes <- ifelse(lr_predict > optimal_threshold, 1, 0)

confusionMatrix(factor(predicted_classes, levels = c("1", "0")),
                factor(test_data_class$ValueConscious, levels = c("1", "0")),
                positive = "1")

# Decision Tree (rpart)
ctree_model = rpart(as.factor(ValueConscious) ~ ., data = train_data_class, method = "class")
summary(ctree_model)
rpart.plot(ctree_model)
predicted_classes_tree = predict(ctree_model, newdata = test_data_class, type = "class")
confusionMatrix(factor(predicted_classes_tree, levels = c("1", "0")),
                factor(test_data_class$ValueConscious, levels = c("1", "0")))


# Random Forest
rf_model = randomForest(ValueConscious ~ .,data = train_data_class)
summary(rf_model)
predicted_probs_rf = predict(rf_model, newdata = test_data_class)
predicted_classes = ifelse(predicted_probs_rf > 0.5, 1, 0)
confusionMatrix(factor(predicted_classes, levels = c("1", "0")),
                factor(test_data_class$ValueConscious, levels = c("1", "0")))
# Random Forest to improve performance
rf_model = randomForest(ValueConscious ~ ., data = train_data_class, ntree = 1000, mtry = 3, 
                        classwt = c("0" = 0.4, "1" = 0.6))  # Weight adjustment

predicted_probs_rf = predict(rf_model, newdata = test_data_class)
# Adjusting threshold for sensitivity
rf_optimal_threshold <- 0.3  # Tune for sensitivity
predicted_classes = ifelse(predicted_probs_rf > rf_optimal_threshold, 1, 0)

confusionMatrix(factor(predicted_classes, levels = c("1", "0")),
                factor(test_data_class$ValueConscious, levels = c("1", "0")),
                positive = "1")

#---------------------------Regression--------------------------------------
# Ensuring your target variable 'Brand.Runs' is correctly formatted as numeric
predictors_regression$Brand.Runs = as.numeric(as.character(predictors_regression$Brand.Runs))

###### Multiple Linear Regression
# Fit the linear regression model
model_reg = lm(Brand.Runs ~ ., data = train_data_reg)
# Checking the summary of the model
summary(model_reg)
# Predicting on the test data
predictions_reg = predict(model_reg, newdata = test_data_reg)
# Showing the regression metrics
forecast::accuracy(predictions_reg, test_data_reg$Brand.Runs)
# Showing the correlation between the actual values and the predicted values
cor(predictions_reg, test_data_reg$Brand.Runs)

##### Regression Tree
# Fitting the regression tree model
tree_model = rpart(Brand.Runs ~ ., data = train_data_reg, method = "anova")
# Plotting the regression tree
rpart.plot(tree_model, main = "Regression Tree for Brand.Runs", extra = 1)
# Predicting on the test data using the tree model
predictions_tree = predict(tree_model, newdata = test_data_reg)
# Showing the regression metrics
forecast::accuracy(predictions_tree, test_data_reg$Brand.Runs)
# Showing the correlation between the actual values and the predicted values
cor(predictions_tree, test_data_reg$Brand.Runs)

##### Random Forest
# Fitting Random Forest Regression
rf_model = randomForest(Brand.Runs ~ ., data = train_data_reg, ntree = 500)
print(rf_model)
predictions = predict(rf_model, newdata = test_data_reg)

# Showing the regression metrics
me = mean(predictions - test_data_reg$Brand.Runs)
rmse = sqrt(mean((predictions - test_data_reg$Brand.Runs)^2))
mae = mean(abs(predictions - test_data_reg$Brand.Runs))
mpe = mean((predictions - test_data_reg$Brand.Runs) / test_data_reg$Brand.Runs) * 100
mape = mean(abs((predictions - test_data_reg$Brand.Runs) / test_data_reg$Brand.Runs)) * 100
# Printing the metrics
cat("(ME):", me, "\n")
cat("(RMSE):", rmse, "\n")
cat("(MAE):", mae, "\n")
cat("(MPE):", mpe, "\n")
cat("(MAPE):", mape, "\n")
cor(predictions, test_data_reg$Brand.Runs)


# Function to extract performance metrics from confusion matrix
calculate_classification_metrics <- function(true_labels, predicted_labels, positive_class = "1") {
  cm <- confusionMatrix(factor(predicted_labels, levels = c("1", "0")),
                        factor(true_labels, levels = c("1", "0")),
                        positive = positive_class)
  
  metrics <- data.frame(
    Model = NA,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    F1_Score = cm$byClass["F1"]
  )
  return(metrics)
}

# Logistic Regression Metrics
metrics_lr <- calculate_classification_metrics(test_data_class$ValueConscious, predicted_classes)
metrics_lr$Model <- "Logistic Regression"

# Decision Tree Metrics
metrics_ctree <- calculate_classification_metrics(test_data_class$ValueConscious, predicted_classes_tree)
metrics_ctree$Model <- "Decision Tree"

# Random Forest Metrics
metrics_rf <- calculate_classification_metrics(test_data_class$ValueConscious, predicted_classes)
metrics_rf$Model <- "Random Forest"

# Combine Metrics for All Models
classification_metrics <- rbind(metrics_lr, metrics_ctree, metrics_rf)

# Identify the Best Model
best_classification_model <- classification_metrics[which.max(classification_metrics$F1_Score), ]
print("Classification Model Metrics:")
print(classification_metrics)
print("Best Classification Model:")
print(best_classification_model)

# Function to extract regression metrics
calculate_regression_metrics <- function(actual, predicted) {
  me <- mean(predicted - actual)
  rmse <- sqrt(mean((predicted - actual)^2))
  mae <- mean(abs(predicted - actual))
  mape <- mean(abs((predicted - actual) / actual)) * 100
  correlation <- cor(actual, predicted)
  
  metrics <- data.frame(
    Model = NA,
    ME = me,
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    Correlation = correlation
  )
  return(metrics)
}

# Linear Regression Metrics
metrics_lm <- calculate_regression_metrics(test_data_reg$Brand.Runs, predictions_reg)
metrics_lm$Model <- "Linear Regression"

# Regression Tree Metrics
metrics_tree <- calculate_regression_metrics(test_data_reg$Brand.Runs, predictions_tree)
metrics_tree$Model <- "Regression Tree"

# Random Forest Metrics
metrics_rf <- calculate_regression_metrics(test_data_reg$Brand.Runs, predictions)
metrics_rf$Model <- "Random Forest"

# Combine Metrics for All Models
regression_metrics <- rbind(metrics_lm, metrics_tree, metrics_rf)

# Identify the Best Model
best_regression_model <- regression_metrics[which.min(regression_metrics$RMSE), ]
print("Regression Model Metrics:")
print(regression_metrics)
print("Best Regression Model:")
print(best_regression_model)

