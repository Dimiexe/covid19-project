# ----------------- FUNCTIONS ----------------------
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# ------------------ SCRIPT -----------------------

# Load the dataset
data <- read.csv("../../datasets/Symptoms_Covid.csv")

# Data cleaning
## Check if there are NAs
sum(is.na(data)) # no NAs

count <- 0
for(i in 1:dim(data)[1]) {
  for (j in 1:dim(data)[2]) {
    if (data[i,j] != 'Yes' && data[i,j] != 'No'){
      count <- count + 1
    }
  }
}
print(count) # count=0, so there aren't any invalid values in our dataset

# Exploring the data
str(data)
## Check how many samples are in each class
sum(data$COVID.19=='Yes')/(sum(data$COVID.19=='Yes')+sum(data$COVID.19=='No'))
# 80% of our samples belong to the positive class and the rest 20% to the negative class.
# This means that there is dataset imbalance, but a mild one, so we'll start the process of choosing a classification model without trying to reduce imbalance
# and later we will also try to apply techniques either to reduce the imbalance or to take it into account when evaluating the model

## In order to predict if someone is positive to Covid-19, we need to know his symptoms and how exposed to positive cases he was in the last days/weeks.
## We don't need to know if he has a chronic disease; this would be useful if we wanted to predict how possible is that this person will eventually die.
## So we drop the columns that have to do with chronic diseases and we'll use the rest ones.
## We also drop the columns Wearing.Masks and Sanitization.from.Market because all their rows contain the same value, so they have no predictive ability.
data_cleaned <- data[-c(6:7, 9:11, 19:20)]

# Separate the independent variables from the dependent one, the one we want to predict (COVID.19)
data_cleaned_x <- data_cleaned[-c(14)] # 13 variables
data_cleaned_y <- data_cleaned[c(14)] # 1 variable

# Exploring and analyzing the relationships between variables
## Compute the similarity between each of our features (independent variables) and y
data_cleaned_TF <- data_cleaned=='Yes'
similarity_matrix = 1-dist(t(data_cleaned_TF), method="binary")
similarity_matrix_df <- as.data.frame(as.matrix(similarity_matrix))
write.csv(similarity_matrix_df, 'SimilarityMatrix.csv')
## This matrix shows that there is a strong similarity (0.7-0.8) between COVID-19 and the symptoms of Breathing.Problem, Fever, Dry.Cough and Sore.throat.
## Running nose, headache, fatigue, gastrointestinal and family working in public exposed places don't play an important role, while
## travelling abroad, contact with covid patient, attended large gathering and visited public exposed places play a slightly important role.
## The conclusions from the similarity matrix are on par with the experience we have with covid so far.

# Preparing the data in order to train and evaluate predictive models
## Shuffle the data and split them in train, validation, test sets
## TODO: now I've split only to train, test -> add also validation if I want to fine-tune hyperparameters
shuffle_index <- sample(1:nrow(data_cleaned))
data_shuffled <- data_cleaned[shuffle_index, ]
data_shuffled_train <- create_train_test(data_shuffled, size = 0.8, train = TRUE)
data_shuffled_test <- create_train_test(data_shuffled, size = 0.8, train = FALSE)

# Finding a proper model to predict if a patient has Covid-19 or not
# Try:
# Decision Trees
## Install the required packages and import them
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
## Train the model based on the training set
fit <- rpart(COVID.19~., data = data_shuffled_train, method = 'class')
## Plot the tree and the probability of each class at each split
rpart.plot(fit, extra= 106) 
## Make predictions for the data in the test set, using the trained model
predict_test <-predict(fit, data_shuffled_test, type = 'class')
## Create a confusion matrix to evaluate the classification
conf_matrix <- table(data_shuffled_test$COVID.19, predict_test)
## Use confusion matrix to calculate four different metrics in order to evaluate the classification
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) # (TP+TN)/(TP+TN+FP+FN)
precision <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[1,2]) # TP/(TP+FP)
recall <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[2,1]) # TP/(TP+FN)
f_score <- 2*precision*recall/(precision+recall) # 2*precision*recall/(precision+recall)
## All of the above four metrics have values of above 95%, which shows that the model is a very good classifier, especially because it has an F-score of 97.6%, which is the most robust metric among those four.
## At this point we could fine-tune the hyperparameters of the decision tree, in order to further improve its performance. However the performance of this models is so good, that I don't think that a hyperparameter-tuning would make a big difference.
## Later on, I will try different models, and after finding the best one, I will fine-tune it.

# TODO: Random Forest
# Linear SVC (SVMs in general)

# TODO: Later, I can try to deal with dataset imbalance, although it's a mild one in this case
## Deal with bias between the two classes (class 'Yes' contains much more sample than class 'No')








