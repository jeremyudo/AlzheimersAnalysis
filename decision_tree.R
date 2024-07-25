install.packages("tree")
# load necessary library
library(tree) 

# load and inspect the dataset
load_and_inspect_data <- function(file_path) {
  dataset <- read.csv(file_path)
  str(dataset)
  summary(dataset)
  return(dataset)
}

# convert columns to factors
preprocess_data <- function(dataset) {
  dataset <- na.omit(dataset)  # remove rows with any naN values
  dataset$Gender <- as.factor(dataset$Gender)
  dataset$Ethnicity <- as.factor(dataset$Ethnicity)
  dataset$EducationLevel <- as.factor(dataset$EducationLevel)
  dataset$Smoking <- as.factor(dataset$Smoking)
  dataset$FamilyHistoryAlzheimers <- as.factor(dataset$FamilyHistoryAlzheimers)
  dataset$CardiovascularDisease <- as.factor(dataset$CardiovascularDisease)
  dataset$Diabetes <- as.factor(dataset$Diabetes)
  dataset$Depression <- as.factor(dataset$Depression)
  dataset$HeadInjury <- as.factor(dataset$HeadInjury)
  dataset$Hypertension <- as.factor(dataset$Hypertension)
  dataset$MemoryComplaints <- as.factor(dataset$MemoryComplaints)
  dataset$BehavioralProblems <- as.factor(dataset$BehavioralProblems)
  dataset$Confusion <- as.factor(dataset$Confusion)
  dataset$Disorientation <- as.factor(dataset$Disorientation)
  dataset$PersonalityChanges <- as.factor(dataset$PersonalityChanges)
  dataset$DifficultyCompletingTasks <- as.factor(dataset$DifficultyCompletingTasks)
  dataset$Forgetfulness <- as.factor(dataset$Forgetfulness)
  dataset$Diagnosis <- as.factor(dataset$Diagnosis)
  return(dataset)
}

# split the data into training and test sets
split_data <- function(dataset, split_ratio = 0.7, seed = 1) {
  set.seed(seed)
  sample_index <- sample(seq_len(nrow(dataset)), size = split_ratio * nrow(dataset))  # create a random sample of row indices
  train_data <- dataset[sample_index, ]  # assign 70% of the data to the training set
  test_data <- dataset[-sample_index, ]  # assign the remaining 30% of the data to the test set
  return(list(train = train_data, test = test_data))
}

# build decision tree model
build_decision_tree <- function(train_data) {
  decision_tree <- tree(Diagnosis ~ ., data = train_data)  # build d_tree using training data
  return(decision_tree)
}

# visualize decision tree
visualize_tree <- function(decision_tree) {
  plot(decision_tree)  # plot decision tree
  text(decision_tree, pretty = 0)  # add labels
}

# evaluate the model
evaluate_model <- function(decision_tree, test_data) {
  predictions <- predict(decision_tree, test_data, type = "class")  # predict the diagnosis on the test data
  confusion_matrix <- table(test_data$Diagnosis, predictions)  # create a confusion matrix
  print(confusion_matrix)  # print the confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)  # calculate the accuracy of the model
  print(paste("accuracy:", accuracy))  # print the accuracy of the model
}

main <- function() {
  dataset <- load_and_inspect_data("Downloads/alzheimers_disease_data.csv")
  dataset <- preprocess_data(dataset)
  data_splits <- split_data(dataset)
  train_data <- data_splits$train
  test_data <- data_splits$test
  decision_tree <- build_decision_tree(train_data)
  visualize_tree(decision_tree)
  evaluate_model(decision_tree, test_data)
}

main()
