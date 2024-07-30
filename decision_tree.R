install.packages("tree")

library(tree) 

# load and inspect the dataset
load_and_inspect_data <- function(file_path) {
  dataset <- read.csv(file_path)  # load the dataset
  str(dataset)  # display the structure of the dataset
  summary(dataset)  # provide summary statistics of the dataset
  return(dataset)  # return the loaded dataset
}

# convert columns to factors
preprocess_data <- function(dataset) {
  dataset <- na.omit(dataset)  # remove rows with any NA values
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
  return(dataset)  # return the preprocessed dataset
}

# split the data into training and test sets
split_data <- function(dataset, split_ratio = 0.8, seed = 1) {
  set.seed(seed)  # set the seed for reproducibility
  sample_index <- sample(seq_len(nrow(dataset)), size = split_ratio * nrow(dataset))  # create a random sample of row indices
  train_data <- dataset[sample_index, ]  # assign 80% of the data to the training set
  test_data <- dataset[-sample_index, ]  # assign the remaining 20% of the data to the test set
  return(list(train = train_data, test = test_data))  # return a list containing the training and test sets
}

# build decision tree model
build_decision_tree <- function(train_data) {
  decision_tree <- tree(Diagnosis ~ ., data = train_data)  # build decision tree using training data
  return(decision_tree)  # return the decision tree model
}

# prune the decision tree model
prune_decision_tree <- function(decision_tree) {
  cv_tree <- cv.tree(decision_tree, FUN = prune.misclass)  # perform cross-validation
  pruned_tree <- prune.misclass(decision_tree, best = cv_tree$size[which.min(cv_tree$dev)])  # prune the tree
  return(pruned_tree)  # return the pruned tree
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

# main function to run the entire process
main <- function() {
  dataset <- load_and_inspect_data("Downloads/alzheimers_disease_data.csv")  
  dataset <- preprocess_data(dataset)  # preprocess the dataset
  data_splits <- split_data(dataset)  # split the dataset into training and test sets
  train_data <- data_splits$train  # assign the training set
  test_data <- data_splits$test  # assign the test set
  decision_tree <- build_decision_tree(train_data)  # build the decision tree model
  pruned_tree <- prune_decision_tree(decision_tree)  # prune the decision tree model
  visualize_tree(pruned_tree)  # visualize the pruned decision tree
  evaluate_model(pruned_tree, test_data)  # evaluate the pruned model
}

main()
