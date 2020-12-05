library(tidyverse)

# A function which downloads the dataset found here: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# Loads the data into R and stores it within a tibble. It then returns a tibble
# with all the variables averaged by "subject" and "activity".
run_analysis <- function(){
  # download and unzip data
  download_data()
  # Create tibble containing all data
  full_data_set <- create_full_data_set()
  # replace the numbers in the activity column with activity names
  full_data_set <- change_activity_names(full_data_set)
  # select all columns pertaining to the mean and std
  select_data_set <- select_mean_and_std(full_data_set)
  # clean up the column names
  select_data_set <- clean_column_names(select_data_set)
  # average each of the variables by "subject" and "activity
  final_data_set <- create_final_data(select_data_set)
  final_data_set
}

# A function to extract all the data from the files X_test.txt, X_train.txt, 
# subject_test.txt, subject_train.txt, y_test.txt, y_train.txt, and features.txt
# and combines all that data in a tibble
create_full_data_set <- function(){
  test_feature_data <- get_feature_data("data/UCI HAR Dataset/test/X_test.txt")
  test_feature_table <- create_feature_table(test_feature_data)
  test_feature_table <- add_column_from_file(file_path = "data/UCI HAR Dataset/test/y_test.txt",
                                            "activity", test_feature_table)
  test_feature_table <- add_column_from_file("data/UCI HAR Dataset/test/subject_test.txt",
                                             "subject", test_feature_table)
  test_feature_table <- add_column(test_feature_table, sample = "test")
  
  train_feature_data <- get_feature_data("data/UCI HAR Dataset/train/X_train.txt")
  train_feature_table <- create_feature_table(train_feature_data)
  train_feature_table <- add_column_from_file("data/UCI HAR Dataset/train/y_train.txt",
                                              "activity", train_feature_table)
  train_feature_table <- add_column_from_file("data/UCI HAR Dataset/train/subject_train.txt",
                                              "subject", train_feature_table)
  train_feature_table <- add_column(train_feature_table, sample = "train")
  
  combined_table <- bind_rows(test_feature_table, train_feature_table)
  combined_table
  
}

# A function to download the data and unzip it
download_data <- function(){
# download and unzip files
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              "./samsung_data.zip")
unzip("samsung_data.zip", overwrite = FALSE, exdir = "./data")
}

# A function which reads the file X_test.txt or X_train.txt and extracts the data,
# converting it to a 2 level nested list of numerics, and returning this list
get_feature_data <- function(path){
# read in test data
feature_data <- scan(file = path, what = "character",
                  sep = "\n")
#trim the white space from the ends of each data set in the list
feature_data <- lapply(feature_data, str_trim, side = "both")
# Currently each set of results (561 variables) exists as one long string
# Split the string into 561 seperate strings
feature_data <- lapply(feature_data, str_split, pattern = "[ ]+")
# convert each of the strings to a numeric
feature_data <- lapply(feature_data, lapply, as.numeric)
feature_data
}

# A function to extract column names from the file features.txt
get_feature_names <- function(){
  feature_names <- scan(file = "data/UCI HAR Dataset/features.txt", what = "character",
                        sep = "\n")
}

# A function which takes as input the processed feature data from the files X_test.txt
# or X_train.txt, and outputs a tibble containing that data, and with column names
# extracted from the file features.txt
create_feature_table <- function(feature_data){
  # unlist the second level lists within the feature data to facilitate copying
  # them into a matrix
  for(i in seq_along(feature_data)){feature_data[i] <- lapply(feature_data[i], unlist)}
  # create a matrix with the dimensions of the feature data set
  matrix <- matrix(nrow = length(feature_data), ncol = length(feature_data[[1]]))
  # fill the matrix with the data from the feature data set
  for(i in seq_along(feature_data)){
    for(j in seq_along(feature_data[[i]])){matrix[i,j] <- feature_data[[i]][j]}
  }
  # get feature names from the file features.txt
  feature_names <- get_feature_names()
  # convert matrix to a tibble
  feature_table <- as_tibble(matrix)
  # give tibble column names
  colnames(feature_table) <- feature_names
  
  feature_table
}
# A function which, when given a file path to a suitable text document, a column
# name, and a table, will return the table it was provided with, with an appended
# column with the provided name, and containing the data from the text file.
add_column_from_file <- function(file_path, column_name, table){
  data <- scan(file = file_path, what = "character", sep = "\n")
  data <- as_tibble(data)
  colnames(data) <- column_name
  new_table <- as_tibble(cbind(data, table))
  new_table
}

# A function which takes a table and replaces the numbers 1-6 in a column
# named "activity with named activities. It then turns the activity column
# into a factor before returning the edited table. Activity names taken from
# the file activity_labels.txt
change_activity_names <- function(table){
  table <- table %>% mutate(activity = gsub("1", "walking", activity)) %>%
  mutate(activity = gsub("2", "walking_upstairs", activity)) %>%
  mutate(activity = gsub("3", "walking_downstairs", activity)) %>%
  mutate(activity = gsub("4", "sitting", activity)) %>%
  mutate(activity = gsub("5", "standing", activity)) %>%
  mutate(activity = gsub("6", "laying", activity)) %>%
  mutate(activity = as.factor(activity))
  table
}
# A function which takes a table and returns it with the selected columns. The 
# columns selected for are "subject", "activity", all pertaining to the mean (but
# only mean(), not meanFreq()), and those pertaining to the std
select_mean_and_std <- function(table){
  column_names_to_select <- c("subject", "activity")
  # select all column names that take the mean. "mean()" is searched for to avoid 
  # returning meanFreq() columns. fixed = TRUE so that () are read as characters
  column_names_to_select <- append(column_names_to_select, 
                                   names(table)[grepl("mean()", names(table), fixed = TRUE)])
  # select all column names pertaining to std
  column_names_to_select <- append(column_names_to_select,
                                   names(table)[grepl("std", names(table))])
  # select the columns with the chosen names
  table <- select(table, all_of(column_names_to_select))
  table
}
# A function which cleans the column names by removing leading numbers, abreviating
# mean() and std() to mean and std, and changing "-" to "_"
clean_column_names <- function(table){
  colnames(table) <- gsub("[0-9]+ ", "", colnames(table))
  # fixed = TRUE so that "()" are read as characters
  colnames(table) <- gsub("()", "", colnames(table), fixed = TRUE)
  colnames(table) <- gsub("-", "_", colnames(table))
  table
}

# A function that takes a tibble, groups that tibble by activity and subject,
# and then returns a tibble of these groups with all other columns averaged
create_final_data <- function(table){
  table <- table %>% mutate(subject = as.factor(subject)) %>% group_by(activity, subject)
  final_table <- summarise_at(table, colnames(table)[3:length(colnames(table))], mean)
  
  final_table
}