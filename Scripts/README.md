The script run_analysis contains a function of the same name. This function downloads the accelerometer data produced by Human Activity Recognition Using Smartphones Dataset Version 1.0 from :  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The data is unzipped and extracted from a series of large text files. Two datasets are included, one set that had been used as a training set by Jorge et. al, and another which was collected as experimentation. Both datasets are combined by this script.

After collecting all the data in a tibble, all data not pertaining to the mean or standard deviation of each variable was excluded. A detailed description of the data can be found in the file codebook.md.

Next, the data within the activity column was modified to contain the names of the activites instead of  a numeric code. The activity names were taken from the file activity_labels.txt contained within the original dataset.

After cleaning up the names of each of the column headers, a new tibble is created containing the average of each of the aformentioned variables for both participant and activity. The output from which can be found in the file final_data.txt
