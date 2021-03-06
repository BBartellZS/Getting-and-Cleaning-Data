Getting-and-Cleaning-Data Course Project
=========================
* This repo contains my script, run_analysis.R, which takes the data for the course project and produces a tidy data set.
* It also includes a code book in CodeBook.md that describes how the code is executed and what all of the variable names are.


Procedure
-------------------------
* Read in data from training set, test set, features, and activity labels
* use rbind() to merge training and test set
* use cbind() to add columns for subject id and activity description
* use activity descriptions to replace the activity number code
* use grepl() to find data columns related to mean and standard deviation
* remove data columns related to meanFreq
* use aggregate() to take the mean of the desired columns grouped by subject and activity type  

Repo Contents
-------------------------
* README.md - this file
* CodeBook.md - codebook describing tidydata.txt, the variables it contains and their meanings
* run_analysis.R - the reproducible code that created tidydata.txt
* tidydata.txt and tidydata.csv - the product of run_analysis.R in two different formats
