Data cleaning Assignment
===================

This code requires the reshape2 package (mandatory)

code verifies the availability of the directories and stops the process if not available

loads the activites in to a datatable and laods the features

loads all subject, activities and X-data for both the test and train dataset

rbind is used to merge the both the test and train datasets

writes the files with the subset of the mean and std variables

uses the cbind and merge to merge the datasets of subject, activities

melt and dcast is used to identify the means at subject and activity levels
