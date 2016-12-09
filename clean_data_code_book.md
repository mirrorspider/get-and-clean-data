# Clean Data Code Book

## Original Data Set
The original data is taken from the "Human Activity Recognition Using Smartphones Dataset" (Version 1.0) from
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio & Luca Oneto at Smartlab - Non Linear Complex Systems Laboratory. Made available at [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

The information in this file builds on the data found in the README.txt and features_info.txt in the HAR archive.

## Transformation
To produce the current data set, the following steps were taken:
* In each of the 2 data sets (test and train) a subset of the features information (\<set\>/X_\<set\>.txt) was taken including only features recording mean or standard deviation data.
* The names of the features were tidied to lowercase and to remove "."" characters
* The feature subset was combined with the corresponding activity (\<set\>/y_\<set\>.txt) and subject (\<set\>/subject_\<test\>.txt) information.
* The activity numeric values 1-6 were replaced with natural langauge labels in line with the information in activity_labels.txt
1. walking
2. walkingupstairs
3. walkingdownstairs
4. sitting
5. standing
6. laying
* The two data sets were combined and written to output/uci_har_mean_std.txt (and also stored in the data.frame variable combined.har.data)
* A summary of the data was produced with the mean for each feature grouped by activity and subject recorded. This is written to the output/summary_uci_har.txt file (and stored in the data.frame variable summary.har.data)


## Files
---
title: "Clean Data Code Book"
author: "Alex Robinson"
date: "9 December 2016"
---