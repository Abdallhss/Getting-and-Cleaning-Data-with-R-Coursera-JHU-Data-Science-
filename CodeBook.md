#Feature Selection 
=================
### P.S: this is a copy of the original features_info file with a minor modification to include on mean and std variables.

>> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz.
Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise.
Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

>> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ).
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

>> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

- tBodyAcc-XYZ

- tGravityAcc-XYZ

- tBodyAccJerk-XYZ

- tBodyGyro-XYZ

- tBodyGyroJerk-XYZ

- tBodyAccMag

- tGravityAccMag

- tBodyAccJerkMag

- tBodyGyroMag

- tBodyGyroJerkMag

- fBodyAcc-XYZ

- fBodyAccJerk-XYZ

- fBodyGyro-XYZ

- fBodyAccMag

- fBodyAccJerkMag

- fBodyGyroMag

- fBodyGyroJerkMag


The set of variables that were estimated from these signals are: 

- **mean()**: Mean value

- **std()**: Standard deviation

>> The complete list of variables of each feature vector is available in 'features.txt'

>> The units are all in SI format.


# Code Analysis

1) We read in the features_names and the activity_labels through read.csv with space seperator.
2) We create a function that reads in the features (X) and labels(y) as well as the subjects (subject) and then bind them using "cbind" after replacing column names if needed. (The original features names were kept as they are descriptive enough.)
3) The train and test data are generated and combined using rbind
4) Using the "grep" function, the mean and std columns were kept and other features were removed ([^F] was used to removed meanFrequency columns).
5) A pipe line is created using 'dplyr' library to group the data by subject and activity then summarized by features mean.
6) After confirming the tidiness of the data, it was saved to a txt format using "write.table". 
