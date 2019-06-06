Create a data set.
Build 100 linear models using the data above and calculate the mean and standard deviation of the combined models. 
Partition the dataset into test and training sets of equal size to generate your indices.
Train a linear model predicting y from x.
Generate predictions on the test set.
Calculate the RMSE of that model. 
Then, report the mean and standard deviation of the RMSEs from all 100 models.


Now, use larger datasets. 
n <- c(100, 500, 1000, 5000, 10000).
Write a function that takes a size n.
Build a dataset with n observations.
Build 100 linear models to get a vector of RMSEs.
Calculates the mean and standard deviation. 
