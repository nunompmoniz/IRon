#'
#' Standard Evaluation Metrics
#'

#' Mean Average Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for the mean average error
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' mae(trues,preds)
mae <- function(trues,preds) {
  mean(abs(trues-preds))
}

#' Mean Squared Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for the mean squared error
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' mse(trues,preds)
mse <- function(trues,preds) {
  mean((trues-preds)^2)
}

#' Root Mean Squared Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for the relevance-weighted root mean squared error
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' rmse(trues,preds)
rmse <- function(trues,preds) {
  sqrt(mean((trues-preds)^2))
}

#' Pearson's Correlation
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for the Pearson's correlation
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' corr(trues,preds)
corr <- function(trues,preds) {
  stats::cor(trues,preds)
}



#' Model Bias
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for model bias
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' bias(trues,preds)
bias <- function(trues,preds) {
  mean(preds - trues)^2
}

#' Model Variance
#'
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @export
#'
#' @return Value for model variance
#'
#' @examples
#' library(IRon)
#' library(earth)
#'
#' data(accel)
#'
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' m <- earth::earth(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#'
#' variance(preds)
variance <- function(preds) {
  mean( ( preds - mean(preds) )^2 )
}


