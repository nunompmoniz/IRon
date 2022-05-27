#'
#' Standard Evaluation Metrics
#'

#' Mean Average Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for the mean average error
#'
mae <- function(trues,preds) {
  mean(abs(trues-preds))
}

#' Mean Squared Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for the mean squared error
#'
mse <- function(trues,preds) {
  mean((trues-preds)^2)
}

#' Root Mean Squared Error
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for the relevance-weighted root mean squared error
#'
rmse <- function(trues,preds) {
  sqrt(mean((trues-preds)^2))
}

#' Pearson's Correlation
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for the Pearson's correlation
#'
corr <- function(trues,preds) {
  stats::cor(trues,preds)
}

#' Model Bias
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for model bias
#'
bias <- function(trues,preds) {
  mean(preds - trues)^2
}

#' Model Variance
#'
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable trues
#'
#' @keywords internal
#'
#' @return Value for model variance
#'
variance <- function(preds) {
  mean( ( preds - mean(preds) )^2 )
}


