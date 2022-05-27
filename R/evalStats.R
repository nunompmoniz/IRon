#' Predictive Modelling Evaluation Statistics
#'
#' @description Evaluation statistics including standard and non-standard evaluation metrics. Returns a structure of data containing the results of several evaluation metrics (both standard and some focused on the imbalanced regression problem).
#'
#' @param formula A model formula
#' @param train A data.frame object with the training data
#' @param test A data.frame object with the test set
#' @param y_pred A vector with the predictions of a given model
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known (use ?phi.control() for more information). If this parameter is not defined, this method will create a relevance function based on the data.frame variable in parameter train. Default is NULL
#' @param cf The coefficient used to calculate the boxplot whiskers in the event that a relevance function is not provided (parameter phi.parms)
#'
#' @return A list with four slots for the results of standard and relevance-based evaluation metrics
#' \item{overall}{Results for standard metrics MAE, MSE and RMSE, along with Pearson's Correlation, bias, variance and the Squared Error Relevance Area metric.}
#'
#' @export
#'
#' @examples
#' library(IRon)
#'
#' if(requireNamespace("earth")) {
#'
#'    data(accel)
#'
#'    form <- acceleration ~ .
#'
#'    ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#'    train <- accel[ind,]
#'    test <- accel[-ind,]
#'
#'    ph <- phi.control(accel$acceleration)
#'
#'    m <- earth::earth(form, train)
#'    preds <- as.vector(predict(m,test))
#'
#'    eval.stats(form, train, test, preds)
#'    eval.stats(form, train, test, preds, ph)
#'    eval.stats(form, train, test, preds, ph, cf=3) # Focusing on extreme outliers
#'
#' }
#'
#'
eval.stats <- function(formula, train, test, y_pred, phi.parms=NULL,cf=1.5) {

  y_train <- train[,which(colnames(train)==formula[[2]])]
  y_test <- test[,which(colnames(test)==formula[[2]])]

  phi.parms <- if(is.null(phi.parms)) { phi.control(y_train,coef=cf) } else { phi.parms }
  phi.trues <- phi(y_test,phi.parms)

  results <- list()

  results[["overall"]] <- c(mae=mae(y_test,y_pred),
    mse=mse(y_test,y_pred),
    rmse=rmse(y_test,y_pred),
    corr=corr(y_test,y_pred),
    bias=as.numeric(bias(y_test,y_pred)),
    variance=as.numeric(variance(y_pred)),
    sera=as.numeric(sera(y_test,y_pred,phi.trues)))

  results

}


