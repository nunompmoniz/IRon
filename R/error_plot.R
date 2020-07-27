# ======================================================================
#' Visualization of comparison between prediction error and item's relevance
#'
#' @description The errorPlot function takes a set of target and predicted values to calculate the deviation as a function of phi and creates a plot with points and an approximate function err(phi).
#'
#' @param trues True target values of a given test set
#' @param preds Predicted values for the test set
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known (use ?phi.control() for more information)
#' @param e Exponent for distance - 1 is linear (mae) and 2 is quadratic (mse)
#' @param thr Relevance threshold (default 0)
#'
#' @return Produces a plot with a visualization comparing prediction error and item's relevance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(IRon)
#' require(earth)
#' require(mgcv)
#'
#' data(accel)
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' ph <- phi.control(accel$acceleration)
#'
#' m <- earth::earth(acceleration ~ ., train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- accel$acceleration
#'
#' errorPlot(trues,preds,phi.parms = ph)
#' }
#'
#'
errorPlot <- function(trues, preds, phi.parms, e=1, thr=0) {

  if(is.null(phi.parms)) {
    stop("A relevance function is required: Use ?phi.control()")
  }

  phis <- phi(trues, phi.parms)
  err <- abs(trues-preds)^e
  df <- data.frame(err=err,phi=phis)

  ggplot2::ggplot(df[df$phi>=thr,],aes(x=phi,y=err)) + ggplot2::geom_point() + ggplot2::geom_smooth()

}

# ======================================================================
#' 3d Visualization of comparison between true value, prediction error and item's relevance
#'
#' @description This function produces a 3d plot describing the prediction error of a given set pair of true and predicted values. The objective is to allow a 3-way comparison between the true values of a given data set, the predicted value of a certain model, and the relevance of each true value.
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known (use ?phi.control() for more information)
#' @param modelname The name attributed to the prediction model used for caption of the plot - empty by default
#' @param e Exponent for distance - 1 is linear (mae) and 2 is quadratic (mse)
#' @param thr Relevance threshold (default 0)
#' @param absolute Boolean for calculating absolute distance or not (default yes)
#' @param errlim Definition of the error limits for the plot (optional)
#'
#' @return Produces a 3D plot with a visualization comparing prediction error, target value and item's relevance
#' @export
#'
#' @examples
#' \dontrun{
#' library(IRon)
#' require(rpart)
#' require(plot3D)
#'
#' data(accel)
#' form <- acceleration ~ .
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' # In the case of a single plot this is a simple solution
#'
#' phi.parms <- phi.control(accel$acceleration)
#' trues <- accel$acceleration
#'
#' m1 <- rpart::rpart(form,train)
#' p1 <- predict(m1,test)
#'
#' errorPlot3D(trues,p1,phi.parms)
#' errorPlot3D(trues,p1,phi.parms,modelname="Regression Trees")
#' errorPlot3D(trues,p1,phi.parms,modelname="Regression Trees",errlim=c(0,10))
#' errorPlot3D(trues,p1,phi.parms,modelname="Regression Trees",absolute=FALSE)
#'
#' #Example for multiple plots w.r.t. various underlying prediction models
#'
#' require(randomForest)
#' require(e1071)
#' require(earth)
#'
#' m2 <- randomForest::randomForest(form,train)
#' p2 <- predict(m2,test)
#'
#' m3 <- e1071::svm(form,train)
#' p3 <- predict(m3,test)
#'
#' m4 <- earth::earth(form,train)
#' p4 <- as.vector(predict(m4,test))
#'
#' par(mfrow = c(2, 2),     # 2x2 layout
#' oma = c(2, 2, 1, 0), # two rows of text at the outer left and bottom margin
#' mar = c(2, 2, 0, 0), # space for one row of text at ticks and to separate plots
#' mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
#' xpd = NA)            # allow content to protrude into outer margin (and beyond)
#'
#' errorPlot3D(trues,p1,phi.parms,modelname="Regression Trees")
#' errorPlot3D(trues,p2,phi.parms,modelname="Random Forests")
#' errorPlot3D(trues,p3,phi.parms,modelname="SVM")
#' errorPlot3D(trues,p4,phi.parms,modelname="MARS")
#' }
errorPlot3D <- function(trues, preds, phi.parms, modelname=NULL, e=1, thr=0, absolute=TRUE, errlim=NULL) {

  if(is.null(phi.parms)) stop("A relevance function is required. Use ?phi.control()")
  if(!is.vector(trues)) stop("Parameter trues is required to be a vector.")
  if(!is.vector(preds)) stop("Parameter preds is required to be a vector.")

  err <- if(absolute) { abs(trues-preds) } else { trues-preds }
  phis <- phi(trues,phi.parms = phi.parms)

  elim <- if(is.null(errlim)) {
    if(absolute) {
      c(0,ceiling(max(err)))
    } else {
      c(floor(min(err)),ceiling(max(err)))
    }
  } else { errlim }

  mname <- ifelse(is.null(modelname),"",modelname)

  plot3D::scatter3D(phis,trues,err,
    cex=1.5,bty="b2",pch=16,
    colkey = FALSE, col = plot3D::ramp.col(c("grey", "red")),
    ticktype="detailed",phi=18,type="h",zlim=elim,
    xlab=expression(phi), ylab="Y",zlab="Error",expand=0.5,main=mname);

}
