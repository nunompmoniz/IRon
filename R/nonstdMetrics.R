#'
#' Non-Standard Evaluation Metrics
#'

#' Squared Error-Relevance Metric (SER)
#'
#' @description Obtains the squared error of predictions for a given subset of relevance
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param phi.trues Relevance of the values in the parameter trues. Use ??phi() for more information. Defaults to NULL
#' @param ph The relevance function providing the data points where the pairs of values-relevance are known. Default is NULL
#' @param t Relevance cut-off. Default is 0.
#'
#' @export
#'
#' @return Squared error for for cases where the relevance of the true value is greater than t (SERA)
#'
#' @examples
#' \dontrun{
#' library(IRon)
#' library(rpart)
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
#' ph <- phi.control(accel$acceleration)
#'
#' m <- rpart::rpart(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#' phi.trues <- phi(test$acceleration,ph)
#'
#' ser(trues,preds,phi.trues)
#' }
ser <- function(trues, preds, phi.trues=NULL, ph=NULL, t=0) {

  if(is.null(phi.trues) && is.null(ph)) stop("You need to input either the parameter phi.trues or ph.")

  if(is.null(phi.trues)) phi.trues <- phi(trues,ph)

  error <- (trues[phi.trues>=t] - preds[phi.trues>=t])^2
  if(any(is.na(error))) error[is.na(error)] <- 0

  sum(error)

}

#' Squared Error-Relevance Area (SERA)
#'
#' @description Computes an approximation of the area under the curve described by squared error of predictions for a sequence of subsets with increasing relevance
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param phi.trues Relevance of the values in the parameter trues. Use ??phi() for more information. Defaults to NULL
#' @param ph The relevance function providing the data points where the pairs of values-relevance are known. Default is NULL
#' @param pl Boolean to indicate if an illustration of the curve should be provided. Default is FALSE
#' @param m.name Name of the model to be appended in the plot title
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001
#' @param return.err Boolean to indicate if the errors at each subset of increasing relevance should be returned. Default is FALSE
#' @param norm Normalize the SERA values for internal optimisation only (TRUE/FALSE)
#'
#' @export
#'
#' @return Value for the area under the relevance-squared error curve (SERA)
#'
#' @examples
#' \dontrun{
#' library(IRon)
#' library(rpart)
#' library(ggplot2)
#' library(scam)
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
#' ph <- phi.control(accel$acceleration)
#'
#' m <- rpart::rpart(form, train)
#' preds <- as.vector(predict(m,test))
#'
#' trues <- test$acceleration
#' phi.trues <- phi(test$acceleration,ph)
#'
#' sera(trues,preds,phi.trues)
#' sera(trues,preds,phi.trues,pl=TRUE)
#' sera(trues,preds,phi.trues,pl=TRUE, m.name="Regression Trees")
#' sera(trues,preds,phi.trues,pl=TRUE, return.err=TRUE)
#' }
sera <- function(trues, preds, phi.trues=NULL, ph=NULL, pl=FALSE,
                 m.name="Model", step=0.001, return.err=FALSE, norm=FALSE) {

  if(!is.data.frame(preds)) preds <- as.data.frame(preds)

  if(is.null(phi.trues) && is.null(ph)) stop("You need to input either the parameter phi.trues or ph.")

  if(is.null(phi.trues)) phi.trues <- phi(trues,ph)

  tbl <- data.frame(trues=trues,phi=phi.trues,preds)

  th <- c(seq(0,1,step))

  ms <-colnames(tbl)[3:ncol(tbl)]

  errors <- sapply(ms,FUN=function(m) sapply(th, FUN = function(x) sum((tbl[tbl$phi>=x,]$trues-tbl[tbl$phi>=x,m])^2)))

  if(any(is.na(errors))) errors[is.na(errors)] <- 0

  if(norm) errors <- errors/errors[1]

  areas <- sapply(1:length(ms), FUN=function(m) sapply(2:length(th), FUN=function(x) step * (errors[x-1,m] + errors[x,m])/2 ))
  colnames(areas) <- ms
  rownames(areas) <- 1:nrow(areas)

  res <- apply(areas,2,FUN=function(x) sum(x))

  if(pl) {

    max_y <- max(errors)

    if(ncol(errors)>1) {

      df <- data.frame(th=th,errors)

      df_melt <- reshape::melt(df,id.vars="th")
      colnames(df_melt)[2] <- "Model"

      print(ggplot2::ggplot(df_melt,aes(x=th,y=value,group=Model,color=Model)) +
              ggplot2::geom_smooth(method="scam",formula=y ~ s(x, k = 30, bs = "mpd"),span=0.1,se=FALSE,fullrange=TRUE) +
              ggplot2::xlab(expression("Relevance"~phi(y))) + ylab("SER") +
              ggplot2::ggtitle("SERA") + ylim(c(0,max_y)) + ggplot2::geom_hline(yintercept=0,colour="black"))


    } else {

      df <- data.frame(th=th,errors=errors)

      print(ggplot2::ggplot(df,aes(x=th,y=errors)) +
              ggplot2::geom_smooth(method="scam",formula=y ~ s(x, k = 30, bs = "mpd"),span=0.1,se=FALSE,colour="blue") +
              ggplot2::xlab(expression("Relevance"~phi(y))) + ylab("SER") +
              ggplot2::ggtitle(paste0(m.name," SERA = ",round(res,3))) + ylim(c(0,max_y))+ ggplot2::geom_hline(yintercept=0,colour="black") + ggplot2::geom_hline(yintercept=df[2,]$preds,linetype="dashed",colour="darkgrey"))

    }

  }

  if(return.err) {

    list(sera=res, errors=as.vector(errors), thrs=th)

  } else {

    res
  }

}
