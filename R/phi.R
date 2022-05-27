## keep up to date (in R and C)
phiMethods <- c("extremes","range")

# Dynamic Library for C functions
#' @useDynLib IRon
#' @importFrom Rcpp sourceCpp
NULL

# ======================================================================

#' Obtain the relevance of data points
#'
#' @description The phi function retrieves the relevance value of the values in a target variable. It does so by resorting to the Piecewise Cubic Hermitate Interpolation Polynomial method for interpolating over a set of maximum and minimum relevance points. The notion of relevance is associated with rarity.Nonetheless, this notion may depend on the domain experts knowledge
#'
#' @param y The target variable of a given data set
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known
#' @param only.phi Boolean (default TRUE) to return either solely the relevance values or the full data structure with the first and second derivative the interpolated values
#'
#' @return A vector with the relevance values of a given target variable
#'
#' @export
#'
#' @examples
#' library(IRon)
#' data(accel)
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' ph <- phi.control(train$acceleration)
#' phis <- phi(test$acceleration,phi.parms=ph)
#'
#' plot(test$acceleration,phis,xlab="Y",ylab="Relevance")
phi <- function(y, phi.parms=NULL, only.phi=TRUE) {

  phi.parms <- if(is.null(phi.parms)) phi.control(y) else phi.parms

  n <- length(y)

  res <- .C("r2phi",
            n = as.integer(n),
            y = as.double(y),
            phi.parms = phi2double(phi.parms),
            y.phi = double(n),
            yd.phi = double(n),
            ydd.phi = double(n)
            )[c('y.phi','yd.phi','ydd.phi')]

  if(only.phi)
    res$y.phi
  else
    res
}

#' Generation of relevance function
#'
#' @description This procedure enables the generation of a relevance function that performs a mapping between the values in a given target variable and a relevance value that is bounded by 0 (minimum relevance) and 1 (maximum relevance). This may be obtained automatically (based on the distribution of the target variable) or by the user defining the relevance values of a given set of target values - the remaining values will be interpolated.
#'
#' @param y The target variable of a given data set
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known
#' @param method The method used to generate the relevance function (extremes or range)
#' @param extr.type Type of extremes to be considered: low, high or both (default)
#' @param control.pts Parameter required when using 'range' method, representing a 3-column matrix of y-value, corresponding relevance value (between 0 and 1), and the derivative of such relevance value
#' @param asym Boolean for assymetric interpolation. Default TRUE, uses adjusted boxplot. When FALSE, uses standard boxplot.
#' @param ... Misc data to be added to the relevance function
#'
#' @return A list with three slots with information concerning the relevance function
#' \item{method}{The method used to generate the relevance function (extremes or range)}
#' \item{npts}{?}
#' \item{control.pts}{Three sets of values identifying the target value-relevance-derivate for the first low extreme value, the median, and first high extreme value}
#'
#' @export
#'
#' @examples
#' library(IRon)
#'
#' data(accel)
#'
#' ind <- sample(1:nrow(accel),0.75*nrow(accel))
#'
#' train <- accel[ind,]
#' test <- accel[-ind,]
#'
#' ph <- phi.control(train$acceleration); phiPlot(test$acceleration, ph)
#' ph <- phi.control(train$acceleration, extr.type="high"); phiPlot(test$acceleration, ph)
#' ph <- phi.control(train$acceleration, method="range",
#'   control.pts=matrix(c(10,0,0,15,1,0),byrow=TRUE,ncol=3)); phiPlot(test$acceleration, ph)
#'
phi.control <- function(y, phi.parms, method = phiMethods,
  extr.type = NULL, control.pts = NULL, asym = TRUE, ...) {

  call <- match.call()

  # Setup of Relevance Function - BEGIN

  if(!missing(phi.parms)) {
    method <- phi.parms$method
    extr.type <- phi.parms$extr.type
    control.pts <- phi.parms$control.pts
  }

  method <- match.arg(method, phiMethods)

  control.pts <- do.call(paste("phi",method,sep="."),
    c(list(y=y), extr.type = extr.type,
      list(control.pts=control.pts), asym = asym, ...))

  phiP <- list(method = method,
    npts = control.pts$npts, control.pts = control.pts$control.pts)

  # Setup of Relevance Function - END

  phiP

}

#' Relevance function for extreme target values
#'
#' @description Automatic approach to obtain a relevance function for a given target variable when the option of extremes is chosen, i.e. users are more interested in accurately predicting extreme target values
#'
#' @param y The target variable of a given data set
#' @param extr.type Type of extremes to be considered: low, high or both (default)
#' @param coef Boxplot coefficient (default 1.5)
#' @param asym Boolean for assymetric interpolation. Default TRUE, uses adjusted boxplot. When FALSE, uses standard boxplot.
#' @param ... Additional parameters
#'
#' @keywords internal
#'
#' @return A list with three slots with information concerning the relevance function
#' \item{method}{The method used to generate the relevance function (extremes or range)}
#' \item{npts}{?}
#' \item{control.pts}{Three sets of values identifying the target value-relevance-derivate for the first low extreme value, the median, and first high extreme value}
phi.extremes <- function(y, extr.type = c("both","high","low"),
                         coef=1.5, asym=TRUE, ...) {

  extr.type <- match.arg(extr.type)

  control.pts <- NULL

  npts <- NULL

  if(asym) {

    extr <- robustbase::adjboxStats(y,coef=coef)

    r <- range(y)

    if(extr.type %in% c("both","low")) {

      ## adjL
      control.pts <- rbind(control.pts,c(extr$fence[1],1,0))

    } else {

      ## min
      control.pts <- rbind(control.pts,c(r[1],0,0))
    }

    ## median
    control.pts <- rbind(control.pts,c(extr$stats[3],0,0))

    if(extr.type %in% c("both","high")) {

      ## adjH
      control.pts <- rbind(control.pts,c(extr$fence[2],1,0))

    } else {

      ## max
      control.pts <- rbind(control.pts,c(r[2],0,0))

    }

    npts <- NROW(control.pts)

  } else {

    extr <- boxplot.stats(y,coef=coef)

    r <- range(y)

    if(extr.type %in% c("both","low") &&
        any(extr$out < extr$stats[1])) {

      ## adjL
      control.pts <- rbind(control.pts,c(extr$stats[1],1,0))

    } else {

      ## min
      control.pts <- rbind(control.pts,c(r[1],0,0))
    }

    ## median
    control.pts <- rbind(control.pts,c(extr$stats[3],0,0))

    if(extr.type %in% c("both","high") &&
        any(extr$out > extr$stats[5])) {

      ## adjH
      control.pts <- rbind(control.pts,c(extr$stats[5],1,0))

    } else {

      ## max
      control.pts <- rbind(control.pts,c(r[2],0,0))

    }

    npts <- NROW(control.pts)

  }

  list(npts = npts,
       control.pts = as.numeric(t(control.pts)))

}


#' Custom Relevance Function
#'
#' @description User-guided approach to obtain a relevance function for certain intervals of the target variable when the option of range is chosen in function phi.control, i.e. users define the relevance of values for which it is known
#'
#' @param y The target variable of a given data set
#' @param control.pts Parameter representing a 3-column matrix of y-value, corresponding relevance value (between 0 and 1), and the derivative of such relevance value, allowing users to specify the known relevance at given target values
#' @param ... Additional parameters
#'
#' @keywords internal
#'
#' @return A list with three slots with information concerning the relevance function
#' \item{method}{The method used to generate the relevance function (extremes or range)}
#' \item{npts}{?}
#' \item{control.pts}{Three sets of values identifying the target value-relevance-derivate for the first low extreme value, the median, and first high extreme value}
phi.range <- function(y, control.pts, ...) {

  ## if it comes from pre-set env
  if(!is.null(names(control.pts)))
    control.pts <- matrix(control.pts$control.pts,nrow=control.pts$npts,byrow=T)

  if(missing(control.pts) || !is.matrix(control.pts) ||
     (NCOL(control.pts) > 3 || NCOL(control.pts) < 2))
    stop('The control.pts must be given as a matrix in the form: \n',
         '< x, y, m > or, alternatively, < x, y >')

  npts <- NROW(control.pts)
  dx <- control.pts[-1L,1L] - control.pts[-npts,1L]

  if(any(is.na(dx)) || any(dx == 0))
    stop("'x' must be *strictly* increasing (non - NA)")

  if(any(control.pts[,2L] > 1 | control.pts[,2L] < 0))
    stop("phi relevance function maps values only in [0,1]")

  control.pts <- control.pts[order(control.pts[,1L]),]

  if(NCOL(control.pts) == 2) {

    ## based on "monoH.FC" method
    dx <- control.pts[-1L,1L] - control.pts[-npts,1L]
    dy <- control.pts[-1L,2L] - control.pts[-npts,2L]
    Sx <- dy / dx

    ## constant extrapolation
    m <- c(0, (Sx[-1L] + Sx[-(npts-1)]) / 2, 0)

    control.pts <- cbind(control.pts,m)

  }

  r <- range(y)
  npts <- NROW(matrix(control.pts,ncol=3))


  list(npts = npts,
       control.pts = as.numeric(t(control.pts)))

}

#Auxiliary function
phi2double <- function(phi.parms) {
  phi.parms$method <- match(phi.parms$method,phiMethods) - 1

  as.double(unlist(phi.parms))
}
