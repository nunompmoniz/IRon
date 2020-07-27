#' Plot of phi versus y and boxplot of y
#'
#' @description The phiPlot function uses a dataset ds containing many y values to produce a line plot of phi versus y and a boxplot of y, and aligns them, one above the other. The first extreme value on either side of the boxplot should correspond to the point where phi becomes exactly 1 on the line plot. This function is dependent on the robustbase, ggplot2, plyr and ggpubr packages, and will not work without them.
#'
#' @param ds Dataset of y values
#' @param phi.parms The relevance function providing the data points where the pairs of values-relevance are known. Default is NULL
#' @param limits Vector with values to draw limits. Default is NULL
#' @param xlab Label of the x axis. Default is y
#' @param ... Extra parameters when deriving the relevance function
#'
#' @return A line plot of phi versus y, as well as a boxplot of y
#'
#' @export
#'
#' @importFrom robustbase adjboxStats
#' @importFrom ggplot2 ggplot aes geom_line geom_point ylab geom_boxplot ggplotGrob ggplot_gtable ggplot_build ylim
#' @importFrom ggpubr theme_transparent rotate
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr ddply
#' @importFrom grDevices boxplot.stats
#'
#' @examples
#' \dontrun{
#' ds <- rnorm(1000, 30, 10); phi.parms <- phi.control(ds); phiPlot(ds,phi.parms)
#' ds <- rpois(100,3); phiPlot(ds)
#' }
phiPlot <- function(ds, phi.parms=NULL, limits=NULL, xlab="y", ...) {

  phis <- c()

  if(is.null(phi.parms)) {
    warning("Deriving a relevance function from the data set in parameter ds ...")
    phi.parms <- phi.control(ds, ...)
    phis <- phi(ds, phi.parms)
  } else {
    phis <- phi(ds, phi.parms)
  }

  df <- data.frame(y=ds,phi=phis)
  df <- df[order(df$y),]

  # Graph of y versus phi
  p1 <- NULL

  if(is.null(limits)) {
    p1 <- ggplot(df,aes(x=y,y=phi)) + geom_line() + ylab(parse(text=paste("phi(",eval(xlab),")",sep=""))) + ylim(0,1) + ggplot2::xlab(xlab)
  } else {
    p1 <- ggplot(df,aes(x=y,y=phi)) + geom_line() + ylab(parse(text=paste("phi(",eval(xlab),")",sep=""))) + ylim(0,1) + ggplot2::xlab(xlab) +
      ggplot2::geom_vline(xintercept=limits,colour="darkgrey",linetype="dashed")
  }

  # Creating stats for the boxplot
  adjStats <- adjboxStats(df$y)$stats
  d <- data.frame(ymin=adjStats[1],ymax=adjStats[5],
                  middle=adjStats[3],
                  lower=adjStats[2],upper=adjStats[4])

  # Boxplot of phi

  p2 <- NULL

  if(any(df$y<d$ymin) | any(df$y>d$ymax)) {
    p2 <- ggplot(d, aes(factor(1))) + geom_boxplot(aes(ymin=d$ymin, ymax=d$ymax,
      middle=d$middle, upper=d$upper, lower=d$lower), stat="identity", fill="lightgray") +
      ggplot2::geom_errorbar(aes(ymin=d$ymin,ymax=d$ymax),linetype = 1,width = 0.5) +
      geom_point(aes(y=y), data=subset(df, y < d$ymin | y > d$ymax)) +
      rotate() + theme_transparent()
  } else {
    p2 <- ggplot(d, aes(factor(1))) + geom_boxplot(aes(ymin=d$ymin, ymax=d$ymax,
      middle=d$middle, upper=d$upper, lower=d$lower), stat="identity", fill="lightgray") +
      ggplot2::geom_errorbar(aes(ymin=d$ymin,ymax=d$ymax),linetype = 1,width = 0.5) +
      rotate() + theme_transparent()
  }



  #Adjust plot widths so that boxplot and line plot are correctly aligned
  p1_grob <- ggplotGrob(p1)
  p2_grob <- ggplotGrob(p2)
  p1_plot <- ggplot_gtable(ggplot_build(p1))
  p2_plot <- ggplot_gtable(ggplot_build(p2))
  p2_plot$widths <-p1_plot$widths

  # Overlay plots
  grid.arrange(p1_plot, p2_plot,
               layout_matrix = rbind(c(2,2,2), c(1,1,1), c(1,1,1), c(1,1,1), c(1,1,1), c(1,1,1)))

}
