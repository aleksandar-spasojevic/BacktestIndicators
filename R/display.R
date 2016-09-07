
#' @export
plot.backtest <- function(x,
                          main = "Cumulative Performance:", legend.loc = "topleft",
                          col = colors, grid.ticks.lwd = 0, grid.col = 0,
                          lwd = 1.3, yaxis.left = FALSE, major.ticks = "years",
                          .xts_version = xts_version(),
                          ...){

  if (.xts_version["minor"] < 10)
    stop(sprintf("version of package 'xts' is %d.%d-%d; we need >=0.10",
                 .xts_version["major"],.xts_version["minor"],.xts_version["patch"]))

  mapply(FUN = function(asset, value, positions,...){
    plot.xts(as.xts(do.call(merge.zoo,lapply(value,unname))),
             main = sprintf("%s %s", main, asset),
             legend.loc = legend.loc, col = col, grid.ticks.lwd = grid.ticks.lwd,
             grid.col = grid.col, lwd = lwd, yaxis.left = yaxis.left,
             major.ticks = major.ticks,
             ...)
  }, names(x$Strategies), x$Strategies, x$Positions,
  MoreArgs = list(...))

}

colors <- c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",
            "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

xts_version <- function(){
  version <- asNamespace("xts")$`.__NAMESPACE__.`$spec[["version"]]
  structure(as.numeric( unlist(strsplit(version, "[.-]+")) ),
            names = c("major","minor","patch"))
}

#' @export
addPositions <- function(positions, main = "Position", col = colors, on = NA,
                         type = "p", pch = 20,
                         ylim = {
                           .min <- min(sapply(positions[[1]], min, na.rm = TRUE))
                           .max <- max(sapply(positions[[1]], max, na.rm = TRUE))

                           c( .min - abs(0.2*.min), .max + 0.2*.max )
                           },
                         ...){

  addSeries(as.xts( do.call(merge.zoo, lapply(positions[[1]], unname)) ),
            main = main, col = col, on = on, type = type, pch = pch, ylim = ylim, ...)
}

#' @export
below <- function(fun, ..., on = NA) fun(on = on, ...)
