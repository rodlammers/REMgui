#' @param D50 Median grain size (mm).
#' @param sp Spread of distribution (default is 1).
#' @param ds Vector of grain sizes to map the gsd to (mm).

library(stochasim)

gsd_maker <- function(D50, sp, ds, plot = TRUE){
  par(mfrow = c(1,1), mar = c(4, 4, 0.5, 0.5), oma = rep(0, 4))
  gsd <- sim_gsd(D50 = D50, sp = sp, Map = plot)
  cdf <- approx(gsd$size_class, gsd$cdf, xout = ds)$y
  cdf[which(is.na(cdf[1:2]))] <- 0
  cdf[which(is.na(cdf))] <- 1
  cdf[length(cdf)] <- 1
  if (plot){
    lines(ds, cdf, col = "red")
    points(ds, cdf, pch = 16, col = "red")
  }
  ps <- diff(c(0,cdf))
  
  return(ps)
}

calc_Dx <- function(ps, Ds, x = 0.5){
  
  cdf <- cumsum(ps)
  Dx <- 2 ^ approx(cdf, log2(Ds), xout = x)$y
  
  return(Dx)
}

calc_sg <- function(ps, Ds){
  phi_g <- log2(Ds) * ps
  
  sg <- 2 ^ sum((phi_g - log2(Ds)) ^ 2 * ps)
  
  return(sg)
  
}
