#sys.frame(1)$ofile

#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, xpd = NA, ...)
}

range02 <- function(x)(x + max(abs(x)))/(2 * max(abs(x)))

range01 <- function(x)(x-min(x))/diff(range(c(x, max(abs(x)))))

range03 <- function(x)(x-min(x))/diff(range(c(x, max(x))))

cRamp <- function(x, palette, alpha = 1){
  #cols <- colorRamp(c("red", "gray40", "blue"))(range02(x))
  #if (sum(x < 0) == length(x)){
    range <- range03(x)
  # }else{
  #   range <- range01(x)
  # }
  if (palette == "viridis"){
    cols <- colorRamp(viridis::viridis_pal()(10))(range)
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }else if( palette == "custom"){
    cols <- colorRamp(colorspace::diverge_hcl(n = length(range), c = c(100, 0), 
                                             l = c(50, 90)))(range)
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }else{
    cols <- suppressWarnings(colorRamp(RColorBrewer::brewer.pal(11, palette))(range))
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }
  return(cols)
}

cRamp_legend <- function(x, palette, alpha = 1){
  range <- seq(0, 1, length.out = x)
  
  if (palette != "viridis"){
    cols <- suppressWarnings(colorRamp(RColorBrewer::brewer.pal(11, palette))(range))
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }else{
    cols <- colorRamp(viridis::viridis_pal()(10))(range)
    cols <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    cols <- adjustcolor(cols, alpha.f = alpha)
  }
  
  return(cols)
}

plot_colors <- function(alpha = 1, plot = FALSE, type = 1){
  colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7", "#999999")
  
  colors <- adjustcolor(colors, alpha.f = alpha)
  
  names(colors) <- c("orange", "sky blue", "bluish green", "yellow", "blue",
                     "vermillion", "reddish purple", "gray")
  
  if (plot){
    par(mfrow = c(1,1), mar = c(0,0,0,0))
    plot(NA, xlim = c(0,1), ylim = c(0, length(colors) + 1), xaxt = "n",
         yaxt = "n", xlab = "", ylab = "", axes = FALSE)
    for (i in 1:length(colors)){
      lines(0:1, rep(i, 2), lwd = 3, col = colors[i])
      points(0.5, i, pch = 21, bg = colors[i], cex = 2)
      text(0.2, i, names(colors)[i], pos = 3)
    }
  }
  
  if (type == 2){
    colors <- c("#ff7f00","#1f78b4","#ffff33","#a6cee3","#33a02c","#e31a1c")
    colors <- adjustcolor(colors, alpha.f = alpha)
  }
  
  return(colors)
}

rodplot <- function(col = "gray60", ...){

  outbg <- adjustcolor(col, alpha.f = 0.7)
  if (!exists("outcex")){
    outcex = 1.3
  }
  
  boxplot( medlwd = 2, outpch = 21, col = col, outbg = outbg,
           whisklty = 1, staplelty = 0, ...)
  #, outbg = adjustcolor(col, alpha.f = 0.7)
}