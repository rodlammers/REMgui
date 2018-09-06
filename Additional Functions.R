plot_network <- function(n_nodes, link){
  
  angle <- pi/8
  x <- matrix(0, nrow = n_nodes + 1, ncol = 2)
  y <- x
  branched <- rep(1, n_nodes)
  
  sgn <- rep(1, n_nodes)
  
  for (i in seq(n_nodes, 1, -1)){
    if (i < n_nodes){
      index <- which(i == link, arr.ind = TRUE)[2]
      ang <- angle / branched[index]
      x[i, 2] <- x[index, 1] #+ sgn[index] * sin(ang)
      y[i, 2] <- y[index, 1] #+ cos(ang)
      x[i, 1] <- x[i, 2] + sgn[index] * sin(ang)
      y[i, 1] <- y[i, 2] + cos(ang)
      
      sgn[index] <- -1 * sgn[index]
      branched[i] <- branched[index] + 1
    }else{
      index <- n_nodes + 1
      x[i, 2] <- x[i + 1, 1]
      y[i, 2] <- y[i + 1, 1]
      x[i, 1] <- x[i, 2]
      y[i, 1] <- y[i, 2] + 1
    }
  }
  
  xmin <- min(x)
  xmax <- max(x)
  ymax <- max(y)
  par(mar = rep(0.5, 4))
  plot(NA, xlim = c(xmin, xmax * 1.05), ylim = c(0, ymax), yaxt = "n",
       xaxt = "n", xlab = "", ylab = "", asp = 1)
  for (i in 1:n_nodes){
    lines(x[i,1:2], y[i,1:2], lwd = 4)
    text(x[i, 1], y[i, 1], label = i, pos = 2)
  }
  
  #return(list(x = x, y = y))
}


