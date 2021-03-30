

###
### Strip designs ----
###

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with increasing rates from west to east.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
incr.design <- function(rates, reps, ydim = 100, width = 3){
  strips <- matrix(0, nrow=ydim, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
    strips[, indx:(indx + width - 1)] <- rates[i]
    indx <- indx + width
    }
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@xmin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with decreasing rates from west to east.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
decr.design <- function(rates, reps, ydim = 100, width = 3){
  strips <- matrix(0, nrow=ydim, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[length(rates)+1-i]
      indx <- indx + width
    }
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@xmin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with increasing rates from west to east,
#' then decreasing.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
incdec.design <- function(rates, reps, ydim = 100, width = 3){
  strips <- matrix(0, nrow=ydim, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[i]
      indx <- indx + width
    }
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[length(rates)+1-i]
      indx <- indx + width
    }
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@xmin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with decreasing rates from west to east,
#' then increasing.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
decinc.design <- function(rates, reps, ydim = 100, width = 3){
  strips <- matrix(0, nrow=ydim, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[length(rates)+1-i]
      indx <- indx + width
    }
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[i]
      indx <- indx + width
    }
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@xmin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Randomised complete block (RCBD) strip design.
#' 
#' Randomised complete block (RCBD) strip design for a fertiliser trial.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
rcbd.design <- function(rates, reps, ydim = 100, width = 3){
  strips <- matrix(0, nrow=ydim, ncol=(reps*length(rates)*width))
  rcbd <- design.rcbd(rates, reps, serie=0)
  indx <- 1 
  for (i in 1:(length(rates) * reps)){
    strips[, indx:(indx + width - 1)] <- as.numeric(as.character(rcbd$book[i, "rates"]))
    indx <- indx + width
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@xmin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}
  
  
  






###
### Checkerboard designs ----
###

#' Randomised checkerboard design.
#' 
#' Fully randomised checkerboard design for a fertiliser trial.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length length of the checkeboard squares.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
ch.fully.random.design <- function(rates, reps, length, ydim = 100, width = 3){
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(ydim/length)          # no. of plots in each strip
  crd <- design.crd(rates, nx*ny, serie=0)
  
  strips <- matrix(0, nrow=ny * length, ncol=(reps*length(rates)*width))
  i <- iy <- 1
  while(iy <= ny * length){
    ix <- 1
    while(ix <= nx * width){
      #text(ix + 1, iy + 1, i, cex=0.5)
      strips[iy:(iy + length - 1), ix:(ix + width - 1)] <- as.numeric(as.character(crd$book[i, "rates"]))
      ix <- ix + width
      i <- i + 1
    }
    iy <- iy + length
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@ymin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Randomised complete block (RCBD) checkerboard design.
#' 
#' Randomised complete block (RCBD) checkerboard design for a fertiliser trial.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length length of the checkeboard squares.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
ch.rcbd.design <- function(rates, reps, length, ydim = 100, width = 3){
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(ydim/length)          # no. of plots in each strip
  rcbd <- design.rcbd(rates, nx*ny, serie=0)
  
  strips <- matrix(0, nrow=ny * length, ncol=(reps*length(rates)*width))
  # blocks of four strips, one row
  i <- iy <- 1
  while(iy <= ny * length){
    ix <- 1
    while(ix <= nx * width){
      #text(ix + 1, iy + 1, i, cex=0.5)
      strips[iy:(iy + length - 1), ix:(ix + width - 1)] <- as.numeric(as.character(rcbd$book[i, "rates"]))
      ix <- ix + width
      i <- i + 1
    }
    iy <- iy + length
  }
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@ymin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Systematic checkerboard design.
#' 
#' Systematic checkerboard design for a fertiliser trial, with multiple repetitions of Nil (rate[1])
#' and repetition of rates across strips (west to east).
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length length of the checkeboard squares.
#' @param ydim y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export  
ch.zeroes.design1 <- function(rates, reps, ydim = 100, width = 3){    # assumes rates[1] == 0
  length <- 2 * (length(rates) - 1)
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(ydim/length)          # no. of plots in each strip
  
  st <- list(
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[2], rates[1], rates[3], rates[1], rates[4], rates[1]),
    c(rates[1], rates[3], rates[1], rates[4], rates[1], rates[2]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[4], rates[1], rates[2], rates[1], rates[3]),
    c(rates[4], rates[1], rates[2], rates[1], rates[3], rates[1])
  )
  
  strips <- matrix(0, nrow=ny * length, ncol=(reps*length(rates)*width))
  
  i <- ix <- indx <- 1
  while(ix <= nx * width) {
    iy <- j <- 1
    while(iy <= ny * length){
      #text(ix + 1, iy + 1, indx, cex=0.5)
      strips[iy:(iy + length - 1), ix:(ix + width - 1)] <- st[[i]][j]
      iy <- iy + length  
      j <- j + 1
      if (j > length) j <- 1
      indx <- indx + 1
      
    }
    i <- i + 1
    ix <- ix + width
    if (i > length) i <- 1
  }
  
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@ymin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

#' Systematic checkerboard design.
#' 
#' Systematic checkerboard design for a fertiliser trial, with multiple repetitions of Nil (rates[1])
#' and same rate repeated across strips (west to east).
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length length of the checkeboard squares.
#' @param ydim y-dimension of simulated trial design
#' @param width width of strips
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export  
ch.zeroes.design2 <- function(rates, reps, ydim = 100, width = 3){    # assumes rates[1] == 0
  length <- 2 * (length(rates) - 1)
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(ydim/length)          # no. of plots in each strip
  
  st <- list(
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1])
  )
  
  strips <- matrix(0, nrow=ny * length, ncol=(reps*length(rates)*width))
  
  i <- ix <- indx <- 1
  while(ix <= nx * width) {
    iy <- j <- 1
    while(iy <= ny * length){
      #text(ix + 1, iy + 1, indx, cex=0.5)
      strips[iy:(iy + length - 1), ix:(ix + width - 1)] <- st[[i]][j]
      iy <- iy + length  
      j <- j + 1
      if (j > length) j <- 1
      indx <- indx + 1
      
    }
    i <- i + 1
    ix <- ix + width
    if (i > length) i <- 1
  }
  
  res <- raster::raster(strips)
  extent(res)@xmin <- 0.5
  extent(res)@ymin <- 0.5
  extent(res)@xmax <- 0.5 + ncol(strips)
  extent(res)@ymax <- 0.5 + nrow(strips)
  res
}

