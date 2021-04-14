

###
### Strip designs ----
###

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with increasing rates from west to east.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
incr.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
    strips[, indx:(indx + width - 1)] <- rates[i]
    indx <- indx + width
    }
  }
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with decreasing rates from west to east.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
decr.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
  indx <- 1
  while(indx <= reps*length(rates)*width) {
    for (i in 1:length(rates))  {
      strips[, indx:(indx + width - 1)] <- rates[length(rates)+1-i]
      indx <- indx + width
    }
  }
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with increasing rates from west to east,
#' then decreasing.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
incdec.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
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
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Systematic strip design.
#' 
#' Systematic strip design for a fertiliser trial with decreasing rates from west to east,
#' then increasing.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
decinc.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
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
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}


#' Systematic repeated design (lowest, up one, highest, down one) 
#' Systematic repeated strip design.
#' 
#' Systematic strip design for a fertiliser trial with the lowest rate in the west,
#' increasing to the next highest, then the highest rate, decreasing to second highest etc.,
#' repated exactly for each rep. Note the number of rates must be a a multiple of 4!!!
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
sys.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
  indx <- 1
  for (k in 1:reps) {
    rates1 <- rates
    i <- which.min(rates1)
    while (length(rates1) > 0) {
      strips[, indx:(indx + width - 1)] <- rates1[i]
      rates1 <- rates1[-i]
      indx <- indx + width
      i <- which.min(rates1)
      strips[, indx:(indx + width - 1)] <- rates1[i]
      rates1 <- rates1[-i]
      indx <- indx + width
      i <- which.max(rates1) 
      strips[, indx:(indx + width - 1)] <- rates1[i]
      rates1 <- rates1[-i]
      indx <- indx + width
      i <- which.max(rates1) 
      strips[, indx:(indx + width - 1)] <- rates1[i]
      rates1 <- rates1[-i]
      indx <- indx + width
    }
  }
  
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Randomised complete block (RCBD) strip design.
#' 
#' Randomised complete block (RCBD) strip design for a fertiliser trial.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
rcbd.design <- function(rates, reps, length = 100, width = 3){
  strips <- matrix(0, nrow=length, ncol=(reps*length(rates)*width))
  rcbd <- design.rcbd(rates, reps, serie=0)
  indx <- 1 
  for (i in 1:(length(rates) * reps)){
    strips[, indx:(indx + width - 1)] <- as.numeric(as.character(rcbd$book[i, "rates"]))
    indx <- indx + width
  }
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
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
#' @param check.length length of the checkerboard squares.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
ch.fully.random.design <- function(rates, reps, check.length, length = 100, width = 3){
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(length/check.length)          # no. of plots in each strip
  crd <- design.crd(rates, nx*ny, serie=0)
  
  strips <- matrix(0, nrow=(ny * check.length), ncol=(reps*length(rates)*width))
  i <- iy <- 1
  while(iy <= ny * check.length){
    ix <- 1
    while(ix <= nx * width){
      #text(ix + 1, iy + 1, i, cex=0.5)
      strips[iy:(iy + check.length - 1), ix:(ix + width - 1)] <- as.numeric(as.character(crd$book[i, "rates"]))
      ix <- ix + width
      i <- i + 1
    }
    iy <- iy + check.length
  }
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Randomised complete block (RCBD) checkerboard design.
#' 
#' Randomised complete block (RCBD) checkerboard design for a fertiliser trial.
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param check.length length of the checkerboard squares.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export
ch.rcbd.design <- function(rates, reps, check.length, length = 100, width = 3){
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(length/check.length)          # no. of plots in each strip
  rcbd <- design.rcbd(rates, nx*ny, serie=0)
  
  strips <- matrix(0, nrow=ny * check.length, ncol=(reps*length(rates)*width))
  # blocks of four strips, one row
  i <- iy <- 1
  while(iy <= ny * check.length){
    ix <- 1
    while(ix <= nx * width){
      #text(ix + 1, iy + 1, i, cex=0.5)
      strips[iy:(iy + check.length - 1), ix:(ix + width - 1)] <- as.numeric(as.character(rcbd$book[i, "rates"]))
      ix <- ix + width
      i <- i + 1
    }
    iy <- iy + check.length
  }
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Systematic checkerboard design.
#' 
#' Systematic checkerboard design for a fertiliser trial, with multiple repetitions of Nil (rate[1])
#' and repetition of rates across strips (west to east). Note: only works for 4 rates!
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param check.length length of the checkerboard squares.
#' @param length y-dimension of simulated trial design.
#' @param width width of strips.
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export  
ch.zeroes.design1 <- function(rates, reps, check.length, length = 100, width = 3){    # assumes rates[1] == 0
  length1 <- 2 * (length(rates) - 1)
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(length/length1)      # no. of plots in each strip
  
  # This only works for 4 rates
  st <- list(
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[2], rates[1], rates[3], rates[1], rates[4], rates[1]),
    c(rates[1], rates[3], rates[1], rates[4], rates[1], rates[2]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[4], rates[1], rates[2], rates[1], rates[3]),
    c(rates[4], rates[1], rates[2], rates[1], rates[3], rates[1])
  )
  
  strips <- matrix(0, nrow=ny * length1, ncol=(reps*length(rates)*width))
  
  i <- ix <- indx <- 1
  while(ix <= nx * width) {
    iy <- j <- 1
    while(iy <= ny * length1){
      #text(ix + 1, iy + 1, indx, cex=0.5)
      strips[iy:(iy + length1 - 1), ix:(ix + width - 1)] <- st[[i]][j]
      iy <- iy + length1  
      j <- j + 1
      if (j > length1) j <- 1
      indx <- indx + 1
      
    }
    i <- i + 1
    ix <- ix + width
    if (i > length1) i <- 1
  }
  
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

#' Systematic checkerboard design.
#' 
#' Systematic checkerboard design for a fertiliser trial, with multiple repetitions of Nil (rates[1])
#' and same rate repeated across strips (west to east). Note: only works for 4 rates!
#'
#' @param rates vector of rates.
#' @param reps number of repetitions.
#' @param check.length length of the checkerboard squares.
#' @param length y-dimension of simulated trial design
#' @param width width of strips
#'
#' @author Fiona Evans
#' 
#' @return Raster. 
#' @export  
ch.zeroes.design2 <- function(rates, reps, check.length, length = 100, width = 3){    
  # assumes rates[1] == 0
  length1 <- 2 * (length(rates) - 1)
  nx <- reps*length(rates)         # no. of strips
  ny <- floor(length/length1)          # no. of plots in each strip
  
  # This only works for 4 rates
  st <- list(
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1]),
    c(rates[1], rates[2], rates[1], rates[3], rates[1], rates[4]),
    c(rates[3], rates[1], rates[4], rates[1], rates[2], rates[1])
  )
  
  strips <- matrix(0, nrow=ny * length1, ncol=(reps*length(rates)*width))
  
  i <- ix <- indx <- 1
  while(ix <= nx * width) {
    iy <- j <- 1
    while(iy <= ny * length1){
      #text(ix + 1, iy + 1, indx, cex=0.5)
      strips[iy:(iy + length1 - 1), ix:(ix + width - 1)] <- st[[i]][j]
      iy <- iy + length1  
      j <- j + 1
      if (j > length1) j <- 1
      indx <- indx + 1
      
    }
    i <- i + 1
    ix <- ix + width
    if (i > length1) i <- 1
  }
  
  res <- raster::raster(strips, 
                        xmn=0.5, xmx=0.5 +  ncol(strips),
                        ymn=0.5, ymx=0.5 +  nrow(strips))
  res
}

