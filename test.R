# roxygen2::roxygenize('.')

library(OFEsim)

dat <- simulate.yield(mean.yield = 3,  # Mean yield potential
                           psill = 0.25,    # Higher -> wider range of simulated values
                           range = 30,      # Degree of spatial autocorrelation
                                            # higher -> larger autocorrelated regions
                           nsim = 1) 

# Fertiliser rates to use in trial
rates <- c(0, 30, 60, 100) 

# Number of reps
reps <- 2

###
### Plot strip designs ----
###

strips1 <- incr.design(rates, reps)
strips2 <- decr.design(rates, reps)
strips3 <- incdec.design(rates, reps)
strips4 <- decinc.design(rates, reps)
strips5 <- rcbd.design(rates, reps)


#png(filename = "./Figures/designs.png", width=885, height=430, res=100)
tmp <- raster::stack(list(strips1, strips2, 
                          strips3, strips4, 
                          strips5))
names(tmp) <- c('Increasing', 'Decreasing', 'IncDec', 'DecInc', 'RCBD')
plot(tmp, axes=F, box=F, nc=5, legend.mar=15, 
     axis.args=list(cex.axis=1), legend=F, 
     legend.width=1.2, legend.shrink=0.75)
#dev.off()


###
### Plot checkerboard designs ----
###

strips1 <- ch.fully.random.design(rates, reps, 6)
strips2 <- ch.rcbd.design(rates, reps, 6)
strips3 <- ch.zeroes.design1(rates, reps)
strips4 <- ch.zeroes.design2(rates, reps)


#png(filename = "./Figures/checkerboard designs.png", width=885, height=430, res=100)
tmp <- raster::stack(list(strips1, strips2, strips3, strips4))
names(tmp) <- c('Random.chk', 'RCBD.chk', 'Regular.chk.1', 'Regular.chk.2')
plot(tmp, axes=F, box=F, nc=5, legend.mar=15, 
     axis.args=list(cex.axis=1), legend=F, 
     legend.width=1.2, legend.shrink=0.75)
#dev.off()
