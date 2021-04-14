#' Simulate spatial data with spatial variability over a grid
#' 
#' Simulate a spatial data with specified mean yield and spatial variability simulated  
#' as a spatially correlated random field predicted on a grid. 
#' The spatial correlation structure is defined by an exponential variogram model with
#' specified psill and range, using gstat function 'vgm' with model = "Exp". Higher values 
#' of psill will result in a wider range fo simulated yield values. Higher values of
#' range will result in larger autocorrrelated regions. Output can be specified as RasterStack or 
#' SpatialPixelsDataFrame.
#'
#' @param mean Mean of the spatial data.
#' @param psill (partial) sill of the variogram model.
#' @param range range parameter of the variogram model.
#' @param nsim number of simulations.
#' @param xdim x-dimension of simulated yield (width).
#' @param ydim y-dimension of simulated yield (length).
#' @param output.type "RasterStack" / "SpatialPixelsDataFrame"
#'
#' @author Fiona Evans
#' 
#' @return RasterStack / SpatialPixelsDataFrame.
#' @export
spatial.sim <- function(mean = 3,  # Mean yield potential
                           psill = 0.25,    # Higher -> wider range of simulated values
                           range = 30,      # Degree of spatial autocorrelation
                                           # higher -> larger autocorrelated regions
                           nsim = 1, 
                           xdim = 100,
                           ydim = 100,
                           output.type = "RasterStack")        # Number of simulations
{
  
  # From http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
  
  # create spatial structure
  xy <- expand.grid(1:xdim, 1:ydim)
  names(xy) <- c("x", "y")
  
  # define the gstat object (spatial model)
  g.dummy <- gstat(formula = z~1, locations = ~x + y, dummy = T, beta = mean, 
                   model = vgm(psill = psill, model = "Exp", range = range), nmax=20)
  
  # make simulation based on the stat object
  yy <- predict(g.dummy, newdata = xy, nsim = nsim)
  gridded(yy) = ~x+y
  names(yy) <- paste0("yield", c(1:ncol(yy)))
  
  y <- raster::stack(yy)
  
  if (output.type == "RasterStack") return(y)
  if (output.type == "SpatialPixelsDataFrame") return(y)
}
