
#' Total diversity and its components: spatial, layer (temporal) and interaction term
#'
#' divcom partitions the overall diversity in a spatial, temporal and interaction term as proposed by Rossi et al. (2020).
#' The diversity calculation is based on the sum of square divided by the number of pixels and layers. The diversity accounts for
#' for the pairwise dissimilarity between pixels belonging to the same layer as well as the dissimilarity between pixels
#' of different layers. All pixels are considered equally important.
#'
#' @param x filename (character), Raster* object
#'
#' @return Named numeric vector with contribution of each layer to diversity
#'
#' @author Christian Rossi christian.rossi1990@gmail.com
#'
#' @import raster
#' @import rgdal
#'
#' @usage divcom(x)
#'
#' @references Rossi et al. (2020) Remote Sensing of Environment 236:111415
#' (\href{https://www.sciencedirect.com/science/article/pii/S0034425719304341}{ScienceDirect})
#'
#'
#' @examples \dontrun{#Calculate total diverstiy and its components of a RasterStack
#' ex <-divcom(MTCI.stabelchod)
#' # MTCI.stabelchod is a RasterStack containing
#' # a time series of spectral information
#' }
#'
#'
#' @export
#'

library(raster)

divcom <-function(x) {


  n.pixel <-cellStats(!is.na(x),sum)[1] #number of pixels that are not NA
  n.bands <-dim(x)[3] #number of layers

  tcd <-sum((cellStats(x,mean)-mean(cellStats(x, mean)))^2) * n.pixel #temporal (layer) diversity

  scd <-cellStats((calc(x,fun=mean,na.rm=TRUE)-mean(cellStats(x, mean)))^2,sum) * n.bands #spatial (pixel) diversity

  cd <-sum(cellStats((x-mean(cellStats(x, mean)))^2,sum)) #total diversity

  icd <-(cd-tcd-scd) #interaction term

  dc.return <-c(tcd,scd,icd,cd)/(n.bands*n.pixel) #contribution of the components

  names(dc.return) <-c("Layer component", "Spatial component", "Intercation term", "Total diversity")



  return(dc.return)


}
