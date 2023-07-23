
#' Total diversity and its components: layer (temporal), spatial and interaction term
#'
#' divcom partitions the overall diversity in a temporal, spatial and interaction term as proposed by Rossi et al. (2021).
#' The diversity calculation is based on the sum of squares divided by the number of pixels and layers. The diversity accounts
#' for the pairwise dissimilarity between pixels belonging to the same layer (temporal information) as well as the dissimilarity between pixels
#' of different layers. All pixels are considered equally important.
#'
#' @param x filename (character), Raster* object
#'
#' @return Named numeric vector with the contribution of each component (i.e., temporal, spatial, interaction and total)
#'
#' @author Christian Rossi christian.rossi1990@gmail.com
#'
#' @import raster
#'
#' @usage divcom(x)
#'
#' @references Rossi, C., Kneubühler, M., Schütz, M., Schaepman, M.E, Haller, R.M., & Risch, A.C. (2021). Remote sensing of spectral diversity:
#' A new methodological approach to account for spatio-temporal dissimilarities between plant communities. Ecological Indicators, 130, 108106.
#'
#' @examples \dontrun{#Calculate total diversity and its components of a RasterStack
#' ex <-divcom(MTCI.stabelchod)
#' # MTCI.stabelchod is a RasterStack containing
#' # a time series of spectral information
#' }
#'
#'
#' @export
#'


divcom <-function(x) {

  if (class(x)[1] != "RasterStack" && class(x)[1] !="RasterBrick")
  {
    stop("x is not RasterStack or RasterBrick")
  }

  n.pixel <-raster::cellStats(!is.na(x),sum)[1] #number of pixels that are not NA
  n.bands <-dim(x)[3] #number of layers

  tcd <-sum((raster::cellStats(x,mean,na.rm=TRUE)-mean(raster::cellStats(x, mean,na.rm=TRUE),na.rm=TRUE))^2,na.rm=TRUE) * n.pixel #temporal (layer) diversity

  scd <-raster::cellStats((raster::calc(x,fun=mean,na.rm=TRUE)-mean(raster::cellStats(x, mean,na.rm=TRUE),na.rm=TRUE))^2,sum,na.rm=TRUE) * n.bands #spatial (pixel) diversity

  cd <-sum(raster::cellStats((x-mean(raster::cellStats(x, mean,na.rm=TRUE),na.rm=TRUE))^2,sum,na.rm=TRUE),na.rm=TRUE) #total diversity

  icd <-(cd-tcd-scd) #interaction term

  dc.return <-c(tcd,scd,icd,cd)/(n.bands*n.pixel) #contribution of the components

  names(dc.return) <-c("Layer component", "Spatial component", "Interaction term", "Total diversity")



  return(dc.return)


}
