

#' Layer contribution to diversity
#'
#' bcd computes the contribution of each layer to the overall diversity as proposed by Rossi et al. (2020). The diversity
#' calculation is based on the sum of square divided by the number of pixels and layers. The diversity accounts for the pairwise
#' dissimilarity between pixels belonging to the same layer as well as the dissimilarity between pixels of
#' different layers. All pixels are considered equally important.
#'
#' @param x filename (character), Raster* object
#' @param percentage logical. If TRUE the contribution of each layer is expressed as percentage.
#'
#' @return Named numeric vector with contribution of each layer to diversity
#'
#' @author Christian Rossi christian.rossi1990@gmail.com
#'
#' @usage typical usage is
#'
#' bcd(x)
#'
#' where x is a RasterStack containing a time series of one spectral information (e.g., NDVI) or
#' plant trait (e.g., specific leaf area).
#'
#' @references Rossi et al. (2020) Remote Sensing of Environment 236:111415
#' (\href{https://www.sciencedirect.com/science/article/pii/S0034425719304341}{ScienceDirect})
#'
#'
#' @examples #Dataset contribution to diversity
#' ex <-bcd(MTCI.stabelchod)
#'
#'
#'
bcd <- function(x, percentage = TRUE) {
  BCD <-
    cellStats((x - mean(cellStats(x, mean))) ^ 2, sum) / (cellStats(!is.na(x), sum)[1] *
                                                            dim(x)[3])


  if (percentage)
  {
    BCD <- BCD / sum(BCD) * 100

  } else{
  }

  return(BCD)

}
