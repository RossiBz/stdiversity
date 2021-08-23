

#' Layer contribution to diversity
#'
#' bcd computes the contribution of each layer to the overall diversity as proposed by Rossi et al. (2021). The diversity
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
#' @import raster
#' @import rgdal
#'
#' @usage bcd(x, percentage=TRUE)
#'
#' @references Rossi, C., Kneubühler, M., Haller, R., Schaepman, M., Schütz, M., & Risch, A. (2021). Remote sensing of spectral diversity: 
#' A new methodological approach to account for spatio-temporal dissimilarities between plant communities. Ecological Indicators, 130, 108106.
#' (\href{https://doi.org/10.1016/j.ecolind.2021.108106})
#'
#'
#' @examples \dontrun{#Dataset contribution to diversity
#' ex <-bcd(MTCI.stabelchod)
#' # MTCI.stabelchod is a RasterStack containing
#' # a time series of spectral information
#' }
#'
#' @export
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
