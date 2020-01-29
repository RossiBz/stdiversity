
#' Local contribution of each pixel to diversity
#'
#' lcd computes the contribution of each pixel to the overall diversity as proposed by Rossi et al. (2020). The diversity
#' calculation is based on the sum of square divided by the number of pixels and layers. The diversity accounts for the pairwise
#' dissimilarity between pixels belonging to the same layer as well as the dissimilarity between pixels of
#' different layers. All pixels are considered equally important.
#'
#' @param x filename (character), Raster* object
#' @param percentage logical. If TRUE the contribution of each pixel is expressed as percentage.
#'
#' @return Raster* object with local contribution of each pixel to diversity
#'
#' @author Christian Rossi christian.rossi1990@gmail.com
#'
#' @import raster
#' @import rgdal
#'
#'
#' @usage lcd(x, percentage=TRUE)
#'
#' @references Rossi et al. (2020) Remote Sensing of Environment 236:111415
#' (\href{https://www.sciencedirect.com/science/article/pii/S0034425719304341}{ScienceDirect})
#'
#'
#' @examples \dontrun{#plot pixel contribution to diversity
#' plot(lcd(MTCI.stabelchod))
#' # MTCI.stabelchod is a RasterStack containing
#' # a time series of spectral information
#' }
#'
#' @export
#'

library(raster)

lcd <- function(x, percentage = TRUE) {
    LCD <-
        calc((x - mean(cellStats(x, mean))) ^ 2, fun = sum) / (cellStats(!is.na(x), sum)[1] * #number of pixels
                                                                   dim(x)[3]) ##number of layers


    if (percentage)
    {
        LCD <- LCD / cellStats(LCD, sum) * 100

    } else{
    }

    return(LCD)

}









