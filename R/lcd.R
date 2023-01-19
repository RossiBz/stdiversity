
#' Local contribution of each pixel to diversity
#'
#' lcd computes the contribution of each pixel to the overall diversity as proposed by Rossi et al. (2021). The diversity
#' calculation is based on the sum of squares divided by the number of pixels and layers. The diversity accounts for the pairwise
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
#' @references Rossi, C., Kneubühler, M., Schütz, M., Schaepman, M.E, Haller, R.M., & Risch, A.C. (2021). Remote sensing of spectral diversity: 
#' A new methodological approach to account for spatio-temporal dissimilarities between plant communities. Ecological Indicators, 130, 108106.
#' (\href{https://doi.org/10.1016/j.ecolind.2021.108106})
#'
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


lcd <- function(x, percentage = TRUE) {
    
    if (!inherits(x, "RasterLayer")) {
    stop("Input must be a RasterLayer object.")
    }
    
    LCD <-
        raster::calc((x - mean(raster::cellStats(x, mean))) ^ 2, fun = sum) / (raster::cellStats(!is.na(x), sum)[1] * #number of pixels
                                                                   dim(x)[3]) ##number of layers


    if (percentage)
    {
        LCD <- LCD / raster::cellStats(LCD, sum) * 100

    } else{
    }

    return(LCD)

}









