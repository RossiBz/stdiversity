
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
#' @references Rossi, C., Kneubühler, M., Haller, R., Schaepman, M., Schütz, M., & Risch, A. (2020). Contemplating spatial and temporal
#' components of functional diversity: Full exploitation of satellite data for biodiversity monitoring. Earth and Space Science
#' Open Archive.(\href{https://www.essoar.org/doi/abs/10.1002/essoar.10501762.1})
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









