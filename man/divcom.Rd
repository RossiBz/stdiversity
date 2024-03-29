% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/divcom.R
\name{divcom}
\alias{divcom}
\title{Total diversity and its components: layer (temporal), spatial and interaction term}
\usage{
divcom(x)
}
\arguments{
\item{x}{filename (character), Raster* object}
}
\value{
Named numeric vector with the contribution of each component (i.e., temporal, spatial, interaction and total)
}
\description{
divcom partitions the overall diversity in a temporal, spatial and interaction term as proposed by Rossi et al. (2021).
The diversity calculation is based on the sum of squares divided by the number of pixels and layers. The diversity accounts
for the pairwise dissimilarity between pixels belonging to the same layer (temporal information) as well as the dissimilarity between pixels
of different layers. All pixels are considered equally important.
}
\examples{
\dontrun{#Calculate total diversity and its components of a RasterStack
ex <-divcom(MTCI.stabelchod)
# MTCI.stabelchod is a RasterStack containing
# a time series of spectral information
}


}
\references{
Rossi, C., Kneubühler, M., Schütz, M., Schaepman, M.E, Haller, R.M., & Risch, A.C. (2021). Remote sensing of spectral diversity:
A new methodological approach to account for spatio-temporal dissimilarities between plant communities. Ecological Indicators, 130, 108106.
}
\author{
Christian Rossi christian.rossi1990@gmail.com
}
