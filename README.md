# stdiversity

## R package to calculate spatial temporal diversity from raster data 

The package allows to calculate spectral/functional diversity from multi-temporal raster 
files and the contribution of different components as proposed by Rossi et al. (2020). The diversity calculation is based on 
the sum of square divided by the number of pixels and layers. The diversity accounts for
for the pairwise dissimilarity between pixels belonging to the same layer as well as the dissimilarity between pixels
of different layers. All pixels are considered equally important.
A map with the contribution of each pixel to diversity can be created. 

## Main features

* Total diversity and its components: spatial, layer (temporal) and interaction term
* Layer contribution to diversity
* Pixel contribution to diversity

## Installation

To load (using `devtools`):

```Rscript
library(devtools)
install_github("RossiBz/stdiversity")
```

## Test dataset

MTCI (MERIS terrestrial chlorophyll index) time series of Stabelchod in the Swiss National Park:

```Rscript
MTCI.stabelchod
```
MTCI was calculated from eleven atmospherically
corrected Sentinel-2 images from summer 2017 and 2018. 
The extent of the raster is constrained to the meadow in Stabelchod and the surrounding forest.

## Authors

* Christian Rossi

## License

Licensed under the GNU General Public License, Version 3.0: https://www.gnu.org/licenses/gpl-3.0.html
