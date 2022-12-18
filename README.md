# TuktuTools

This package containing tools for studying Tuktu (caribou) movement ecology and spatial patterns. To install the current version of this package in R:

```
devtools::install_github("ocouriot/TuktuTools", build_vignettes = TRUE)
```

To date, this package contains sets of functions to process and filter data:

- process data from MoveBank
- filter data to a specific period of time
- filter data to a minimum number of fixes per day
- remove "outliers" fixes (i.e., fixes that are not biologicaly likely for the species)

To analyze spatial patterns, such as:
- identifying different populations (herds)
- computing individual movement rates, displacements, speeds
- estimate individual mean daily location (i.e., the mean x and y coordinates of all the daily locations for a given individual)
- estimating ranging areas with Local Convex Hulls (LoCoH) or Kernel Utilization Distributions (KUD))
- estimating daily pairwise distance or pairwise overlap between pairs of individuals
- identify calving status (non-calving or calving, calving with survival or calving with calf death), as well as calving timing and location for given females. 


The package also contains functions to visualize movement data:
- a function to visualize individual paths through time
- a function to download the map of a defined area as a raster

Finally, the package contains data for 4 individual caribou as well as a vignette (the first in a series), on estimating parturition dates. See

```
require(TuktuTools)
vignette("estimating_parturition")
```
