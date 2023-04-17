# TuktuTools <img src="man/figures/logo.svg" align="right" height="139" />

This package containing tools for studying Tuktu (caribou) movement ecology and spatial patterns. To install the current version of this package in R:

```
devtools::install_github("ocouriot/TuktuTools", build_vignettes = TRUE)
```

To date, this package contains sets of functions that prep and filter movement data:

- `process_moveNWT` - process data from Movebank;
- `removeoutliers` - flags fixes considered as *outliers*;
- `prepData` - prepares and filters data to a specific period of time and/or a minimum number of fixes per day; 
- `scan_tracks` - visualizes individual paths through time

The package also contains functions that analyze spatial patterns, including:
- `getSpeed` - computes individual movement rates, displacements, speeds;
- `getDailyMean` - estimates individual mean daily location (i.e., the mean x and y coordinates of all the daily locations for a given individual);
- `getLoCoH` and `getKernelUD` - estimate ranging areas with Local Convex Hulls (LoCoH) or Kernel Utilization Distributions (KUD));
- `getPairwiseDistance` and `getPairwiseOverlap` - estimate daily pairwise distance or pairwise overlap between pairs of individuals;
- `estimateCalving` - estimate calving status (non-calving or calving, calving with survival or calving with calf death), calving timing, and calving location for given females, using movement rate; 
- `estimateMigration_stan` - estimates population-level migration timing using STAN

The package contains the following data:

- `caribou` - anonymized movement data for 4 individual Barren-ground caribou;
- `simulated_migrations` - simulated movement tracks for 18 individuals

Finally, the package contains a vignette (the first in a series), on [estimating parturition dates](https://htmlpreview.github.io/?https://github.com/ocouriot/TuktuTools/blob/main/doc/estimating_parturition.html) or by running the following code: 

```
require(TuktuTools)
vignette("estimating_parturition")
```
