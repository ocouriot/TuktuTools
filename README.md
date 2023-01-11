# TuktuTools

This package containing tools for studying Tuktu (caribou) movement ecology and spatial patterns. To install the current version of this package in R:

```
devtools::install_github("ocouriot/TuktuTools", build_vignettes = TRUE)
```

To date, this package contains sets of functions that process, prep, filter and movement data by:

- processing data from MoveBank; 
- filtering data to a specific period of time and/or a minimum number of fixes per day; 
- removing outliers;
- `scan_tracks` - visualizes individual paths through time

The package also contains functinos that analyze spatial patterns, including:
- identifying different populations (herds);
- computing individual movement rates, displacements, speeds;
- `getDailyMean` - estimates individual mean daily location (i.e., the mean x and y coordinates of all the daily locations for a given individual);
- estimating ranging areas with Local Convex Hulls (LoCoH) or Kernel Utilization Distributions (KUD));
- estimating daily pairwise distance or pairwise overlap between pairs of individuals;
- identifying calving status (non-calving or calving, calving with survival or calving with calf death), calving timing, and calving location for given females (see vignette link below); 
- identifying calving ranges from herd-level statistics.  

Finally, the package contains anonymized data for 4 individual caribou as well as a vignette (the first in a series), on [estimating parturition dates](https://htmlpreview.github.io/?https://github.com/ocouriot/TuktuTools/blob/main/doc/estimating_parturition.html). 

```
require(TuktuTools)
vignette("estimating_parturition")
```
