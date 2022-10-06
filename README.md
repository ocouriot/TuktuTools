# TuktuTools
Package containing tools for studying Tuktu (caribou) spatial patterns.

This package contains functions to get and process data from MoveBank, and to clean the data:
- filter data to a specific/determined period of time
- filter data to a minimum number of fixes per day
- remove "outliers" fixes (i.e., fixes that are not biologicaly likely for the species).

This package also contains functions to analyze spatial patterns, such as functions to:
- identify population (herd) affiliation
- estimate individual movement rate
- estimate individual mean daily location (i.e., the mean x and y coordinates of all the daily locations for a given individual)
- estimate the ranges using two different methods (Local Convex Hulls (LoCoH) or Kernel Utilization Distributions (KUD))
- identify the calving status (non-calving or calving, calving with survival or calving with calf death), as well as calving timing and location for a given female
- estimate the calving ranges of a given population (herd)
- identify the individual day of arrival on the herd's calving range 
- estimate the daily pairwise distance or pairwise overlap between each pair of individuals.

The package also contains functions to visualize movement data:
- a function to vizualize individual paths through time
- a function to download the map of a defined area as a raster.


Finally, the package contains data for 4 individual caribou as well as functions examples.
