## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = TRUE, cache  = FALSE,  rmarkdown.html_vignette.check_title = FALSE)

## ----LoadLibraries, echo = FALSE, eval = TRUE, results = 'hide'---------------
require(TuktuTools)
require(kableExtra)
require(ggplot2)
require(ggthemes)
require(ggpubr)
require(ggrepel)
require(mapview)

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("https://github.com/ocouriot/TuktuTools")
#  require(TuktuTools)

## -----------------------------------------------------------------------------
data(caribou)
head(caribou)

## ---- eval = FALSE------------------------------------------------------------
#  removeOutliers(df, steps = 10)

