## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE, echo = FALSE,
                      rmarkdown.html_vignette.check_title = FALSE)

## ----LoadLibraries, echo = FALSE, eval = TRUE, results = 'hide'---------------
require(TuktuTools)
require(kableExtra)
require(ggplot2)
require(ggthemes)
require(ggpubr)
require(ggrepel)
require(mapview)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  removeOutliers(df, steps = 10)

## ----removeOutliers, echo = TRUE, eval = TRUE---------------------------------
# load data
data(caribou)

# remove potential outliers and keep only females
caribou.cleaned <- removeOutliers(caribou) %>% subset(outlier == "FALSE" & sex == "f")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  prepData(df, start, end, nfixes, dayloss)

## ----prepData, echo = TRUE, eval = TRUE---------------------------------------
caribou.prepped <- prepData(caribou.cleaned, start = "05-19", end = "07-07", nfixes = 1, dayloss = 3)


## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  getSpeed(x)

## ----getSpeed, echo = TRUE, eval = TRUE---------------------------------------
# get movement rate for the females
caribou.mr <- getSpeed(caribou.prepped)

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  estimateCalving(df, int, kcons, models = c("full", "calfonly"), PlotIt = FALSE, saveplot = FALSE)

## ----estimateParturitions, eval = TRUE, echo = TRUE---------------------------
# Will generate a sample of two different individuals each time
part <- estimateCalving(df = caribou.mr, int=3, kcons=c(5,21), models = "calfonly", PlotIt=TRUE, saveplot=FALSE)

## ----ShowParturitionResults---------------------------------------------------
# Coefficients
kable_styling(kable(head(part$coeffs)))

# Parameters
kable_styling(kable(head(part$par)))

# results
kable_styling(kable(head(part$results)))

