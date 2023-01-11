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

## ----removeOutliers, eval = TRUE----------------------------------------------
# load data
data(caribou)

# remove potential outliers and keep only females
caribou.cleaned <- removeOutliers(caribou) %>% subset(!outlier & sex == "f")

## ---- eval = FALSE------------------------------------------------------------
#  prepData(df, start, end, nfixes, dayloss)

## ----prepData, eval = TRUE----------------------------------------------------
caribou.prepped <- prepData(caribou.cleaned, 
                            start = "05-19", end = "07-07", 
                            nfixes = 1, dayloss = 3)

## ---- eval = FALSE------------------------------------------------------------
#  getSpeed(df)

## ----getSpeed, eval = TRUE----------------------------------------------------
caribou.mr <- getSpeed(caribou.prepped)
head(caribou.mr)

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  estimateCalving(df, int, kcons, models = c("full", "calfonly"), PlotIt = FALSE, saveplot = FALSE)

## ----estimateParturitions, eval = TRUE, fig.width = 8-------------------------
# Will generate a sample of two different individuals each time
part <- estimateCalving(df = caribou.mr, int=3, kcons=c(5,21), 
                        models = "calfonly", drawplot=TRUE, saveplot=FALSE)

## -----------------------------------------------------------------------------
part$statistics

## -----------------------------------------------------------------------------
part$par

## -----------------------------------------------------------------------------
part$results

## ---- eval = FALSE------------------------------------------------------------
#  require(TuktuTools)
#  data(caribou)
#  caribou_prepped <- caribou %>% removeOutliers %>%
#    prepData(start = "05-19", end = "07-07", nfixes = 1, dayloss = 3) %>% getSpeed
#  estimateCalving(caribou_prepped, int=3, kcons=c(5,21), models = "calfonly")

