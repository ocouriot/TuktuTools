#' Parturition model plot function
#'
#' Function to plot speed pattern of a female with the fitted values estimated by
#' the parturition.model given the chosen bestmodel.
#' among 'nocalf', 'calf', 'calfdeath'
#' See ?parturition.model for more information about the method
#'
#' The visualization portion of this code is adapted directly from the supplementary materials in Cameron et al. 2018.
#'
#' @param df a dataframe containing the speed between subsequent relocation of ONE individual,
#' obtained using `getSpeed`. 
#' See ?get.speed for more information on the Data requirements
#' @param int integer value indicating the minimum number of days between
#'     the beginning of the time series and the first BP (calf birth),
#'     the last BP (calf death) and the end of the time series,
#'     and between the two BPs.
#'     The main reason for this constraint is that a minimum number of data points are required in
#'     each section of the time-series to be able to estimate model parameters.
#'     We recomanded 9 relocations, thereby 3 days.
#' @param kcons vector of the minimum and maximum time it takes the female to recover normal speed (in days)
#' @param bestmodel the selected model by the parturition.model function, for this individual among: 'nocalf',
#' 'calf', 'calfdeath'
#'
#' @return The function returns a plot of the speed pattern and the fit of the model specified by bestmodel
#' as well as the plot of the track of the individual with the calving location (if bestmodel is equal to calf or calfdeath)
#'
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785
#' Cameron, M.D., Joly, K., Breed, G.A., et al. (2018). Movement-based methods to infer parturition events in migratory ungulates. Canadian Journal of Zoology. 96:1187–1195. https://doi.org/10.1139/cjz-2017-0314
#' 
#' @example examples/example_estimateCalving.R
#' 
#' @export

getCalvingPlot <- function (df, int, kcons, bestmodel) {


  # run the mnll3 for each individual to obtain MLE, AIC for the 3 models
    temp=droplevels(df)
    temp <- temp[order(temp$Time),]
    ID=unique(temp$ID)
    Year <- unique(temp$Year)
    temp$yday <- yday(temp$Time)
    speed <- na.omit(temp$speed)
    speedmean <- mean(speed)
    speedvar <- var(speed)

    # run the mnll3M function for the individual
    results <- mnll3M(temp, int, kcons)

    # get fitted values
    if (bestmodel == "nocalf"){
      beta.0 <- speedmean / speedvar
      alpha.0 <- speedmean * beta.0
      fit.values <- rep(alpha.0/beta.0, length(speed))
    }

    if (bestmodel == "calf"){
      fit <- nllCalf(temp, BP = results$BPs[["BP1.calf"]], k = kcons)
      fit.values <- fit$fit
    }

    if (bestmodel == "calfdeath"){
      fit <- nllCalfDeath(temp, BP1 = results$BPs[["BP1.calfdeath"]], BP2 = results$BPs[["BP2.calfdeath"]], k = kcons)
      fit.values <- fit$fit
    }


    ####### Making plots of results ##########
      temp <- droplevels(subset(temp, is.na(speed)==FALSE))
    if (bestmodel == "nocalf") {   #if the best model is the "didn't calve model", plot a flat line

        p1 <- ggplot(temp,aes(Time,speed, color = yday),show.legend = FALSE) +
          geom_line(show.legend = FALSE) + scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background = element_blank()) +  #Sets background to white
          geom_hline(aes(yintercept=fit.values, group=1), colour= 'darkblue', show.legend=FALSE, size=1) +
          labs(x = "Date", y = "Speed (m.h-1)", title = paste0("No calf model for: ", ID , ' in ', Year)) +
          theme(axis.line.x = element_line(size = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y = element_line(size = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title = element_text(size = 16,face = "bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500))

        # plot the individual track
        p2 <- ggplot(temp,aes(x,y, color = yday)) +
          geom_path(show.legend = FALSE) + coord_fixed() +
          scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background=element_blank()) +  #Sets background to white
          labs(x = "X coordinates",
               y = "Y coordinates") +
          theme(axis.line.x=element_line(size=.5,colour = "darkgrey",linetype = "solid")) +
          theme(axis.line.y=element_line(size=.5,colour = "darkgrey",linetype = "solid")) #add axis lines
      } # END plot of the no calf model


      if (bestmodel == "calf"){   #if the best model is the "calf model", plot a single break point
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calf"]])

        ## Plotting ##
        p1 <- ggplot(temp,aes(Time,speed, color = yday),show.legend = FALSE) +
          geom_line(show.legend = FALSE) + scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+1*24*3600),label=as.Date(calve),y=1500),angle=90,size=3,fontface="italic", colour = 'black') + #Labels the calving line with calving date
          labs(x="Date",y="Speed (m.h-1)",title=paste0("Calf model for: ", ID , ' in ', Year)) +
          theme(axis.line.x=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=16,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(x=as.POSIXct(Time), y=fit.values, group=1), colour = 'darkblue', show.legend=FALSE, size=1) #plots predicted values

        # plot the individual track
        p2 <- ggplot(temp,aes(x,y, color = yday)) +
          geom_path(show.legend = FALSE) + coord_fixed() +
          scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_point(aes(x = na.omit(temp[temp$Time == calve,]$x),
                         y = na.omit(temp[temp$Time == calve,]$y)),
                     shape=24, fill = 'red', color = 'red', size = 2) + # point at calving event
          labs(x = "X coordinates",
               y = "Y coordinates") +
          theme(axis.line.x=element_line(size=.5,colour = "darkgrey",linetype = "solid")) +
          theme(axis.line.y=element_line(size=.5,colour = "darkgrey",linetype = "solid")) #add axis lines
      } # END plot of the calf model


      if (bestmodel == "calfdeath"){  #if the best model is the "calved then calf lost" model, plot 2 breakpoints
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calfdeath"]])
        calf.loss=as.POSIXct(results$BPs[["date.BP2.calfdeath"]])

        ## Plotting ##
        p1 <- ggplot(temp,aes(Time,speed,color = yday)) +
          geom_line(show.legend = FALSE) +
          scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+1*24*3600),label=as.Date(calve),y=1500),angle=90,size=3,fontface="italic", colour = 'black') + #Labels the calving line with calving date
          geom_vline(xintercept=as.numeric(calf.loss),linetype=4,colour="black") + #break point at calf loss event
          geom_text(aes(x=(calf.loss-1*24*3600),label=as.Date(calf.loss),y=1500),angle=90,size=3,fontface="italic", colour = 'black') + #Labels calf loss
          labs(x="Date",y="Speed (m.h-1)",title=paste0("Calf death model for: ", ID , ' in ', Year)) +
          theme(axis.line.x=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=16,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(as.POSIXct(Time), fit.values, group=1), colour= 'darkblue', show.legend=FALSE, size=1) #plots predicted values

        # plot the individual track
        p2 <- ggplot(temp,aes(x,y, color = yday)) +
          geom_path(show.legend = FALSE) + coord_fixed() +
          scale_color_gradientn(colours = rainbow(5)) +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_point(aes(x = na.omit(temp[temp$Time == calve,]$x),
                         y = na.omit(temp[temp$Time == calve,]$y)),
                     shape=24, fill = 'red', color = 'red', size = 2) + # point at calving event
          geom_point(aes(x = na.omit(temp[temp$Time == calf.loss,]$x),
                         y = na.omit(temp[temp$Time == calf.loss,]$y)),
                     shape=25, fill = 'black', color = 'black', size = 2) + # point at calf death event
          labs(x = "X coordinates",
               y = "Y coordinates") +
          theme(axis.line.x=element_line(size=.5,colour = "darkgrey",linetype = "solid")) +
          theme(axis.line.y=element_line(size=.5,colour = "darkgrey",linetype = "solid")) #add axis lines
      } # end if calf death statement

    ggpubr::ggarrange(p1, p2, ncol = 2)

}
