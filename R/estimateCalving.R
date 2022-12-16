#' Parturition timing function
#'
#' Function that determine calving status of a female (i.e. no calf, with a calf, calf lost)
#' the calving date and the calf death date (if any).
#' This is an adaptation of the individual based method developed by Demars et al. (2013),
#' which has proven good reliability to estimate calving status and calving date for females
#' from the Western Arctic Herd in a previous study (Cameron et al. 2018).
#' However, we adapted this method to be able to infer parturition based on the 
#' female movement rate through time.
#'
#'
#' All models assume speed follows a Gamma distribution and differ in two parameters: shape
#' and scale, which correspond to \eqn{mean(speed)^2 / var(speed)}
#' and \eqn{var(speed) / mean(speed)}, respectively.
#' The mean speed is thus equal to \eqn{shape * scale}.
#' \itemize{
#' \item For the model representing females that do not calve: the mean speed remains constant
#' through the entire calving period.
#' \item For the model representing females that had a calf who survived 4 weeks after birth:
#' the mean speed is constant before calving, then abruptly drops for calving, creating a break point.
#' After calving, the mean speed increases progressively following:
#' \eqn{ (shape.calving * (mean(shape) - shape.calving) / k * time) * (scale.calving * (mean(scale) - scale.calving) / k) * time)}
#' where k is the time, defined in days, required for the calf to achieve adult movement rates.
#' \item For the model representing females losing calves: there is an abrupt change in the slope of
#' the post-calving increase, creating a second break point after which the mean speed immediately
#' recovers its pre-calving value.}
#' The models therefore differ in their number of parameters to estimate:
#' the no calf model has two – shape and scale;
#' the calf model has four – shape, scale, k and calving date;
#' and the calf death model has five – shape, scale, k, calving and calf death.
#' We discriminated among models using Akaike’s Information Criterion (AIC)
#' with the best model being the one with the lowest AIC value.
#'
#' The visualization portion of this code is adapted directly from Matt Cameron and supplementary materials in Cameron et al. 2018.
#'
#' @param df a dataframe containing the movement rate between subsequent 
#' relocation of individuals, as obtained using `getSpeed`.
#' See ?get.speed for more information on the Data requirements
#' @param int integer value indicating the minimum number of days between
#'     the beginning of the time series and the first BP (calf birth),
#'     the last BP (calf death) and the end of the time series,
#'     and between the two BPs.
#'     The main reason for this constraint is that a minimum number of data points are required in
#'     each section of the time-series to be able to estimate model parameters.
#'     We recommended 9 relocations, thereby 3 days.
#' @param kcons vector of the minimum and maximum time it takes the female to recover normal speed (in days)
#' @param models either "full" to fit all three models (i.e., no calf, calf and calf death models), or 
#' "calfonly" to fit only the no calf and calf models.
#' @param PlotIt if TRUE, the function will draw a plot of the movement rate in function to the date
#' with the prediction line of the best model selected by AIC
#' @param saveplot if TRUE, the plot of the best model will be 
#' @param dir the directory used to save the plot (e.g., "C://Users/Documents/")
#'
#' @return returns list of 3 dataframes:
#' `coeffs` containing the best model for each female, selected based on AIC, the AIC of the 3 models 
#' (i.e. no calf model, calf model and calf death model) and the negative likelihood 
#' of each model.
#' `Par` containing the estimated parameters of all the models (0: no calf, 1: calf, 2: calf death)
#' `results` containing the best model, for each female, based on AIC,
#'  the calving timing and location (if the calf model is the best model),
#' and the mortality timing and location of the calf (if the calf death is the best model).
#'
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785
#' Cameron, M.D., Joly, K., Breed, G.A., et al. (2018). Movement-based methods to infer parturition events in migratory ungulates. Canadian Journal of Zoology. 96:1187–1195. https://doi.org/10.1139/cjz-2017-0314
#' 
#' @example examples/example_estimateCalving.R
#' 
#' @export

estimateCalving <- function (df, int, kcons, models = c("full","calfonly"),
                               PlotIt = FALSE, saveplot = FALSE) {
  
  df2 <- df %>% as.data.frame %>% mutate(ID_Year = as.factor(paste(ID, year(Time), sep = "_")))
  
  # create the returned dataframes
  coeffs <- data.frame()
  par <- data.frame()
  results.summary <- data.frame()
  
  # run the mnll3 for each individual to obtain MLE, AIC for the 3 models
  for(i in unique(df2$ID_Year))  {
    print(i)
    
    temp=droplevels(subset(df2,ID_Year==i))
    temp <- temp[order(temp$Time),]
    ID_Year <- unique(temp$ID_Year)
    ID <- unique(temp$ID)
    Year <- unique(year(temp$Time))
    
    speed <- na.omit(temp$speed)
    speedmean <- mean(speed)
    speedvar <- var(speed)
    
    # run the mnll3M or mnll2M function for the individual
    if(models == "full"){
      results <- mnll3M(temp, int, kcons)
      # exctract the parameters, AICs...
      results.data.temp=data.frame(ID_Year = as.factor(as.character(ID_Year)), ID=ID, Year = Year,
                                   Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                   M0.AIC=as.numeric(results$results[1,"AIC.nocalf"]),
                                   Mcalf.AIC=as.numeric(results$results[1,"AIC.calf"]),
                                   Mcalfdeath.AIC=as.numeric(results$results[1,"AIC.calfdeath"]),
                                   M0.mnll=as.numeric(results$results[1,"mnllNoCalf"]),
                                   Mcalf.mnll=as.numeric(results$results[1,"mnllCalf"]),
                                   Mcaldeath.mnll=as.numeric(results$results[1,"mnllCalfDeath"]))
      
      
      ## results table
      results.summary.temp <- data.frame(ID=ID, Year = Year, Best.Model=results.data.temp$Best.Model,
                                         calving.date=ifelse(as.character(results$results[1,"Best.Model"]) == "calf",
                                                             as.POSIXct(results$BPs[["date.BP1.calf"]]),
                                                             ifelse(as.character(results$results[1,"Best.Model"]) == "calfdeath",
                                                                    as.POSIXct(results$BPs[["date.BP1.calfdeath"]]),NA)),
                                         mort.date=ifelse(as.character(results$results[1,"Best.Model"])=="calfdeath",
                                                          as.POSIXct(results$BPs[["date.BP2.calfdeath"]]),NA),
                                         Recovery=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                         results$BPs$recovery.calf,NA))
      results.summary.temp$calving.date <- as.POSIXct(results.summary.temp$calving.date,
                                                      origin="1970-01-01", tz="GMT")
      results.summary.temp$mort.date <- as.POSIXct(results.summary.temp$mort.date,
                                                   origin="1970-01-01", tz="GMT")
      calving.date.julian <- yday(results.summary.temp$calving.date)
      
      # Estimated parameters (alpha.mean, beta.mean, alpha.calf, beta.calf and recovery time)
      
      {
        beta.0 <- speedmean / speedvar
        alpha.0 <- speedmean * beta.0
        parnocalf <- data.frame(ID = ID, Year = Year, alpha.0 = alpha.0, beta.0 = beta.0)
        fit.values.nocalf <- rep(alpha.0/beta.0, length(speed))
        results.summary.temp$calf.loc.x <- NA
        results.summary.temp$calf.loc.y <- NA
      }
      
      
      {
        fit.calf <- try(nllCalf(temp, BP = results$BPs[["BP1.calf"]], k = kcons), silent = TRUE)
        if(!inherits(fit.calf, 'try-error')){
          parcalf <- data.frame(alpha.mean1=fit.calf$par[["alpha.mean"]], beta.mean1=fit.calf$par[["beta.mean"]],
                                alpha.calf1=fit.calf$par[["alpha.calf"]], beta.calf1=exp(fit.calf$par[["log.beta.calf"]]),
                                BP.calf1 = results$BPs[["BP1.calf"]], calving.date1 = results$BPs[["date.BP1.calf"]], 
                                recovery1=fit.calf$par[["recovery"]])
          fit.values.calf <- fit.calf$fit } 
        if (inherits(fit.calf, 'try-error')) {
          parcalf <- data.frame(alpha.mean1=NA, beta.mean1=NA,
                                alpha.calf1=NA, beta.calf1=NA,
                                BP.calf1 = NA, calving.date1 = NA, 
                                recovery1=NA)
        }
      }
      if(results.summary.temp$Best.Model == 'calf'){
        results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("x")][1]
        results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("y")][1]
      }
      
      
      
      {
        fit.calfdeath <- try(nllCalfDeath(temp, BP1 = results$BPs[["BP1.calfdeath"]], BP2 = results$BPs[["BP2.calfdeath"]], k = kcons), silent = TRUE)
        if(!inherits(fit.calfdeath, 'try-error')){
          parcalfdeath <- data.frame(alpha.mean2=fit.calfdeath$par[["alpha.mean"]], beta.mean2=fit.calfdeath$par[["beta.mean"]],
                                     alpha.calf2=fit.calfdeath$par[["alpha.calf"]], beta.calf2=exp(fit.calfdeath$par[["log.beta.calf"]]),
                                     BP.calf2 = results$BPs[["BP1.calfdeath"]], calving.date2 = results$BPs[["date.BP1.calfdeath"]], 
                                     BP.calfdeath2 = results$BPs[["BP2.calfdeath"]], calfdeath.date2 = results$BPs[["date.BP2.calfdeath"]], 
                                     recovery2=fit.calfdeath$par[["recovery"]])
          fit.values.calfdeath <- fit.calfdeath$fit } 
        if (inherits(fit.calfdeath, "try-error")){
          parcalfdeath <- data.frame(alpha.mean2=NA, beta.mean2=NA,
                                     alpha.calf2=NA, beta.calf2=NA,
                                     BP.calf2 = NA, calving.date2 = NA, 
                                     BP.calfdeath2 = NA, calfdeath.date2 = NA, 
                                     recovery2=NA)
        }}
      
      if(results.summary.temp$Best.Model == 'calfdeath'){
        results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("x")][1]
        results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("y")][1]
      }
      
      par <- rbind(par, cbind(parnocalf, parcalf, parcalfdeath))
      coeffs <- rbind(coeffs,results.data.temp[1,])
      results.summary=rbind(results.summary,results.summary.temp)

    } else if(models == "calfonly"){
      # run the mnll2M function for the individual
      results <- mnll2M(temp, int, kcons)
      # exctract the parameters, AICs...
      results.data.temp=data.frame(ID_Year = as.factor(as.character(ID_Year)), ID=ID, Year = Year,
                                   Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                   M0.AIC=as.numeric(results$results[1,"AIC.nocalf"]),
                                   Mcalf.AIC=as.numeric(results$results[1,"AIC.calf"]),
                                   M0.mnll=as.numeric(results$results[1,"mnllNoCalf"]),
                                   Mcalf.mnll=as.numeric(results$results[1,"mnllCalf"]))
      
      ## results table
      results.summary.temp <- data.frame(ID=ID, Year = Year, Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                         calving.date=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                             as.POSIXct(results$BPs[["date.BP1.calf"]]), NA),
                                         Recovery=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                         results$BPs[["recovery.calf"]], NA))
      results.summary.temp$calving.date <- as.POSIXct(results.summary.temp$calving.date,
                                                      origin="1970-01-01", tz="GMT")
      calving.date.julian <- yday(results.summary.temp$calving.date)
      
      
      
      # Estimated parameters (alpha.mean, beta.mean, alpha.calf, beta.calf and recovery time)
      {
        beta.0 <- speedmean / speedvar
        alpha.0 <- speedmean * beta.0
        parnocalf <- data.frame(ID = ID, Year = Year, alpha.0 = alpha.0, beta.0 = beta.0)
        fit.values.nocalf <- rep(alpha.0/beta.0, length(speed))
        results.summary.temp$calf.loc.x <- NA
        results.summary.temp$calf.loc.y <- NA
      }
      
      
      {
        fit.calf <- try(nllCalf(temp, BP = results$BPs[["BP1.calf"]], k = kcons), silent = TRUE)
        if(!inherits(fit.calf, "try-error")){
          parcalf <- data.frame(alpha.mean1=fit.calf$par[["alpha.mean"]], beta.mean1=fit.calf$par[["beta.mean"]],
                                alpha.calf1=fit.calf$par[["alpha.calf"]], beta.calf1=exp(fit.calf$par[["log.beta.calf"]]),
                                BP.calf1 = results$BPs[["BP1.calf"]], calving.date1 = results$BPs[["date.BP1.calf"]], 
                                recovery1=fit.calf$par[["recovery"]])
          fit.values.calf <- fit.calf$fit
        }
        if(inherits(fit.calf, "try-error")){
          parcalf <- data.frame(alpha.mean1=NA, beta.mean1=NA,
                                alpha.calf1=NA, beta.calf1=NA,
                                BP.calf1 = NA, calving.date1 = NA, 
                                recovery1=NA)
        }
        
        results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("x")][1]
        results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                c("y")][1]
      }
      
      par <- rbind(par, cbind(parnocalf, parcalf))
      coeffs <- rbind(coeffs,results.data.temp[1,])
      results.summary=rbind(results.summary,results.summary.temp)
    }
   
    
    ####### if PlotAIC = TRUE statement plots
    ####### Making plots of results ##########
    temp <- droplevels(subset(temp, is.na(speed)==FALSE))
    temp <- temp[order(temp$Time),]
    if(PlotIt){
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "nocalf") {   #if the best model is the "didn't calve model", plot a flat line
        
        p1 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background = element_blank()) +  #Sets background to white
          geom_hline(aes(yintercept=fit.values.nocalf, group=1, colour=2), show.legend=FALSE, size=1) +
          labs(x = "Date", y = "Speed (m.h-1)", title = paste("No calf model for",ID_Year, sep = " ")) +
          theme(axis.line.x = element_line(size = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y = element_line(size = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title = element_text(size = 12,face = "bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500))
        
        if(saveplot)
          ggsave(plot = p1, filename=paste0(paste(ID_Year,"no_calving.jpeg",sep="_")), 
                 width = 8,height = 4,device="jpg") else print(p1)
        
      } # END plot of the no calf model
      
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "calf"){   #if the best model is the "calf model", plot a single break point
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calf"]])
        
        ## Plotting ##
        p2 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+2*24*3600),label=calve,y=1500),angle=90,size=4,fontface="italic") + #Labels the calving line with calving date
          labs(x="Date",y="Speed (m.h-1)",title=paste("Calf model for",ID_Year,sep = " " )) +
          theme(axis.line.x=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=12,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(as.POSIXct(Time), fit.values.calf, colour=1, group=1), show.legend=FALSE, size=1) #plots predicted values
        if(saveplot)
          ggsave(plot= p2, filename=paste0(dir, paste(ID_Year,"Calved.jpeg",sep="_")), 
                 width = 8,height = 4,device="jpg") else print(p2)
      } # END plot of the calf model
      
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "calfdeath"){  #if the best model is the "calved then calf lost" model, plot 2 breakpoints
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calfdeath"]])
        calf.loss=as.POSIXct(results$BPs[["date.BP2.calfdeath"]])
        
        ## Plotting ##
        p3 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+2*24*3600),label=calve,y=1500),angle=90,size=4,fontface="italic") + #Labels the calving line with calving date
          geom_vline(xintercept=as.numeric(calf.loss),linetype=4,colour="black") + #break point at calf loss event
          geom_text(aes(x=(calf.loss-2*24*3600),label=calf.loss,y=1500),angle=90,size=4,fontface="italic") + #Labels calf loss
          labs(x="Date",y="Speed (m.h-1)",title=paste("Calf death model for",ID_Year,sep = " " )) +
          theme(axis.line.x=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(size=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=12,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(as.POSIXct(Time), fit.values.calfdeath, colour=1, group=1), show.legend=FALSE, size=1) #plots predicted values
        if(saveplot)
          ggsave(plot = p3, filename=paste0(dir, paste(ID_Year,"Calf_loss.jpeg",sep="_")),
                 width = 8,height = 4,device="jpg") else print(p3)
      } # End Plot of the calf death model
      
    } # End if PlotIt == T
    
  } # END for loop for each individual

  return(list(coeffs = coeffs, par = par, results = results.summary))
  
}
