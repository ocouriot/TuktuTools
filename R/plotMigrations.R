#' Plotting migrations
#'
#' @export
#' 

ggTracks <- function(my.df){
  p.xy <- ggplot(my.df, aes(x, y, col=nickname)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.8) + theme_few() + coord_fixed() + scale_colour_hue(l = 40)  + ggtitle(paste(my.df$study[1], my.df$year[1]))
  p.tx <- ggplot(my.df, aes(yday, x, col=nickname)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40)
  p.ty <- ggplot(my.df, aes(yday, y, col=nickname)) +  geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40)
  
  grid.arrange(p.xy, p.tx, p.ty, layout_matrix = cbind(c(1,1), c(1,1), 2:3))
}


plotFits <- function(myfit, title = myfit$my.df$year[1], fit = TRUE, density=TRUE, chains = TRUE){
  require(rstan)
  require(coda)
  require(mixtools)
  require(gplots)
  require(lattice)
  
  migration.fit <- myfit$migration.fit
  my.data <- myfit$my.data
  
  # diagnose chains for each year
  
  phats <- summary(As.mcmc.list(migration.fit))$quantiles[,"50%"]
  
  Mus <- phats[grep("mean", names(phats))]
  sds <- phats[grep("sd", names(phats))]
  Sigmas <- phats[grepl("Sigma", names(phats)) & grepl("mu", names(phats))] %>% as.list
  names(Sigmas) <- c("S1.11","S1.12","S1.21","S1.22",
                     "S2.11","S2.12","S2.21","S2.22")
  Mu1 <- Mus[c(1,3)]
  Mu2 <- Mus[c(2,4)]
  S1 <- with(Sigmas, matrix(c(S1.11, S1.12, S1.21, S1.22), nrow = 2))
  S2 <- with(Sigmas, matrix(c(S2.11, S2.12, S2.21, S2.22), nrow = 2))
  Area1.95 <- ellipse(Mu1, S1, draw = FALSE)
  Area2.95 <- ellipse(Mu2, S2, draw = FALSE)
  Area1.50 <- ellipse(Mu1, S1, alpha = 0.5, draw = FALSE)
  Area2.50 <- ellipse(Mu2, S2, alpha = 0.5, draw = FALSE)
  mux1s <- phats[grepl("mux1", names(phats))]
  muy1s <- phats[grepl("muy1", names(phats))]
  mux2s <- phats[grepl("mux2", names(phats))]
  muy2s <- phats[grepl("muy2", names(phats))]
  
  n.ind <- max(my.data$id)
  palette(rich.colors(n.ind))
  
  # plot this puppy!
  
  if(fit){
    layout(rbind(c(1, 2), c(1, 3)))
    par(mar = c(0, 4, 0, 0), oma = c(4, 0, 4, 4), mgp = c(2,.5,0), cex.lab = 1.25, tck = 0.01, xpd=NA)
    
    with(my.data, plot(x,y,asp=1, col=alpha(id,.8), pch = 19, cex=0.5))
    #mtext(title, side = 3, line = 1, cex = 1.5)
    title(title, outer = TRUE, cex=2)
    dlply(with(my.data, data.frame(x,y,id)), "id", 
          function(df) with(df, lines(x,y,col=alpha(id, .8))))
    polygon(Area1.95, col =rgb(.7,.7,1,.4), bor = NA)
    polygon(Area2.95, col =rgb(.7,1,.7,.4), bor = NA)		
    polygon(Area1.50, col =rgb(.7,.7,1,.4), bor = NA)
    polygon(Area2.50, col =rgb(.7,1,.7,.4), bor = NA)		
    
    points(mux1s, muy1s, col = 1:n.ind, cex=1.5, pch=3, lwd=2)
    points(mux2s, muy2s, col = 1:n.ind, cex=1.5, pch=3, lwd=2)
    
    
    yday.model <- with(my.data, c(min(yday), phats["t_mean"] + c(0, phats["dt_mean"]), max(yday)))
    y.model <- with(my.data, rep(phats[c("muy_mean[1]", "muy_mean[2]")], each = 2))
    x.model <- with(my.data, rep(phats[c("mux_mean[1]", "mux_mean[2]")], each = 2))
    
    with(my.data, plot(yday, y, col="darkgrey", pch = 19, cex=0.5, xaxt="n", xlab=""))
    dlply(with(my.data, data.frame(yday,y,id)), "id", 
          function(df) with(df, lines(yday,y,col="grey")))
    lines(yday.model, y.model, col="darkred", lwd = 4)
    
    with(my.data, plot(yday, x, col="darkgrey", pch = 19, cex=0.5))
    dlply(with(my.data, data.frame(yday,x,id)), "id", 
          function(df) with(df, lines(yday,x,col="grey")))
    lines(yday.model, x.model, col="darkred", lwd = 4)
  }
  # diagnostic plots
  
  pars <-  c("mux_mean[1]", "muy_mean[1]", "mux1_sigma", "muy1_sigma", "rho1",
             "mux_mean[2]", "muy_mean[2]", "mux2_sigma", "muy2_sigma", "rho2",
             "t_mean", "t_sd", "dt_mean", "dt_sd")
  
  migration.chains <- As.mcmc.list(migration.fit, pars)	
  if(density) print(densityplot(migration.chains, layout = c(5,3)))
  if(chains) print(xyplot(migration.chains, layout = c(2,7)))
}		


scan.year <- function(my.df, title = my.df$year[1], lines = TRUE){
  require(ggthemes)
  require(gridExtra)
  
  p.xy <- ggplot(my.df, aes(x, y, col=nickname)) + geom_path(alpha = 0.5) + 
    geom_point(alpha = 0.5, size = 0.8) + theme_few() + coord_fixed() + 
    ggtitle(title) + scale_colour_hue(l = 40)
  p.tx <- ggplot(my.df, aes(yday, x, col=nickname)) + geom_path(alpha = 0.5) + 
    geom_point(alpha = 0.5, size = 0.5) + theme_few() + 
    theme(legend.position="none") + scale_colour_hue(l = 40)
  p.ty <- ggplot(my.df, aes(yday, y, col=nickname)) +  geom_path(alpha = 0.5) + 
    geom_point(alpha = 0.5, size = 0.5) + theme_few() + 
    theme(legend.position="none") + scale_colour_hue(l = 40)
  
  if(!lines){
    p.xy <- remove_geom(p.xy, "GeomPath")
    p.tx <- remove_geom(p.tx, "GeomPath")
    p.ty <- remove_geom(p.ty, "GeomPath")
  }
  
  grid.arrange(p.xy, p.tx, p.ty, layout_matrix = cbind(c(1,1), 2:3))
}

mapFits <- function(myfit, bathurst.map){
  require(coda)
  require(mixtools)
  require(gplots)
  require(lattice)
  
  migration.fit <- myfit$migration.fit
  my.data <- myfit$my.data
  my.df <- myfit$my.df
  
  myyear <- my.df$year[1]
  
  # diagnose chains for each year
  
  phats <- summary(As.mcmc.list(migration.fit))$quantiles[,"50%"]
  
  Mus <- phats[grep("mean", names(phats))]
  sds <- phats[grep("sd", names(phats))]
  Sigmas <- phats[grepl("Sigma", names(phats)) & grepl("mu", names(phats))] %>% as.list
  names(Sigmas) <- c("S1.11","S1.12","S1.21","S1.22",
                     "S2.11","S2.12","S2.21","S2.22")
  Mu1 <- Mus[c(1,3)]
  Mu2 <- Mus[c(2,4)]
  S1 <- with(Sigmas, matrix(c(S1.11, S1.12, S1.21, S1.22), nrow = 2))
  S2 <- with(Sigmas, matrix(c(S2.11, S2.12, S2.21, S2.22), nrow = 2))
  Area1.95 <- ellipse(Mu1, S1, draw = FALSE)
  Area2.95 <- ellipse(Mu2, S2, draw = FALSE)
  Area1.50 <- ellipse(Mu1, S1, alpha = 0.5, draw = FALSE)
  Area2.50 <- ellipse(Mu2, S2, alpha = 0.5, draw = FALSE)
  mux1s <- phats[grepl("mux1", names(phats))]
  muy1s <- phats[grepl("muy1", names(phats))]
  mux2s <- phats[grepl("mux2", names(phats))]
  muy2s <- phats[grepl("muy2", names(phats))]
  
  # convert all of these to lat-long
  
  ll.lm <- lm(cbind(lon, lat) ~ poly(x,2)*poly(y,2), data = my.df) 
  
  xy.results <- list(Mus = rbind(Mu1, Mu2), Area1.95, Area2.95, Area1.50, Area2.50)
  ll.results <- lapply(xy.results, function(l) predict(ll.lm, newdata = data.frame(x = l[,1], y = l[,2]))) %>% lapply(as.data.frame)	
  names(ll.results) <- c("Mus", "Area1.95", "Area2.95", "Area1.50", "Area2.50")	
  
  require(ggmap)
  #bathurst.map <- get_map(c(-115,65), source="google", maptype="terrain", zoom=5)
  #save(bathurst.map, file = "../results/bathurstggmap.rda")
  #load(file = "../results/bathurstggmap.rda")
  
  p.latlon <-	ggmap(bathurst.map) + geom_path(data = my.df, mapping = aes(x = lon, y = lat, col=factor(id))) + 
    coord_map(xlim = c(-125,-100), ylim = c(61, 68.5)) + labs(x = "Longitude", y = "Latitude") + scale_color_discrete(l = 50) + guides(col = FALSE) + 
    geom_polygon(data = ll.results$Area1.95, aes(x = lon, y = lat), fill = alpha("blue", .2), col="blue") + 
    geom_polygon(data = ll.results$Area1.50, aes(x = lon, y = lat), fill = alpha("blue", .2), col="blue") + 
    geom_polygon(data = ll.results$Area2.95, aes(x = lon, y = lat), fill = alpha("green", .2), col="green") + 
    geom_polygon(data = ll.results$Area2.50, aes(x = lon, y = lat), fill = alpha("green", .2), col="green") + 
    ggtitle(paste("Slave-Bathurst Caribou:", myyear))
  
  
  model.fit = data.frame(
    yday.model = with(my.data, c(min(yday), phats["t_mean"] + c(0, phats["dt_mean"]), max(yday))),
    lat.model = rep(ll.results$Mu[,2], each = 2),
    lon.model = rep(ll.results$Mu[,1], each = 2))
  
  p.tlon <- ggplot(my.df, aes(yday, lon, col=nickname)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40) + geom_path(data = model.fit, aes(yday.model, lon.model), col="darkred", alpha = 0.5,  lwd=2) + xlab("Day of year") + ylab("Longitude")
  
  p.tlat <- ggplot(my.df, aes(yday, lat, col=nickname)) + geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40) + geom_path(data = model.fit, aes(yday.model, lat.model), col="darkred", alpha = 0.5,  lwd=2) + xlab("Day of year") + ylab("Latitude")
  
  require(gridExtra)
  grid.arrange(p.latlon, p.tlat, p.tlon, layout_matrix = cbind(c(1,1), c(1,1),c(1,1), 2:3, 2:3))
  
}		

