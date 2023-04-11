#' Hierarchical estimation of migration timing
#' 
#' 
#' 
#' @export
#' 
#' 

fitSpringMigration <- function(move_data, directory, 
                               filename = paste0("fit",myyear), 
                               chains = chains,
                               printpars = 0, 
                               printvals = 0, 
                               squish = 1, ...){
  
  stan_data <- with(move_data, list( n = length(x), k = length(unique(id)), 
                                     x = x, y = y, yday = yday,
                                     id = as.integer(factor(as.character(id))), 
                                     squish = squish))
  
  migration.fit <- try(sampling(springmigration_stan, stan_data, 
                                #init = inits, 
                                chains = chains, ...))
  
  myresults <- list(my.df = move_data, my.data = stan_data, migration.fit = migration.fit)
  save(myresults, file = paste0(directory, filename))
  return(myresults)
}

plotFits <- function(myfit, title = myfit$my.df$year[1], 
                     fit = TRUE, density=TRUE, chains = TRUE){
  require(rstan)
  require(gplots)
  require(mixtools)
  
  migration.fit <- myfit$migration.fit
  my.data <- myfit$my.data
  
  phats <- summary(migration.fit)$summary[,"50%"]
  Mus <- phats[grep("mean", names(phats))]
  sds <- phats[grep("sd", names(phats))]
  Sigmas <- phats[grepl("sigma", names(phats)) & grepl("mu", names(phats))]
  rhos <- phats[grep("rho", names(phats))]
  S1 <- matrix(c(Sigmas[1]^2, rhos[1]*Sigmas[1]*Sigmas[2],
                 rhos[1]*Sigmas[1]*Sigmas[2], Sigmas[2]^2), 2, 2)
  S2 <- matrix(c(Sigmas[3]^2, rhos[2]*Sigmas[3]*Sigmas[4],
                 rhos[2]*Sigmas[3]*Sigmas[4], Sigmas[4]^2), 2, 2)
  Mu1 <- Mus[grepl("[1]", names(Mus))]
  Mu2 <- Mus[grepl("[2]", names(Mus))]
  Area1.95 <- ellipse(Mu1, S1, draw = FALSE)
  Area2.95 <- ellipse(Mu2, S2, draw = FALSE)
  Area1.50 <- ellipse(Mu1, S1, alpha = 0.5, draw = FALSE)
  Area2.50 <- ellipse(Mu2, S2, alpha = 0.5, draw = FALSE)
  mux1s <- phats[grepl("mux1", names(phats))]
  muy1s <- phats[grepl("muy1", names(phats))]
  mux2s <- phats[grepl("mux2", names(phats))]
  muy2s <- phats[grepl("muy2", names(phats))]
  yday.model <- with(my.data, c(min(yday), phats["t_mean"] + c(0, phats["dt_mean"]), max(yday)))
  y.model <- with(my.data, rep(phats[c("muy_mean[1]", "muy_mean[2]")], each = 2))
  x.model <- with(my.data, rep(phats[c("mux_mean[1]", "mux_mean[2]")], each = 2))
  
  n.ind <- max(my.data$id)
  palette(rich.colors(n.ind))
  
  # plot this puppy!
  
  if(fit){
    par(mar = c(0, 4, 0, 0), oma = c(4, 0, 4, 4), mgp = c(2,.5,0), 
        cex.lab = 1.25, tck = 0.01, xpd=NA)
    layout(rbind(c(1, 2), c(1, 3)))
    with(my.data, plot(x,y, asp=1, col=id, pch = 19, cex=0.5))
    d_ply(data.frame(my.data), "id", function(df) with(df, lines(x,y, col = id)))
    polygon(Area1.95, col = alpha("darkred", .2), bor = NA)
    polygon(Area2.95, col = alpha("darkred", .2), bor = NA)
    polygon(Area1.50, col = alpha("darkred", .7), bor = NA)
    polygon(Area2.50, col = alpha("darkred", .7), bor = NA)
    with(my.data,plot(yday, y, col= alpha(id,.5), pch = 19, cex=0.5, xaxt="n", xlab=""))
    d_ply(data.frame(my.data), "id",
          function(df) with(df, lines(yday, y, col = alpha(id,.5))))
    lines(yday.model, y.model, col="darkred", lwd = 4)
    with(my.data,plot(yday, x, col= alpha(id,.5), pch = 19, cex=0.5,  xlab=""))
    d_ply(data.frame(my.data), "id",
          function(df) with(df, lines(yday, x, col = alpha(id,.5))))
    lines(yday.model, x.model, col="darkred", lwd = 4)
  }
  # diagnostic plots
  pars.fit <- migration.fit %>% names
  pars <-  c("mux_mean[1]", "mux1_sigma", "muy_mean[1]", "muy1_sigma","rho1",
             "mux_mean[2]", "mux2_sigma", "muy_mean[2]", "muy2_sigma","rho2",
             "t_mean", "t_sd", "dt_mean", "dt_sd", "A", "sigma_ranging", "sigma_migration")
  
  if(density) stan_dens(migration.fit, par = pars, separate_chains = TRUE, ncol = 5) %>% print
  if(chains) stan_trace(migration.fit, par = pars) %>% print
}		


getInits <- function(my.data, t_mean = 110, dt_mean = 20, 
                     mu1_mean = NULL, mu2_mean = NULL, 
                     chains = 4, plotme = TRUE, K = 1, ddays = 15){
  
  
  start.days <- which(with(my.data, yday <= (min(yday) + ddays)))
  end.days <- which(with(my.data, yday >= (max(yday) - ddays)))
  if(is.null(mu1_mean)) mu1_mean = c(median(my.data$x[start.days]), median(my.data$y[start.days]))  
  if(is.null(mu2_mean)) mu2_mean = c(median(my.data$x[end.days]), median(my.data$y[end.days]))  
  
  constrain <- function(x, min = -Inf, max = Inf) pmax(x, min) %>% pmin(max)
  
  inits.raw <- with(my.data,
                    list(mu1_mean = mu1_mean,
                         mu2_mean = mu2_mean,
                         mux1_sigma = (sd(x)),
                         mux2_sigma = (sd(x)),
                         muy1_sigma = (sd(y)),
                         muy2_sigma = (sd(y)),
                         t_mean = t_mean))
  
  makeInitsList <- function(a, data = my.data, inits.raw = inits.raw, K = K){
    
    k <- length(unique(data$id))
    
    initlist <- with(data,
                     list(mu1_mean = (inits.raw$mu1_mean + rnorm(2, sd = 10)*K)  %>% 
                            cbind(c(max(x), max(y))) %>% apply(1, min) %>% 
                            cbind(c(min(x), min(y))) %>% apply(1, max) ,
                          mu2_mean = (inits.raw$mu2_mean + rnorm(2, sd = 10)*K) %>% 
                            cbind(c(max(x), max(y))) %>% apply(1, min) %>% 
                            cbind(c(min(x), min(y))) %>% apply(1, max),
                          mux1_sigma = (var(x) + rnorm(1, sd = 100)*K) %>% constrain(min = 0.1),
                          mux2_sigma = (var(x) + rnorm(1, sd = 100)*K) %>% constrain(min = 0.1),
                          muy1_sigma = (var(y) + rnorm(1, sd = 100)*K) %>% constrain(min = 0.1),
                          muy2_sigma = (var(y) + rnorm(1, sd = 100)*K) %>% constrain(min = 0.1),
                          t_mean = (t_mean + rnorm(1, sd = 3)*K) %>% constrain(min = min(yday), max = max(yday)), 
                          dt_mean = (dt_mean + rnorm(1, sd = 1)*K) %>% constrain(min = 0.1), 
                          t_sd = 10 + rnorm(1, sd = 1)*K %>% constrain(min = 0.1), 
                          dt_sd = (5 + rnorm(1, sd = 0.5)*K) %>% constrain(min = 0.1),
                          sigma_r = (10 + rnorm(1, sd = 2)*K) %>% constrain(min = 0.1), 
                          sigma_m = (20 + rnorm(1, sd = 3)*K) %>% constrain(min = 0.1), 
                          phi_r = 0.1, 
                          phi_m = .8))
    
    
    initlist$mu1 = daply(data %>% mutate(id = as.integer(id)), "id", function(df) df[1,c("x", "y")]) %>% as.numeric %>% matrix(ncol=2)
    initlist$mu2 = daply(data %>% mutate(id = as.integer(id)), "id", function(df) df[nrow(df),c("x", "y")]) %>% as.numeric %>% matrix(ncol=2) 
    
    initlist$t <- with(initlist, t_mean + rnorm(k, sd = 5)*K) %>% constrain(min = min(data$yday), max = max(data$yday))
    initlist$dt <- with(initlist, dt_mean + rnorm(k, sd = 1)*K) %>% constrain(min = 0.1, max = max(data$yday))
    return(initlist)
  }
  
  inits <- lapply(1:chains, makeInitsList, data = my.data, inits.raw = inits.raw, K = K)
  
  if(plotme){
    layout(cbind(c(1,1), 2:3))
    
    with(my.data, plot(x, y, pch = 19, col = rgb(0,0,0,.4), cex= 0.5, asp = 1))
    with(inits.raw, points(rbind(mu1_mean, mu2_mean), col = 2:3, cex = 2, lwd = 4, lty = 2))
    lapply(inits, function(l) with(l, points(rbind(mu1_mean, mu2_mean), col = 2:3, cex = 2, lwd = 2, pch = 4)))
    
    with(my.data, plot(yday, x, pch = 19, col = rgb(0,0,0,.4), cex= 0.5))  
    with(inits.raw, abline(v = c(t_mean, t_mean+dt_mean), col=2:3, lwd = 2))
    lapply(inits, function(l) with(l, abline(v = c(t_mean, t_mean+dt_mean), col = 2:3, lty = 3)))
    
    with(my.data, plot(yday, y, pch = 19, col = rgb(0,0,0,.4), cex= 0.5))  
    with(inits.raw, abline(v = c(t_mean, t_mean+dt_mean), col=2:3, lwd = 2))
    lapply(inits, function(l) with(l, abline(v = c(t_mean, t_mean+dt_mean), col = 2:3, lty = 3)))
  }
  
  return(inits)
}


summarizeFit <- function(myfit){
  
  require(coda)
  require(mixtools)
  
  migration.fit <- myfit$migration.fit
  my.data <- myfit$my.data
  my.df <- myfit$my.df
  
  # diagnose chains for each year
  
  phats <- summary(As.mcmc.list(migration.fit))$quantiles[,"50%"]
  
  Mus <- phats[grep("mean", names(phats))]
  Sigmas <- phats[grepl("Sigma", names(phats)) & grepl("mu", names(phats))] %>% as.list
  names(Sigmas) <- c("S1.11","S1.12","S1.21","S1.22",
                     "S2.11","S2.12","S2.21","S2.22")
  
  xy.results <- cbind(Mus[c("mux_mean[1]", "mux_mean[2]")], Mus[c("muy_mean[1]", "muy_mean[2]")])
  ll.lm <- lm(cbind(lon, lat) ~ poly(x,2)*poly(y,2), data = my.df) 
  
  # latlong
  
  ll.results <- predict(ll.lm, newdata = data.frame(x =  xy.results[,1], y =  xy.results[,2])) %>% t %>% as.vector %>% t %>% data.frame 
  names(ll.results) <- c("Lon1", "Lat1", "Lon2", "Lat2")
  
  # xysigma
  
  xy <- data.frame(t(xy.results[cbind(c(1,1,2,2), c(1,2,1,2))] ))
  names(xy) <- c("x1", "y1", "x2", "y2")
  xysigma <- with(Sigmas, data.frame(xy, sx1 = S1.11, sy1 = S1.22, sxy = S2.11, sx2 = S2.11, sy2 = S2.22, sxy = S2.12))
  
  # time results
  Ts <- phats[c("t_mean", "dt_mean", "t_sd", "dt_sd")] %>% t %>% data.frame
  
  return(cbind(ll.results, Ts))
}
