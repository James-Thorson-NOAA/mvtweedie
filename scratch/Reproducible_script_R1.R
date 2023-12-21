
#####################
#
# Directions:
#  1. Ensure that "mvtweedie-main.zip", "MDO.seabirdforagingarea.SST.csv", "Wolf.csv", and "Seabird.csv" are all in the directory `run_dir`
#  2. Ensure that all packages called via `library(.)` are installed
#  3. Consider instead installing an up-to-date version of mvtweedie from GitHub (available upon acceptance with release numbers, where publication uses release `1.0.0`)
#
#####################

# Direction 1
run_dir = R'(C:\Users\James.Thorson\Desktop\Git\mvtweedie\scratch)'

# Load packages
library(ggplot2)
library(mgcv)
library(mvtweedie)
library(rnaturalearth)
library(sf)
library(rgeos)
library(abind)
library(raster)
library(plotrix)
library(tweedie)
library(viridisLite)
library(dplyr)
library(broom)

##############
# Local plotting function
##############

plot_histogram <-
function( x,
          freq = TRUE,
          breaks = "Sturges",
          y_buffer = 0.05,
          ylim = NULL,
          xlim = NULL,
          main = "",
          col = "lightgrey",
          bty = "o",
          add = FALSE,
          ...){

  # Modify default inputs
  if( !is.list(x) ){
    if( is.vector(x) ){
      x = list( x )
    }
    if( is.matrix(x) ){
      tmp = list()
      for( cI in 1:ncol(x) ){
        tmp[[cI]] = x[,cI]
      }
      x = tmp
    }
  }
  if( length(col)==1 & length(x)>1 ) col = rep(col,length(x))

  # Figure out ylim
  Hist = NULL
  if(is.null(ylim)) ylim = c(NA, 0)
  if(is.null(xlim)){
    xlim_to_use = c(NA, NA)
  }else{
    xlim_to_use = xlim
  }
  for(i in 1:length(x)){
    Hist[[i]] = hist( x[[i]], breaks=breaks, plot=FALSE )
    if(is.na(ylim[1]) & freq==TRUE) ylim[2] = max(ylim[2], max(Hist[[i]]$counts)*(1+y_buffer) )
    if(is.na(ylim[1]) & freq==FALSE) ylim[2] = max(ylim[2], max(Hist[[i]]$density)*(1+y_buffer) )
    if(is.null(xlim)) xlim_to_use = range( c(xlim_to_use,Hist[[i]]$breaks), na.rm=TRUE)
  }
  if(is.na(ylim[1])) ylim[1] = 0

  # Plot
  for(i in 1:length(x)){
    hist( x[[i]], breaks=breaks, freq=freq, ylim=ylim, xlim=xlim_to_use, col=col[i], main=main, add=ifelse(i==1,add,TRUE), ...)
  }
  if( bty=="o" ) box()

  # Return stuff
  Return = list("Hist"=Hist, "ylim"=ylim)
  return( invisible(Return) )
}


##############
# Simulation
##############

set.seed(101)

y = x = seq(0,1,length=100)
n_prey = 100

prey_cz = cbind(
  "x" = c(0.6, 0.2, 0.2),
  "y" = c(0.5, 0.8, 0.5),
  "sd" = c(0.2, 0.2, 0.2),
  "mean" = c(1.2, 1.2, 1.2),
  "cv" = c(1, 1, 1)
)

site_cz = cbind(
  "x" = c(0.2, 0.5, 0.8),
  "y" = c(0.8, 0.5, 0.2),
  "radius" = c(0.050, 0.075, 0.100)
)


get_density = function(x, y, alpha_z){
  sqrt( dnorm(x,alpha_z[1],alpha_z[3])/dnorm(alpha_z[1],alpha_z[1],alpha_z[3])
      * dnorm(y,alpha_z[2],alpha_z[3])/dnorm(alpha_z[2],alpha_z[2],alpha_z[3]) )
}
D_xyc = abind(
  outer(x, y, FUN=get_density, alpha_z = prey_cz[1,]),
  outer(x, y, FUN=get_density, alpha_z = prey_cz[2,]),
  outer(x, y, FUN=get_density, alpha_z = prey_cz[3,]),
  along=3
)

simulate_prey = function(n=1000, alpha_z){
  loc_nz = matrix(nrow=0,ncol=2)
  while(nrow(loc_nz)<n){
    xy = runif(2)
    dens = get_density( x=xy[1], y=xy[2], alpha_z=alpha_z )
    if(dens>runif(1)){
      loc_nz = rbind(loc_nz,xy)
    }
  }
  # Simulate weights
  weight_n = rgamma(n=n, shape=1/alpha_z['cv']^2, scale=alpha_z['mean']*alpha_z['cv']^2)
  response = cbind("x"=loc_nz[,1], "y"=loc_nz[,2], "weight"=weight_n)
  return(response)
}

pdf( file=paste0(run_dir,"Fig_1.pdf"), width=6.0, height=7.5 )
  n_d = 1000
  n_r = 1000
  separate_zeros = TRUE
  par(mfrow=c(4,3), mar=c(2,2,2,1), oma=c(0,3.5,0,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i" )
  # First row
  Y_rcc = array(NA, dim=c(n_r,3,3), dimnames=list(NULL,"Site"=NULL,"Prey"=NULL) )
  for( c1 in 1:3 ){
    D = sp::SpatialPointsDataFrame( coords=expand.grid(x,y), data=data.frame("D"=as.vector(D_xyc[,,c1])) )
    #R = plotKML::vect2rast( D, cell.size=mean(diff(x)), fun=mean )

    Raster_layer = raster::raster( D, nrows=length(x), ncols=length(x) )
    R = raster::rasterize( x=D@coords, y=Raster_layer, field=as.numeric(D@data[,1]), fun=mean )
    #raster::image( Raster_proj[[tI]], col=col(1000), zlim=Zlim, add=TRUE )
    
    #R = raster( t(D_xyc[,,c1]), xmn=0, xmx=1, ymn=0, ymx=1)
    #plot(R)
    plot(1, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", xlab="", ylab="")
    contour( R, add=TRUE, levels=seq(0,1,by=0.25) )
    P = simulate_prey(n=n_prey, alpha_z=prey_cz[c1,])
    points( x=P[,1], y=P[,2], col=viridis(3)[c1], pch=20, cex=sqrt(P[,3]) )
    #drawc1rcle( x=site_cz[c1,'x'], y=site_cz[c1,'y'],
    #  radius=site_cz[c1,'radius'], fillColor=rainbow(3,alpha=0.2)[c1])
    for(c2 in 1:nrow(site_cz)){
      draw.circle( x=site_cz[c2,'x'], y=site_cz[c2,'y'],
        radius=site_cz[c2,'radius'], col=rainbow(3,alpha=0.2)[c2], border=rgb(0,0,0,0))
      dist_vec = RANN::nn2( data=site_cz[c2,c('x','y'),drop=FALSE], query=P[,c('x','y')] )$nn.dist[,1]
      Y_rcc[1,c2,c1] = sum(P[which(dist_vec<site_cz[c2,'radius']),'weight'])
    }
    if(c1==3){
      legend("topright", bty="n", fill=rainbow(3), legend=toupper(letters[1:3]), title="Site" )
    }
    mtext( side=3, text=paste0("Prey ",c1), col=viridis(3)[c1], font=2 )
    if(c1==1) mtext( side=2, text="Single replicate\nof spatial distribution", line=2)
  }
  for( rI in 1:n_r ){
    for( c1 in 1:3 ){
      P = simulate_prey(n=n_prey, alpha_z=prey_cz[c1,])
      for(c2 in 1:nrow(site_cz)){
        dist_vec = RANN::nn2( data=site_cz[c2,c('x','y'),drop=FALSE], query=P[,c('x','y')] )$nn.dist[,1]
        Y_rcc[rI,c2,c1] = sum(P[which(dist_vec<site_cz[c2,'radius']),'weight'])
      }
    }
  }
  # Prepare for 3rd row
  d_z = seq(0,10,length=n_d)
  phi_cc = p_cc = lambda_cc = gamma_cc = alpha_cc = mu_cc = array(NA, dim=c(3,3), dimnames=list("Site"=NULL,"Prey"=NULL))
  maxY_c = maxD_c = rep(0,3)
  for( c1 in 1:3 ){
    maxD_c[c1] = 0
    for( c2 in 1:3 ){
      lambda_cc[c1,c2] = get_density( x=site_cz[c1,1], y=site_cz[c1,2], alpha_z=prey_cz[c2,] )
      lambda_cc[c1,c2] = lambda_cc[c1,c2] / (sum(D_xyc[,,c1])*mean(diff(x))^2) * site_cz[c1,3]^2*pi * n_prey
      alpha_cc[c1,c2] = 1 / prey_cz[c2,'cv']^2
      gamma_cc[c1,c2] = prey_cz[c2,'mean'] * prey_cz[c2,'cv']^2
      mu_cc[c1,c2] = lambda_cc[c1,c2] * gamma_cc[c1,c2] * alpha_cc[c1,c2]
      p_cc[c1,c2] = (alpha_cc[c1,c2]+2) / (alpha_cc[c1,c2]+1)
      phi_cc[c1,c2] = gamma_cc[c1,c2] * (alpha_cc[c1,c2]+1) * mu_cc[c1,c2]^(-1/(alpha_cc[c1,c2]+1))
      Zero_prob = exp(-lambda_cc)
    }
  }
  pprime_cc = p_cc
  phiprime_cc = phi_cc
  for( c1 in 1:3 ){
    for( c2 in 1:3 ){
      pprime_cc[c1,c2] = mean(p_cc)
    }
  }
  for( c1 in 1:3 ){
    for( c2 in 1:3 ){
      # Harmonic mean
      #phiprime_cc[c1,c2] = mean(phi_cc[,c2])
      #phiprime_cc[c1,c2] = median(phi_cc[,c2])
      #phiprime_cc[c1,c2] = 1/mean(1/phi_cc[,c2])
    }
    FUN = function( phi ){
      LL = 0
      for(c1star in 1:3){
      for(c2star in 1:3){
        LL = LL + sum(log(dtweedie(Y_rcc[,c1star,c2star], mu=mu_cc[c1star,c2star], power=pprime_cc[c1star,c2star], phi=phi)))
      }}
      return(LL)
    }
    opt = optimize( f=FUN, interval=c(0.1,10), maximum=TRUE )
    phiprime_cc[,c1] = opt$maximum
  }
  for( c1 in 1:3 ){
    for( c2 in 1:3 ){
      maxD_c[c1] = max( maxD_c[c1], qtweedie(p=0.99, mu=mu_cc[c1,c2], power=pprime_cc[c1,c2], phi=phiprime_cc[c1,c2]) )
    }
  }
  # 2nd row
  for( c1 in 1:3 ){
    X = Y_rcc[,c1,]
    X = ifelse(X>maxD_c[c1],maxD_c[c1],X)
    if( separate_zeros==TRUE ){
      Zero_c = colMeans(X==0)
      X = ifelse(X==0,-2,X)
      out = plot_histogram( X, breaks=c(-2,-1,seq(0,maxD_c[c1],length=20)), plot=FALSE, freq=FALSE ) #, ylim=c(0,maxY_c[c1]) )
      maxY_c[c1] = max( c(maxY_c[c1], 1.2*out$ylim[2], 1.2*Zero_c) )
      out = plot_histogram( X, breaks=c(-2,-1,seq(0,maxD_c[c1],length=20)), col=viridis(3,alpha=0.2), border=NA, freq=FALSE, xlim=c(0,maxD_c[c1]), ylim=c(0,maxY_c[c1]) )
      points( x=rep(0,3), y=Zero_c, col=viridis(3), pch=15, cex=2 )
      abline( v=Y_rcc[1,c1,], col=viridis(3), lty="dotted", lwd=2 )
      mtext( side=3, text=paste0("Site ",toupper(letters[c1])), col=rainbow(3)[c1], font=2 )
    }else{
      out = plot_histogram( X, breaks=c(-2,-1,seq(0,maxD_c[c1],length=20)), col=viridis(3,alpha=0.2), border=NA, freq=FALSE, xlim=c(0,maxD_c[c1]) ) #, ylim=c(0,maxY_c[c1]) )
      maxY_c[c1] = max( c(maxY_c[c1], 1.2*out$ylim[2]) )
    }
    legend("topright", bty="n", fill=viridis(3), legend=formatC(colMeans(Y_rcc[,c1,])/sum(colMeans(Y_rcc[,c1,])),format="f",digits=2), title="Proportion" )
    if(c1==1) mtext( side=2, text="Sampling from\ntrue distribution", line=2 )
  }
  # 3rd row
  density_zcc = array(NA, dim=c(length(d_z),3,3), dimnames=list("Density"=NULL,"Site"=NULL,"Prey"=NULL) )
  bsample_zcc = rsample_zcc = array(NA, dim=c(1000,3,3), dimnames=list("Density"=NULL,"Site"=NULL,"Prey"=NULL) )
  for( c1 in 1:3 ){
    d_z = seq(0, maxD_c[c1], length=n_d)
    while(any(is.na(rsample_zcc[,c1,]))){
      for( c2 in 1:3 ){
        density_zcc[,c1,c2] = dtweedie( d_z, mu=mu_cc[c1,c2], power=p_cc[c1,c2], phi=phi_cc[c1,c2] )
        rsample = rtweedie( dim(rsample_zcc)[1], mu=mu_cc[c1,c2], power=p_cc[c1,c2], phi=phi_cc[c1,c2] )
        bsample_zcc[,c1,c2] = rsample_zcc[,c1,c2] = ifelse( is.na(rsample_zcc[,c1,c2]), rsample, rsample_zcc[,c1,c2] )
      }
      rsample_zcc[,c1,] = rsample_zcc[,c1,] * outer( ifelse(rowSums(rsample_zcc[,c1,])==0,NA,1), rep(1,3) )
    }
    if( separate_zeros==TRUE ){
      plot( 1, type="n", xlim=range(d_z), xlab="Prey biomass", ylab="", ylim=c(0,maxY_c[c1]) )   # , ylim=c(0,1.05*max(density_zcc[,c1,]))
      points( x=rep(0,3), y=density_zcc[1,c1,], col=viridis(3), pch=20, cex=3 )
      matplot( x=d_z[-1], y=density_zcc[-1,c1,], col=viridis(3), lty="solid", type="l", add=TRUE, lwd=3 )
      mtext( side=3, text=paste0("Site ",toupper(letters[c1])), col=rainbow(3)[c1], font=2 )
      #abline( v=Y_rcc[1,c1,], col=viridis(3), lty="dotted", lwd=2 )
    }else{
      X = bsample_zcc[,c1,]
      X = ifelse(X>maxD_c[c1],maxD_c[c1],X)
      plot_histogram( X, breaks=c(-2,-1,seq(0,maxD_c[c1],length=20)), col=viridis(3,alpha=0.2), border=NA, freq=FALSE, xlim=c(0,maxD_c[c1]), ylim=c(0,maxY_c[c1]) )
    }
    legend("topright", bty="n", fill=viridis(3), legend=formatC(mu_cc[c1,]/sum(mu_cc[c1,]),format="f",digits=2), title="Proportion" )
    if(c1==1) mtext( side=2, text="Tweedie:\ndistribution of density", line=2 )
  }
  # 4th row
  density_zcc = array(NA, dim=c(length(d_z),3,3), dimnames=list("Density"=NULL,"Site"=NULL,"Prey"=NULL) )
  bsample_zcc = rsample_zcc = array(NA, dim=c(1000,3,3), dimnames=list("Density"=NULL,"Site"=NULL,"Prey"=NULL) )
  for( c1 in 1:3 ){
    d_z = seq(0, maxD_c[c1], length=n_d)
    while(any(is.na(rsample_zcc[,c1,]))){
      for( c2 in 1:3 ){
        density_zcc[,c1,c2] = dtweedie( d_z, mu=mu_cc[c1,c2], power=pprime_cc[c1,c2], phi=phiprime_cc[c1,c2] )
        rsample = rtweedie( dim(rsample_zcc)[1], mu=mu_cc[c1,c2], power=pprime_cc[c1,c2], phi=phiprime_cc[c1,c2] )
        bsample_zcc[,c1,c2] = rsample_zcc[,c1,c2] = ifelse( is.na(rsample_zcc[,c1,c2]), rsample, rsample_zcc[,c1,c2] )
      }
      rsample_zcc[,c1,] = rsample_zcc[,c1,] * outer( ifelse(rowSums(rsample_zcc[,c1,])==0,NA,1), rep(1,3) )
    }
    if( separate_zeros==TRUE ){
      plot( 1, type="n", xlim=range(d_z), xlab="Prey biomass", ylab="", ylim=c(0,maxY_c[c1]) )   # , ylim=c(0,1.05*max(density_zcc[,c1,]))
      points( x=rep(0,3), y=density_zcc[1,c1,], col=viridis(3), pch=20, cex=3 )
      matplot( x=d_z[-1], y=density_zcc[-1,c1,], col=viridis(3), lty="solid", type="l", add=TRUE, lwd=3 )
      mtext( side=3, text=paste0("Site ",toupper(letters[c1])), col=rainbow(3)[c1], font=2 )
      #abline( v=Y_rcc[1,c1,], col=viridis(3), lty="dotted", lwd=2 )
    }else{
      X = bsample_zcc[,c1,]
      X = ifelse(X>maxD_c[c1],maxD_c[c1],X)
      plot_histogram( X, breaks=c(-2,-1,seq(0,maxD_c[c1],length=20)), col=viridis(3,alpha=0.2), border=NA, freq=FALSE, xlim=c(0,maxD_c[c1]), ylim=c(0,maxY_c[c1]) )
    }
    legend("topright", bty="n", fill=viridis(3), legend=formatC(mu_cc[c1,]/sum(mu_cc[c1,]),format="f",digits=2), title="Proportion" )
    if(c1==1) mtext( side=2, text="mvtweedie GLM:\ndistribution of density", line=2 )
  }
dev.off()

##############
# Seabird example
##############

SST = read.csv( paste0(run_dir,"MDO.seabirdforagingarea.SST.csv"))
DF = read.csv(file=paste0(run_dir,"Seabird.csv"))
DF$group = factor(DF$group)

DF$SST = SST[match(DF$Year,SST$Year),'SST_mean']
gam_seabird = gam( formula = Response ~ 0 + group + s(Year,by=group,bs="gp") + SST:group,
  data = DF,
  family = tw )
class(gam_seabird) = c( "mvtweedie", class(gam_seabird) )

# ggplot formatting
newdata = expand.grid("group"=levels(DF$group), "Year"=min(DF$Year):max(DF$Year))
newdata$SST = SST[match(newdata$Year,SST$Year),'SST_mean']
pred_seabird = predict( gam_seabird,
                   se.fit=TRUE,
                   origdata = DF,
                   newdata = newdata )
newdata = cbind( newdata, fit=pred_seabird$fit, se.fit=pred_seabird$se.fit )
newdata$lower = newdata$fit - newdata$se.fit
newdata$upper = newdata$fit + newdata$se.fit

#
DFp <- DF %>%
    group_by(Year) %>%
    arrange(group) %>%
    group_by(group, Year) %>%
    mutate(id = seq(n())) %>%
    group_by(Year, id) %>%
    mutate(prop = Response/sum(Response, na.rm = TRUE))

#
theme_set(theme_bw())
pdf( file=paste0(run_dir,"Fig_2.pdf"), width=6, height=4 )
  ggplot(newdata, aes(Year, fit, color = SST)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    facet_wrap(vars(group)) +
    scale_color_viridis_c(name = "SST") +
    ylim(0,1) +
    labs(y="Predicted proportion")
dev.off()

#
pdf( file=paste0(run_dir,"Fig_C3.pdf"), width=6, height=4 )
  ggplot(newdata, aes(Year, fit, color = SST)) +
    stat_sum(data=DFp, aes(y=prop), colour="black", alpha = 0.2) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    facet_wrap(vars(group)) +
    scale_color_viridis_c(name = "SST") +
    ylim(0,1) +
    labs(y="Predicted proportion")
dev.off()


# Effects plot
effects_table <- tidy(gam_seabird, parametric = TRUE) %>%
    filter(grepl("SST", term)) %>%
    mutate(across(term, ~gsub("(group|:SST_mean)","", .))) %>%
    mutate(across(term, ~reorder(factor(.), estimate)))

pdf( file=paste0(run_dir,"Fig_C2.pdf"), width=6, height=4 )
  ggplot(effects_table, aes(y=term, x = estimate, xmin = estimate-2*std.error, xmax = estimate+2*std.error)) +
    geom_pointrange() +
    geom_vline(xintercept=0, lty =2)
dev.off()

##############
# Wolf example
##############

DF = read.csv(file=paste0(run_dir,"Wolf.csv"))
DF$group = factor(DF$group)

# Using mgcv
gam_wolf = gam( formula = Response ~ 0 + group + s(Latitude,Longitude,m=c(1,0.5),bs="ds") +
     s(Latitude,Longitude,by=group,m=c(1,0.5),bs="ds"),
  data = DF,
  family = tw )
class(gam_wolf) = c( "mvtweedie", class(gam_wolf) )

# Predict raster on map
pred_wolf = predict_mvtweedie( gam_wolf,
                 origdata = DF,
                 length_out = 100 )
pred_wolf$cv.fit = pred_wolf$se.fit / pred_wolf$fit

# Map oceanmap layer
US_high = ne_countries(scale=10, country="united states of america")
p <- as(raster::extent(-140,-125,50,60), "SpatialPolygons")
wmap <- gIntersection(US_high, p)
oceanmap <- gDifference(p, wmap)
oceanmap = st_as_sf(oceanmap) %>% st_set_crs(., 4326) 

sf.isl <- data.frame(island = c("Baranof", "Chichagof", "Admiralty"), lat = c(57.20583, 57.88784, 57.59644), lon = c(-135.1866, -136.0024, -134.5776)) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326) 

mask.land = ne_countries(scale=10, country="united states of america", returnclass = 'sf') %>%
  st_set_crs(., 4326) %>%
  st_cast(., "POLYGON") %>%
  st_join(., sf.isl) %>% 
  filter(!is.na(island))

# Make figure
my_breaks = c(0.02,0.1,0.25,0.5,0.75)
pdf( file=paste0(run_dir,"Fig_3.pdf"), width=8, height=5 )
  ggplot(oceanmap) +
    geom_tile(data=pred_wolf, aes(x=Longitude,y=Latitude,fill=fit)) +
    geom_sf() +
    geom_sf(data = mask.land, inherit.aes = FALSE, fill = "darkgrey") + 
    coord_sf(xlim=range(pred_wolf$Longitude), ylim=range(pred_wolf$Latitude), expand = FALSE) +
    facet_wrap(vars(group), ncol = 5) +
    scale_fill_gradient2(name = "Proportion", trans = "sqrt", breaks = my_breaks) +
    scale_y_continuous(breaks = seq(55,59,2)) +
    scale_x_continuous(breaks = c(-135, -133.5, -132)) +
    theme(axis.text = element_text(size = 7))
dev.off()

