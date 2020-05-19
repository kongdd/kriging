source("main_pkgs.R")

# write_fig(show.vgms(), "gstats_vgm_models.pdf")

## example 01 ------------------------------------------------------------------
library(ggplot2)
library(constrainedKriging)
data("meuse.blocks")
{
  data(meuse)
  coordinates(meuse) <- ~ x + y
  lzn.vgm <- variogram(log(zinc)~1, meuse) # calculates sample variogram values 
  lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1)) # fit model
  
  plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model
  
  data("meuse.grid")
  coordinates(meuse.grid) <- ~ x + y # step 3 above
  # lzn.kriged  <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)
  g <- gstat(formula = log(zinc) ~ 1, data = meuse, model=lzn.fit)
  # predict.gstat.R L119 add browser
  lzn.kriged2 <- predict(g, newdata = meuse.blocks,  
                         sps.args = list(n = 4, type = "regular", offset = c(.5, .5)))
  
  lzn.kriged %>% as.data.frame %>%
    ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "yellow", high="red") +
    # scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
    theme_bw()
}

## block krige -----------------------------------------------------------------
# approximation of block variance
# pixel area = 75m x 75m
# exponential covariance function with measurement error = 0, nugget = 0.05,
# part. sill =  0.15 and range parameter = 192.5
blockKrige_test = FALSE
if (blockKrige_test) {
  preCK=preCKrige(newdata=meuse.blocks,
                model=covmodel("exponential",0,0.05,0.15,192.5),pwidth=75,pheight=75)
  plot(preCK, 59)
  # block prediction by constrained kriging on the log scale
  CK=CKrige(formula=log(zinc)~sqrt(dist),data=meuse,
            locations=~x+y,object=preCK,ex.out=TRUE)
  {
    data(meuse.grid)
    coordinates(meuse.grid) = ~x+y
    proj4string(meuse.grid) <- CRS("+init=epsg:28992")
    gridded(meuse.grid) = TRUE
    
    gridparameters(meuse.grid)
    
    zn = krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, zn.res.m)
    zn$pointkr = zn$var1.pred
    zn40 = krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, zn.res.m,
                  + block = c(40, 40))
    zn400 = krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, zn.res.m,
                    + block = c(400, 400))
    zn$block40 = zn40$var1.pred
    zn$block400 = zn400$var1.pred
    spplot(zn[c("pointkr", "block40", "block400")], as.table = TRUE)
    zn$pointkr.se = sqrt(zn$var1.var)
    zn$block40.se = sqrt(zn40$var1.var)
    zn$block400.se = sqrt(zn400$var1.var)
    spplot(zn[c("pointkr.se", "block40.se", "block400.se")], as.table = TRUE)
  }
}
