source("main_pkgs.R")

## Note: 
# 1. WGS84大地坐标系统下，block krige不工作
# ------------------------------------------------------------------------------

# cellsize = 0.25
cellsize = 1/16
range = c(91.5, 92.5, 31, 32)
grid <- get_grid(range, cellsize = cellsize, type = "vec")

df <- as.data.table(t(data2))
sp@data <- df
# spplot(sp, 1:16, as.table = TRUE)

## 01. test kriging model and parameter optim ----------------------------------
# write_fig(
{
  ps <- foreach(i = seq(1, ncol(sp), 30)[1:25]) %do% {
  # for (i in ) {
    # i = 91
    s = sp[, i]
    ind <- s@data[[1]] %>% which.notna() # rm NA vlaues
    s = s[ind, ]
    formula <- as.formula(sprintf("%s ~ 1", names(s)))
    vgm <- fit_kriging_vgm(s, range = 20, show = TRUE)
    vgm$plot
    # grid2 <- krige(formula, s, grid, model = vgm$vgm)
    # grid2@data
  }
  p <- arrangeGrob(grobs = ps, nrow = 5)
  write_fig(p, "kriging_vgm_model_parameters.pdf", 15, 10)
}

## 1. krige action -------------------------------------------------------------
## 01. test kriging model and parameter optim ----------------------------------
# write_fig(
blocks = as_SpatialPolygonsDataFrame(grid)
{
  inds2 = seq(1, ncol(sp), 30)[1:25]
  inds = 1:ncol(sp)
  lst <- foreach(i = inds) %do% {
    runningId(i, 100)
    # for (i in ) {
    # i = 91
    s = sp[, i]
    ind <- s@data[[1]] %>% which.notna() # rm NA vlaues
    s = s[ind, ]
    formula <- as.formula(sprintf("%s ~ 1", names(s)))
    vgm <- fit_kriging_vgm(s, range = 20, show = TRUE)
    # vgm$plot
    g <- gstat(formula = formula, data = s, model = vgm$vgm)
    # predict.gstat.R L119 add browser
    ans <- predict(g, newdata = blocks,  
                           sps.args = list(n = 4, type = "regular", offset = c(.5, .5)))
    grid2@data
  }
  l <- lst %>% purrr::transpose() %>% map(~do.call(cbind, .) %>% data.table)
  # p <- arrangeGrob(grobs = ps, nrow = 5)
  # write_fig(p, "kriging_vgm_model_parameters.pdf", 15, 10)
}

grid2 <- grid
grid2@data <- l$var1.pred %>% set_colnames(dates_str[inds])
r <- brick(grid2)
writeRaster(r, sprintf("soil_naqu_%sdeg_201008-201412_OriginalKrige.tif", as.character(cellsize)), 
            overwrite = TRUE)

blocks = as_SpatialPolygonsDataFrame(grid)
pwidth <- pheight <- area(blocks) %>% max() %>% sqrt() %>% divide_by(2)

nugget = vgm$vgm$psill[2]
psill = vgm$vgm$psill[2]
range = vgm$vgm$range[2]
preCK=preCKrige(newdata=blocks,
                model=covmodel("exponential",0, nugget, psill, range),pwidth=75,pheight=75)
plot(preCK, 59)

{
  brks = seq(0, 0.5, 0.01) %>% c(-Inf, ., Inf)
  ncol = length(brks) - 1
  cols <- get_color("amwg256", ncol) %>% rev()
  sizes <- seq(0.5, 1.5, length.out = ncol)
  
  p2 <- spplot(grid2, inds2, at = brks, col.regions = cols, 
              panel = function(x, y, z, subs, ...) {
                panel.levelplot(x,y,z, subs, ...)
                i = inds[panel.number()]
                val <- data2[i, ]
                ind_nona <- which.notna(val)
                val <- cut(val[ind_nona], brks)
                # browser()
                panel.xyplot(st2$lon[ind_nona], st2$lat[ind_nona], pch = 21, fill = cols[val], cex = sizes[val], col = "black")
              },
               # sp.layout = sp_layout, 
              as.table = TRUE)
  p2
  write_fig(p2, "krige_outputs_example.pdf", 10, 7)
}
# }, "kriging_vgm_model_parameters.pdf", 10, 10)


## 01. test kriging model and parameter optim ----------------------------------
