source("main_pkgs.R")

cellsize = 0.25
cellsize = 1/16
range = c(91.5, 92.5, 31, 32)
grid <- get_grid(range, cellsize = cellsize, type = "vec")

## 
# Create a tessellated surface
{
  # xlim <- range(st$lon)
  # ylim <- range(st$lat)
  
  PPP <- with(st2, ppp(lon, lat, owin(range[1:2], range[3:4])))
  th  <- dirichlet(PPP)
  mask <-  as(th, "SpatialPolygons")
  mask %<>% SpatialPolygonsDataFrame(data = data.table(site = st2$I))
  proj4string(mask) <- prj84
}

sp <- df2sp(st)
# spplot(poly)
write_fig({
  plot(poly, axes = TRUE, xaxs = "i", yaxs = "i"); #grid()
  plot(sp, add = TRUE)
}, "soil_naqu_voronoi_info.pdf", 8, 7)

## 2. weights of thesen
polys <- as_SpatialPolygonsDataFrame(grid)

lst = foreach(i = 1:nrow(polys)) %do% {
  
  res <- raster::intersect(mask, polys[i, ])
  ## calculate weights
  area <- area(res)/1e6 # km^2
  w <- area/sum(area)
  dw <- res@data %>% cbind(area =area, w = w)
  
  ## interp
  ind <- match(dw$site, st2$I) # selected sites
  mat = data2[, ind, drop = FALSE]
  
  # deal with NA values
  z = rowWeightedMeans(mat, w, na.rm = TRUE)
}

mat_z = do.call(cbind, lst) %>% t()
df_z = as.data.table(mat_z) %>% set_colnames(dates_str)

grid@data <- df_z
r <- brick(grid)
writeRaster(r, sprintf("soil_naqu_%sdeg_201008-201412_voronoi.tif", as.character(cellsize)), 
            overwrite = TRUE)
# mat = matrix(1:16, 4)
# mat[1] = NA

p <- spplot(grid, 1:16, as.table = TRUE)
write_fig(p, "soil_naqu_voronoi.pdf")

mat = as.array(r)
