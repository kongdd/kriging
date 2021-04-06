source("test/main_pkgs.R")
load("data-raw/raw_soil_Inputs.rda")

# Maqu: 33.5-34.125，101.625-102.6875
# Ali : 32.3125-33.5, 79.5-80.1875
# Naqu: 31-32, 91.5-92.5
cellsize = 0.25
cellsize = 1/16

ranges = list(
  Naqu = c(91.5, 92.5, 31, 32),
  Ali  = c(79.5, 80.1875, 32.3125, 33.5),
  Maqu = c(101.625, 102.6875, 33.5, 34.125))

# Create a tessellated surface
{
  site  = "Naqu"
  range = ranges[[site]]
  grid <- get_grid(range, cellsize = cellsize, type = "vec")
  polys <- as_SpatialPolygonsDataFrame(grid)
  # xlim <- range(st$lon)
  # ylim <- range(st$lat)
  # st2 = st[area == site, ] %>% cbind(I = 1:nrow(.), .)
}

sp <- df2sp(st)
# spplot(poly)
write_fig({
  plot(mask, axes = TRUE, xaxs = "i", yaxs = "i"); #grid()
  plot(sp, add = TRUE)
}, "soil_naqu_voronoi_info.pdf", 8, 7)

## 2. weights of thesen
{
  depths = c(5, 10, 20, 40)
  df = INPUTS$Naqu[depth == 5]
  d_time = df[, 1]
  data = df[, -(1:2)] %>% as.matrix()
  sites = colnames(data) %>% gsub("^SM_|^ST_", "", .)
  ind_st = match(sites, st$station_no)
  
  st2 = st[ind_st, ] %>% cbind(I = 1:nrow(.), .)
  PPP <- with(
    st2,
    ppp(lon, lat, owin(range[1:2], range[3:4]))
  )
  th <- dirichlet(PPP)
  # mask <- as.SpatialPolygons.tess(th)
  mask <- as(th, "SpatialPolygons")
  mask %<>% SpatialPolygonsDataFrame(data = st2[, .(I, station_no)])
  proj4string(mask) <- prj84
}

lst = foreach(i = 1:nrow(polys)) %do% {  
  res <- raster::intersect(mask, polys[i, ]) # 和泰森多边形相交，加权平均
  ## calculate weights
  area <- area(res)/1e6 # km^2
  w  <- area/sum(area)
  dw <- res@data %>% cbind(area =area, w = w)
  
  ## interp
  ind <- match(dw$I, st2$I) # selected sites
  mat = data[, ind, drop = FALSE]

  # deal with NA values
  z = rowWeightedMeans(mat, w, na.rm = TRUE)
}

mat_z = do.call(cbind, lst) %>% t()
df_z = as.data.table(mat_z) %>% set_colnames(d_time$datetime %>% as.character())

grid@data <- df_z
r <- brick(grid)

arr <- as.array(r)
write_fig({
  plot(r[[1:16]])
}, "a.pdf")
write_fig({
  plot(r2[[1:16]])
}, "a2.pdf")
image(arr[,,1] %>% t() %>% flipud())

ncwrite(
  list(SM = arr %>% aperm(c(2, 1, 3)) %>% flipud()),
  "soil_naqu_0.0625deg_20100801H00-20161231H23_voronoi-static.nc",
  var.units = NULL,
  var.longname = NULL,
  prec = "float",
  dims = NULL,
  dimnames_last = NULL,
  range = range,
  dates = d_time$datetime,
  calendar = "gregorian",
  missval = -9999L,
  compression = NA,
  overwrite = FALSE,
  verbose = FALSE
)

# writeRaster(r, sprintf("soil_naqu_%sdeg_201008-201412_voronoi.tif", as.character(cellsize)), 
#             overwrite = TRUE)
writeRaster(r, sprintf("soil_naqu_%sdeg_20100801H00-20161231H23_voronoi.tif", as.character(cellsize)), 
            overwrite = TRUE)
d_time %>% cbind(I = 1:nrow(.), .) %>% fwrite("bands_20100801H00-20161231H23.txt")
# p <- spplot(grid, 1:16, as.table = TRUE)
# write_fig(p, "soil_naqu_voronoi.pdf")
# mat = as.array(r)
