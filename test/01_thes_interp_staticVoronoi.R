source("test/main_pkgs.R")
# spplot(poly)
write_fig({
  plot(mask, axes = TRUE, xaxs = "i", yaxs = "i"); #grid()
  plot(sp, add = TRUE)
}, "soil_naqu_voronoi_info.pdf", 8, 7)

## 2. weights of thesen

depths <- c(5, 10, 20, 40)

StaticVoronoi("Ali")
StaticVoronoi("Maqu")
StaticVoronoi(depthi = 5)
StaticVoronoi(depthi = 10)
StaticVoronoi(depthi = 20)
StaticVoronoi(depthi = 40)

StaticVoronoi = function(site = c("Ali", "Maqu", "Naqu")[1], depthi = NA) {
  # site  = "Naqu"
  range = ranges[[site]]
  grid <- get_grid(range, cellsize = cellsize, type = "vec")
  polys <- as_SpatialPolygonsDataFrame(grid)
  # xlim <- range(st$lon)
  # ylim <- range(st$lat)
  # st2 = st[area == site, ] %>% cbind(I = 1:nrow(.), .)
  
  {
    df = INPUTS[[site]]
    if (!is.na(depthi)) {
      df = df[depth == depthi]
    } else {
      depthi = df$depth[1]
    }
    
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
    runningId(i)
    res <- raster::intersect(mask, polys[i, ]) # 和泰森多边形相交，加权平均
    ## calculate weights
    area <- area(res) / 1e6 # km^2
    w <- area / sum(area)
    dw <- res@data %>% cbind(area = area, w = w)

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

  str_depth = sprintf("%02dcm", depthi)
  date_start = df$datetime %>% first() %>% format("%Y%m%dH%H")
  date_end   = df$datetime %>% last() %>% format("%Y%m%dH%H")
  ncwrite(
    list(SM = arr %>% aperm(c(2, 1, 3)) %>% flipud()),
    glue("soil_{site}-{str_depth}_0.0625deg_{date_start}-{date_end}_StaticVoronoi.nc"),
    prec = "float",
    range = range,
    dates = d_time$datetime,
    calendar = "gregorian",
    missval = -9999L,
    compression = 2,
    overwrite = TRUE,
  )
}

# write_fig({
#   plot(r[[1:16]])
# }, "a.pdf")
# write_fig({
#   plot(r2[[1:16]])
# }, "a2.pdf")
# image(arr[,,1] %>% t() %>% flipud())

# writeRaster(r, sprintf("soil_naqu_%sdeg_201008-201412_voronoi.tif", as.character(cellsize)), 
#             overwrite = TRUE)
# writeRaster(r, sprintf("soil_naqu_%sdeg_20100801H00-20161231H23_voronoi.tif", as.character(cellsize)), 
#             overwrite = TRUE)
# d_time %>% cbind(I = 1:nrow(.), .) %>% fwrite("bands_20100801H00-20161231H23.txt")
# p <- spplot(grid, 1:16, as.table = TRUE)
# write_fig(p, "soil_naqu_voronoi.pdf")
# mat = as.array(r)
