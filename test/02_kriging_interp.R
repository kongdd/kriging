source("test/main_pkgs.R")
library(iterators)

## Note: 
# - WGS84大地坐标系统下，不工作，需要转为投影坐标系统
# ------------------------------------------------------------------------------
# 1.2 SpatialPixels
# 1.1 spatialPoints
depths <- c(5, 10, 20, 40)[-c(1:2)]
depthi <- depths[2]

InitCluster(12, kill = TRUE)

depths = NA
# site   = "Ali"
# "Ali", 
for(site in c("Maqu")) {
  foreach(depthi = depths) %do% {
    # site = "Naqu"
    # site = "Ali"
    # site = "Maqu"
    range = ranges[[site]]
    grid <- get_grid(range, cellsize = cellsize, type = "vec")
    blocks <- as_SpatialPolygonsDataFrame(grid) %>% to_CGCS2000()
    df = INPUTS[[site]]
    if (!is.na(depthi)) {
      df = df[depth == depthi]
    } else {
      depthi = df$depth[1]
    }
    
    d_time = df[, 1]
    dates_str <- d_time$datetime %>% as.character()
    
    data = df[, -(1:2)] %>% as.matrix()
    sites = colnames(data) %>% gsub("^SM_|^ST_", "", .)
    ind_st = match(sites, st$station_no)
    
    st2 = st[ind_st, ] %>% cbind(I = 1:nrow(.), .)
    sp <- df2sp(st2) %>% to_CGCS2000()
    sp %<>% to_CGCS2000()
    sp@data <- as.data.table(t(data))
    
    inds_test = 1:ncol(sp) %>% 
      set_names(d_time$datetime[.] %>% as.character())
    
    lst <- foreach(i = inds_test, icount()) %do% {
      runningId(i, 1)
      # i = 151
      s = sp[, i]
      ind <- s@data[[1]] %>% which.notna() # rm NA vlaues
      if (length(ind) < 10) return(rep(NA_real_, length(blocks)))
      
      s = s[ind, ]
      formula <- as.formula(sprintf("%s ~ 1", names(s)))
      vgm <- fit_kriging_vgm(s, range = 30 * 1e3, show = FALSE)
      # vgm$plot
      g <- gstat(formula = formula, data = s, model = vgm$vgm)
      # predict.gstat.R L119 add browser
      ans <- predict(g,
                     newdata = blocks, debug.level = 0,
                     sps.args = list(n = 4, type = "regular", offset = c(.5, .5))
      )
      ans@data$var1.pred
    }
    # l <- lst %>% purrr::transpose() %>% map(~do.call(cbind, .) %>% data.table)
    df_pred = do.call(cbind, lst) %>%
      as.data.table()
    # p <- arrangeGrob(grobs = ps, nrow = 5)
    # write_fig(p, "kriging_vgm_model_parameters.pdf", 15, 10)
    # write_fig(vgm$plot, "a.pdf")
    grid2 <- grid
    grid2@data <- df_pred
    r <- brick(grid2)
    
    ## write output
    str_depth = sprintf("%02dcm", depthi)
    date_start = d_time$datetime %>%
      first() %>%
      format("%Y%m%dH%H")
    date_end = d_time$datetime %>%
      last() %>%
      format("%Y%m%dH%H")
    outfile = glue("soil_{site}-{str_depth}_0.0625deg_{date_start}-{date_end}_blockKriging.nc")
    writeRaster(r, outfile)
  }
}

# fit.variogram(vgm, model = vgm(psill, "Gau", range = range, nugget = nugget)) 
# # df      <- 
# # datetime <- data
# # spplot(sp, 1:16, as.table = TRUE)
# 
# inds      = 1:ncol(sp)
# inds_test = seq(1, ncol(sp), 30)[1:25]
# 
# ## 2.1 Select kriging model and parameter  -------------------------------------
# # write_fig(
# {
#   ps <- foreach(i = inds_test) %do% {
#     # for (i in ) {
#     # i = 91
#     s = sp[, i]
#     ind <- s@data[[1]] %>% which.notna() # rm NA vlaues
#     s = s[ind, ]
#     formula <- as.formula(sprintf("%s ~ 1", names(s)))
#     vgm     <- fit_kriging_vgm(s, range = NULL, show = TRUE)
#     vgm$plot
#     # grid2 <- krige(formula, s, grid, model = vgm$vgm)
#     # grid2@data
#   }
#   p <- arrangeGrob(grobs = ps, nrow = 5)
#   write_fig(p, "kriging_vgm_model_parameters.pdf", 15, 10)
# }
# 
# 
# ## original kriging 
# # {
# #   temp <- foreach(i = inds_test) %do% {
# #     # for (i in ) {
# #     # i = 91
# #     ind <- sp[[i]] %>% which.notna() # rm NA vlaues
# #     formula <- as.formula(sprintf("%s ~ 1", names(sp[, i])))
# #     # variogram(formula,  sp[ind, i])
# #     grid2 <- krige(formula, sp[ind, i], grid, model = vgm$vgm)
# #     grid2@data
# #   } 
# #   temp2 = temp %>% do.call(rbind, .) %>% data.table()
# # }
# 
# ## 2.2 krige spatial interp ----------------------------------------------------
# # write_fig(
# {
#   inds      = 1:ncol(sp)
#   inds_test = seq(1, ncol(sp), 30)[1:25]
#   inds_test = inds
#   
#   
# }
# 
# 
# writeRaster(r, sprintf("soil_naqu_%sdeg_201008-201412_blockKrige.tif", as.character(cellsize)), 
#             overwrite = TRUE)
# 
# {
#   brks = seq(0, 0.5, 0.01) %>% c(-Inf, ., Inf)
#   ncol = length(brks) - 1
#   cols <- get_color("amwg256", ncol) %>% rev()
#   sizes <- seq(0.5, 1.5, length.out = ncol)
#   
#   p2 <- spplot(grid2, inds_test, at = brks, col.regions = cols, 
#                panel = function(x, y, z, subs, ...) {
#                  panel.levelplot(x,y,z, subs, ...)
#                  i = inds[panel.number()]
#                  val <- data2[i, ]
#                  ind_nona <- which.notna(val)
#                  val <- cut(val[ind_nona], brks)
#                  # browser()
#                  panel.xyplot(st2$lon[ind_nona], st2$lat[ind_nona], pch = 21, fill = cols[val], cex = sizes[val], col = "black")
#                },
#                # sp.layout = sp_layout, 
#                as.table = TRUE)
#   p2
#   write_fig(p2, "blockKrige_outputs_example.pdf", 10, 7)
# }
# 
# # }, "kriging_vgm_model_parameters.pdf", 10, 10)
# ## 01. test kriging model and parameter optim ----------------------------------
