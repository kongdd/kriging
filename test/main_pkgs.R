# source("main_pkgs.R")

library(R.matlab)
library(magrittr)
library(data.table)

# load spatial packages
library(gstat)
library(spatstat)
library(maptools)
library(raster)

library(grid)
library(gridExtra)
library(lattice)
library(ggplotify)
library(lubridate)

library(foreach)
library(matrixStats)
library(purrr)
shell <- Ipaper:::shell
# Rscript -e "library('devtools'); devtools::install('polyclip')"
library(sp2)
library(rcolors)
library(latticeGrob)
library(Ipaper)

apply_row <- function (mat, by, FUN = rowMeans2, ...) 
{
  if (length(by) != ncol(mat)) {
    stop("Length of by is not equal to ncol of mat")
  }
  grps <- unique(by) %>% sort()
  foreach(grp = grps, .combine = cbind) %do% {
    I <- which(by == grp)
    FUN(mat[, I, drop = FALSE], na.rm = TRUE, ...)
  } %>% set_colnames(grps) %>% set_rownames(rownames(mat))
}

# load data
if (!exists("loaded")) {
  l <- readMat("data-raw/naqu_5cm_sm.mat")
  st <- l$gps %>% data.table() %>% set_colnames(c("lon", "lat")) %>% cbind(I = 1:nrow(.), .)
  pos <- st[, paste0(lon, ",", lat)]
  info <- match2(pos, pos)
  
  data = l$d %>% data.table()
  d_date = data[, 1:3] %>% set_colnames(c("year", "month", "day")) 
  # fwrite(d_date, "soil_naqu_bandsInfo.csv")
  dates = d_date[, make_date(year, month, day)]
  dates_str = format(dates, "v%Y%m%d")
  data = data[, -(1:3)]
  
  data2 = apply_row(as.matrix(data), by = info$I_y)
  st2   = st[unique(info$I_y)]
  sp <- df2sp(st2)
  loaded = TRUE
}

# Variogram varies across time, in the range of [0.003, 0.01]. 
# Hence kriging parameter should be fitted for every time.

## Two simple vgm model were used.
# (1) Exp 
# (2) Lin: if `Exp` model failed to converge, then `Lin` used.

#' fit kriging vgm model
#' 
#' @param s SpatialPointsDataFrame, which is used to be interpolated
#' @param range vgm parameter, distance range, in the unit of km
#' @param show boolean, whether plot output?
#' 
#' @export 
fit_kriging_vgm <- function(s, range = 20, show) {
  formula <- as.formula(sprintf("%s ~ 1", names(s)))
  vgm     <- variogram(formula, s)  # calculates sample variogram values 
  
  # init vgm parameters
  nugget = vgm$gamma %>% head() %>% mean()
  psill  = vgm$gamma %>% {tail(.) - head(.)} %>% abs() %>% mean()
  str_model = "Exp"

  vgm.fit <- tryCatch({
    fit.variogram(vgm, model = vgm(psill, "Exp", range = range, nugget = nugget)) # fit model
  }, warning = function(w) {
    # warning means `Exp` fail to converge
    str_model <<- "Nug"
    fit.variogram(vgm, model = vgm(psill, "Lin", range = 0, nugget = nugget)) # fit model
    # fit.variogram(vgm, model=vgm(A, "Lin", range = 0,  nugget = nugget/2)) # fit model
  })
  l = variogramLine(vgm.fit, 40)

  ans <- list(vgm = vgm.fit)
  if (show) {
    p <- xyplot(gamma ~ dist, l,
      ylim = c(0, 1.1 * max(vgm$gamma)),
      xlim = c(0, max(l$dist)), type = "l",
      panel = function(x, y, type, ...) {
        panel.xyplot(x, y, type, ...)
        panel.xyplot(vgm$dist, vgm$gamma, "p")
        grid.text(str_model, 0.1, 0.95, just = c(0, 1))
        # panel.text(-Inf, Inf, "Nug")
      },
      xlab = NULL,
      ylab = NULL, main = dates_str[i]
    ) +
      theme_lattice(
        plot.margin = c(1, 1, 1, 1) * 1,
        axis.margin = c(0.5, 1.5, 1, 1.5) * 0.5
      )
    ans$plot = as.grob(p)
  }
  return(ans)  
}
