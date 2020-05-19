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

# Rscript -e "library('devtools'); devtools::install('polyclip')"
library(sp2)
library(rcolors)
library(latticeGrob)
library(Ipaper)
shell <- Ipaper:::shell

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
  
  devtools::load_all()
  data2 = apply_row(as.matrix(data), by = info$I_y)
  st2   = st[unique(info$I_y)]
  sp <- df2sp(st2)
  loaded = TRUE  
  # use_data(st2)
  # use_data(sp)
}
