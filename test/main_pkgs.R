# source("test/main_pkgs.R")

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
library(glue)

# Rscript -e "library('devtools'); devtools::install('polyclip')"
library(sf2)
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
  
  st = fread("data-raw/DATA/TP_station_locations.csv")
  sp <- df2sp(st)
  loaded = TRUE  
  # use_data(st2)
  # use_data(sp)
}


missinfo <- function(x) {
  
  missinfo_var <- function(x, datetime) {
    ind <- which.notna(x)
    first <- ind[1]
    last <- ind[length(ind)]

    data.table(begin = datetime[first], end = datetime[last])
  }

  x[, -(1:2)] %>%
    lapply(missinfo_var, x$datetime) %>%
    melt_list("site")
}

# Maqu: 33.5-34.125，101.625-102.6875
# Ali : 32.3125-33.5, 79.5-80.1875
# Naqu: 31-32, 91.5-92.5
cellsize = 0.25
cellsize = 1/16

ranges = list(
  Ali  = c(79.5, 80.1875, 32.3125, 33.5),
  Maqu = c(101.625, 102.6875, 33.5, 34.125),
  Naqu = c(91.5, 92.5, 31, 32)
)

load("data-raw/raw_soil_Inputs.rda")
names(INPUTS) <- names(ranges)
INPUTS$Ali %<>% .[, -5]# 

# grid <- get_grid(c(70, 140, 15, 55), cellsize = c(10, 20))
# polys <- as_SpatialPolygonsDataFrame(grid)
# write_shp(polys, "chinadem_30m_grid.shp")
sp <- df2sp(st)
