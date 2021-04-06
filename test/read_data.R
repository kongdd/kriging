library(foreach)
library(iterators)
library(plyr)
library(stringr)

source("test/main_pkgs.R")
dirs = dir("data-raw/DATA/SM", full.names = TRUE) %>% set_names(basename(.))

read_data <- function(file){
  fread(file)[, 1:5]
}

lst <- foreach(indir = dirs, i = icount()) %do% {
    runningId(i)
    files = dir(indir, full.names = TRUE)
  
    prefixs = basename(files) %>% gsub(".csv", "", .)
    files %<>% set_names(prefixs)
      
    depths = str_extract(prefixs, "\\d{1,2}(?=cm)")
    
    data = llply(files, read_data)
    df = melt_list(data, "site")
    df[year == "锘<bf>2010", year := 2010]
    df[, year := as.numeric(year)]
    df$depth = df$site %>% str_extract("\\d{1,2}(?=cm)") %>% as.numeric()
    df$site %<>% str_extract(".*(?=_\\d{1,2}cm)") 
    df %<>% unique()
    
    # lst <- split()
    # df2 = dcast(df, year+month+day+hour+depth~site)
    df$datetime = df[, make_datetime(year, month, day, hour)]
    df = df[, .(site, datetime, VMC, depth)]
    
    df[VMC == -99, VMC := NA]
    data = df[, .(VMC = mean(VMC, na.rm = TRUE)), .(site, datetime, depth)] %>% 
      dcast(datetime + depth ~ site)
}

save(INPUTS, file = "data-raw/raw_soil_Inputs.rda")
# 2010-08-01 to 2016-12-31

load("data-raw/raw_soil_Inputs.rda")
# INPUTS = lst
