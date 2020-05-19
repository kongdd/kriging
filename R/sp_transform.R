
# blockKrige需要在投影坐标系统下工作

# 从地理坐标系统WGS84，转为国家标准投影坐标系统CGCS2000。
# CGCS2000_3_Degree_GK_Zone_30，投影信息编号从ArcGIS中获取。

# proj_p.Info <- EPSG[which(EPSG$code == 4490), ] # display information

#' CGCS2000 Projected coordinate system
#' 
#' Projected coordinate system is defined on a flat, two-dimensional surface
#' 
#' @details 
#' `CGCS2000_3_Degree_GK_Zone_30`
#'  - `WKID`: 4518 Authority: EPSG
#' 
#'  - `Projection`: Gauss_Kruger
#'  - `False_Easting`: 30500000.0
#'  - `False_Northing`: 0.0
#'  - `Central_Meridian`: 90.0
#'  - `Scale_Factor`: 1.0
#'  - `Latitude_Of_Origin`: 0.0
#'  - `Linear Unit`: Meter (1.0)
#' 
#' @return 
#' CRS arguments:
#'  +init=epsg:4518 +proj=tmerc +lat_0=0 +lon_0=90 +k=1 +x_0=30500000 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#' 
#' @examples 
#' prj_p_CGCS2000
#' @import sp
#' @export
prj_p_CGCS2000 <- CRS("+init=epsg:4518")


#' CGCS2000 Geographic coordinate system
#' 
#' @details 
#' CGCS2000_3_Degree_GK_CM_90E
#'  - WKID: 4539 Authority: EPSG
#'  - `Projection`: Gauss_Kruger
#'  - `False_Easting`: 500000.0
#'  - `False_Northing`: 0.0
#'  - `Central_Meridian`: 90.0
#'  - `Scale_Factor`: 1.0
#'  - `Latitude_Of_Origin`: 0.0
#'  - `Linear Unit`: Meter (1.0)
#' 
#' @return 
#' CRS arguments:
#'  +init = epsg:4490 + proj = longlat + ellps = GRS80 + no_defs
#' @examples
#' prj_g_CGCS2000
#' @export
prj_g_CGCS2000 <- CRS("+init=epsg:4490")

#' Convert to CGCS2000 Projected coordinate system
#' 
#' @param x Spatial* object
#' @export 
to_CGCS2000 <- function(x) {
    spTransform(x, prj_p_CGCS2000)
}
