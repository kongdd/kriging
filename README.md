
# kriging

<!-- badges: start -->
<!-- badges: end -->

<!-- 文件里有2个变量，d是土壤水分，属性依次是year， month, date, 71个站点的观测，里面有无效值NaN
gps是71个站点的经纬度 -->

```r
cellsize = 0.25
range = c(91.5, 92.5, 31, 32)
```

对于voronoi算法，我希望程序是输入台站位置，观测数据，以及网格的边界（网格大小是可以调的），输出是网格的插值结果，也就是加权平均的结果。
对于block kriging，应该是确定插值区域和分辨率，得到一个插值结果，分辨率可以调，目前需要2种，0.0625度和0.25度。


## 1. Installation

用到的R包大部分可以通过cran官网安装。以下R包需要采用devtools安装。
```r
library(sp2)
library(rcolors)
library(latticeGrob)
library(Ipaper)
```

安装方法
```r
devtools::install_github(kongdd/sp2)
devtools::install_github(kongdd/rcolors)
devtools::install_github(kongdd/latticeGrob)
devtools::install_github(kongdd/Ipaper)
```

## 2. Methods

- blockKrige

blockKrige使用的gstat完成，每个block再划分为四个网格，通过这四个网格krige插值得到该block的值。
block划分为几个网格，通过`sps.args`参数控制：

```r
ans <- predict(g, newdata = blocks, debug.level = 0, 
                   sps.args = list(n = 4, type = "regular", offset = c(.5, .5)))
```

注意：blockKrige的运行速度比krige至少慢十几倍。

以上参数官方说明文档并不详细，是通过阅读gstat的代码确定的。如果要细究blockKrige的公式，也需要去阅读gstat的源代码。

- 泰森多边形

较为简单，不再详细介绍


## 3. How to run this scripts?

(1) 双击`kriging.Rproj`打开项目
(2) 打开`test/02_kriging_interp.R`，运行程序
