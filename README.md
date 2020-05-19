
# kriging

<!-- badges: start -->
<!-- badges: end -->

文件里有2个变量，d是土壤水分，属性依次是year， month, date, 71个站点的观测，里面有无效值NaN
gps是71个站点的经纬度

根据站点的范围，可以做4*4网格点的插值

```r
cellsize = 0.25
range = c(91.5, 92.5, 31, 32)
```
（一个网格0.25*0.25度，网格中心点91.625：0.25：92.375, 31.875：-0.25：31.125，网格边界就是91.5-92.5，32-31）

对于voronoi算法，我希望程序是输入台站位置，观测数据，以及网格的边界（网格大小是可以调的），输出是网格的插值结果，也就是加权平均的结果。

对于block kriging，应该是确定插值区域和分辨率，得到一个插值结果，分辨率可以调，目前需要2种，0.0625度和0.25度。

如有问题，及时与我沟通。谢谢!

这个应该不太难实现，matlab里面有voronoi的相关算法，但是它无法给边界的约束，我目前用的版本也没有多边形叠加的功能，所以我想用r实现。


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

如果您的网络不能访问github，请试试手机开热点。如果还不行，请联系我。
