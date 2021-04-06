# Variogram varies across time, in the range of [0.003, 0.01]. 
# Hence kriging parameter should be fitted for every time.

#' fit kriging vgm model
#' 
#' @description 
#' Two simple vgm model were used:
#' (1) Exp
#' (2) Lin: if `Exp` model failed to converge, then `Lin` used.
#' 
#' @param s SpatialPointsDataFrame, which is used to be interpolated
#' @param range vgm parameter, distance range, in the unit of m
#' @param show boolean, whether plot output?
#' 
#' @export 
fit_kriging_vgm <- function(s, range = NULL, show) {
  formula <- as.formula(sprintf("%s ~ 1", names(s)))
  vgm     <- variogram(formula, s)  # calculates sample variogram values 
  
  # browser()
  if (is.null(range)) range = vgm$dist %>% max() %>% divide_by(2)
  # init vgm parameters
  nugget = vgm$gamma %>% head() %>% mean()
  psill  = vgm$gamma %>% {tail(.) - head(.)} %>% abs() %>% mean()
  str_model = "Exp"

  vgm.fit <- tryCatch({
    fit.variogram(vgm, model = vgm(psill, "Gau", range = range, nugget = nugget)) # fit model
  } , warning = function(w) {
    # warning means `Exp` fail to converge
    str_model <<- "Nug"
    fit.variogram(vgm, model = vgm(psill, "Lin", range = range, nugget = nugget)) # fit model
    # fit.variogram(vgm, model=vgm(A, "Lin", range = 0,  nugget = nugget/2)) # fit model
  })
  
  l = variogramLine(vgm.fit, max(vgm$dist))
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
    # print(p)
    ans$plot = as.grob(p)
  }
  return(ans)  
}

get_VGMdata <- function(s) {
  formula <- as.formula(sprintf("%s ~ 1", names(s)))
  vgm     <- variogram(formula, s)  # calculates sample variogram values 
  vgm
}
