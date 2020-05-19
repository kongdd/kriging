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
