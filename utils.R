rcumsum_weird <- function(x) {
    idx_na <- is.na(x)
    x[idx_na] <- 0
    x <- rev(cumsum(rev(x)))
    x
}
