rcumsum_weird <- function(x) {
    idx_na <- is.na(x)
    x[idx_na] <- 0
    x <- rev(cumsum(rev(x)))
    x
}

timing_sum <- function(df, timing_type) {
    tp <- names(timings[timings == timing_type])
    new_df <- df[rownames(df) %in% tp,]
    if(class(new_df) == "numeric") {
        net_type <- new_df
    } else if(class(new_df) == "matrix") {
        net_type <- colSums(new_df, na.rm = TRUE)
    } else {
        net_type <- rep(0, ncol(df))
    }
    return(net_type)
}

