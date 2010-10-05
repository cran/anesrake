wtd.partial.cor <- function(x, y=NULL, preds=NULL, weight=NULL, collapse=TRUE){
  x <- as.matrix(x)
  if(is.null(weight)){
    weight <- rep(1, dim(x)[1])
  }
  if(is.null(y)){
    y <- x
  }
  y <- as.matrix(y)
  est <- apply(x, 2, function(x) apply(y, 2, function(y) onecor.partial.wtd(x, y, preds, weight))[1,])
  se <- apply(x, 2, function(x) apply(y, 2, function(y) onecor.partial.wtd(x, y, preds, weight))[2,])
  tval <- apply(x, 2, function(x) apply(y, 2, function(y) onecor.partial.wtd(x, y, preds, weight))[3,])
  pval <- apply(x, 2, function(x) apply(y, 2, function(y) onecor.partial.wtd(x, y, preds, weight))[4,])
  out <- list(correlation=est, std.err=se, t.value=tval, p.value=pval)
  if(is.vector(est) & collapse==TRUE){
    out <- matrix(unlist(out), ncol=4, byrow=FALSE)
    rownames(out) <- names(est)
    colnames(out) <- c("correlation", "std.err", "t.value", "p.value")
  }
  out
}

