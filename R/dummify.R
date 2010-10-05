dummify <- function(x){
  if(!is.factor(x)){
    stop("variable needs to be a factor")
  }
  levs <- levels(x)
  out <- model.matrix(~x-1)
  colnames(out) <- levs
  attributes(out)$assign <- NULL
  attributes(out)$contrasts <- NULL
  out
}
