dummify <- function(x){
  if(!is.factor(x)){
    stop("variable needs to be a factor")
  }
  levels(x) <- gsub(" ", "_", levels(x))
  eval(parse(text=paste(levels(x)[1], "<- as.numeric(x==levels(x)[1])")))
  combine <- paste("cbind(", levels(x)[1])
  for(i in levels(x)[2:length(levels(x))]){
    eval(parse(text=paste(i, "<- as.numeric(x==i)")))
    combine <- paste(combine, i, sep=", ")
  }
  out <- eval(parse(text=paste(combine,")", sep="")))
  colnames(out) <- levels(x)
  out
}
