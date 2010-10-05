onecor.partial.wtd <- function(x, y, preds=NULL, weight=NULL){
  if(is.null(weight)){
    weight <- rep(1, length(x))
  }
  if(!is.null(preds)){
    r1 <- lm(stdz(y, weight=weight)~stdz(x, weight=weight)+preds, weight=weight)
  }
  else{
    r1 <- lm(stdz(y, weight=weight)~stdz(x, weight=weight), weight=weight)
  }
  corcoef <- coef(summary(r1))[2,]
  corcoef
}
