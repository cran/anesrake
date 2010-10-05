onecor.partial.wtd <- function(x, y, preds=NULL, weight=NULL){
  if(sum(!is.na(x))>0 & sum(!is.na(y))>0){
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
  }
  else
    corcoef <- rep(NA, 4)
  corcoef
}
