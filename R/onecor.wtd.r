onecor.wtd <- function(x, y, weight=NULL){
  if(is.null(weight)){
    weight <- rep(1, length(x))
  }
  r1 <- lm(stdz(y, weight=weight)~stdz(x, weight=weight), weight=weight)
  corcoef <- coef(summary(r1))[2,]
  corcoef
}
